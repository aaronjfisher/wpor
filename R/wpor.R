#' Cross-fit a workflow; a wrapper for `fit_resamples`
#' @param wf a tidymodels workflow.
#' @param rset a collection (i.e., "set") of resamples.
#' E.g., for propenisty models, the output of vfold_cv
crossfit <- function(wf, rset) {
  wf %>%
    tune::fit_resamples(
      rset,
      control = control_resamples(save_pred = TRUE)
    ) %>%
    collect_predictions(summarize = FALSE) %>%
    arrange(.row) %>%
    rename(.fold_id = id)
}

#' A wrapper for predict that changes handling of default types for workflow
#'
#' The default `type` for prediction for mode = classification
#' in parsnip is to output predicted classes.
#' This changes that default to 'prob', which
#' produces a tibble of probabilities for each class.
#'
#' For regression modes in tidymodels, or for user defined
#' model object classes, `predict_prob` defaults to `predict`.
predict_prob <- function(object, ...) {
  mode <- "custom"
  if ("workflow" %in% class(object)) {
    mode <- pull_workflow_spec(object)$mode
  }
  if ("model_spec" %in% class(object)) {
    mode <- object$mode
  }

  if (mode == "classification") {
    return(predict(object, type = "prob", ...))
  } else {
    return(predict(object, ...))
  }
}


#' Fit workflow or object on resampled folds
#' @param object must have a fit method, which returns an object with a
#' predict method; which returns a tibble with .pred or .pred_1 and .pred_0
#' @param rset e.g., the output of vfold_cv
#' This function is based on tune::fit_resamples, but it is not a generic.
#' Thus, for custom fitting procedures that are not workflows,
#' the user still needs to ensure that a fit method exists,
#' but does not need to ensure that a fit_resamples method exists.
#' This version relies only on the `fit` method of the workflow.
fit_on_folds <- function(object, rset) {
  assert_shared_data_address(rset)
  rset %>%
    mutate(predictions = lapply(
      rset$splits, \(s){
        mod <- fit(object, analysis(s))
        pred <- predict_prob(mod, new_data = assessment(s))
        ## validate_pred(pred)
        pred$.row <- assessment(s)$.row
        pred
      }
    )) %>%
    select(-splits) %>%
    rename(.fold_id = id) %>%
    unnest(predictions) %>%
    arrange(.row, .fold_id) %>%
    relocate(.row, .before = everything())
}

check_dat <- function(data) {
  stopifnot(!any(c(".weights", "pseudo", ".row") %in% names(data)))
  data <- mutate(data, .row = 1:nrow(data)) %>%
    relocate(.row, .before = everything())
  data
}

#' Apply weighted pseudo-outcome regression
#' @param data a data.frame or tibble containing columns `treatment` and `outcome`.
#' @param outcome_wf,treatment_wf tidmodel workflows for each modeling step. These should have formulas with
#' `outcome` and `treatment` on the left-hand side, respectively.
#' @param min_prob a truncation probability for the propensity scores.
#' @param v number of folds, passed to vfold_cv
#' @param cf_order either 2, 3, or 4.
#' @param verbose whether to print progress.
#' @returns a tibble of nuisance predictions. If cf_order is >2, there will be more than 1 prediction per row of `data`. The tibble contains: `.row`, the row index of `data`; `.fold_id`, the fold used to train the predictions; `.pred_treatment`, the predicted probability of treatment; `.pred_control`, the predicted probability of control (equal to 1-.pred_treatment if cf_order <= 3); `.pred_outcome_0` the predicted outcome under treatment == 0, `.pred_outcome_1` the predicted outcome under treatment == 1; `.pred_outcome_obs` the predicted outcome under the observed treatment, i.e., marginalizing over treatment.
#' @export
crossfit_nuisance <- function(data,
                              outcome_wf, treatment_wf,
                              min_prob = 0.01,
                              v = 10,
                              cf_order = 2,
                              verbose = TRUE) {
  stopifnot(is.factor(data$treatment))
  stopifnot(all(data$treatment %in% 0:1))
  data <- check_dat(data)

  if (cf_order == 2) {
    folds_all <- vfold_cv(data, v) # contains full data
  }
  if (cf_order %in% c(3, 4)) {
    folds_all <- multiway_cf_folds(data, cf_order)
  }
  folds_train_1 <- filter_analysis(folds_all, treatment == 1)
  folds_train_0 <- filter_analysis(folds_all, treatment == 0)

  # lobstr::obj_size(folds_train_1)
  # lobstr::obj_size(folds_all)
  # lobstr::obj_size(data)

  if (verbose) message("Cross fitting treatment model")
  E_treatment_tbl <- fit_on_folds(treatment_wf, folds_all) %>%
    mutate(.pred_treatment = ptrim(.pred_1, min_prob, 1 - min_prob)) %>%
    select(.row, .fold_id, .pred_treatment)

  if (verbose) message("Cross fitting outcome model")
  E_outcome_1_tbl <- fit_on_folds(outcome_wf, folds_train_1) %>%
    rename(.pred_outcome_1 = .pred) %>%
    select(.row, .fold_id, .pred_outcome_1)
  E_outcome_0_tbl <- fit_on_folds(outcome_wf, folds_train_0) %>%
    rename(.pred_outcome_0 = .pred) %>%
    select(.row, .fold_id, .pred_outcome_0)
  E_outcome_obs <- fit_on_folds(outcome_wf, folds_all) %>%
    rename(.pred_outcome_obs = .pred) %>%
    select(.row, .fold_id, .pred_outcome_obs)
  ## !! decide whether to compute this adaptively??

  if (verbose) message("Reshaping nuisance predictions")
  pred_df <- merge(E_outcome_0_tbl, E_outcome_1_tbl) %>%
    merge(E_outcome_obs) %>%
    merge(E_treatment_tbl) %>%
    tibble() %>%
    arrange(.row, .fold_id) %>%
    nest(.by = .row) %>%
    rename(.pred = data)

  if (cf_order == 2) {
    pred_df2 <- pred_df
    pred_df2$.pred <- lapply(pred_df$.pred, \(p){
      stopifnot(length(p$.fold_id) == 1)
      mutate(p, .pred_control = 1 - .pred_treatment)
    })
  }
  if (cf_order == 3) {
    pred_df2 <- pred_df
    pred_df2$.pred <- lapply(pred_df$.pred, \(p){
      stopifnot(length(p$.fold_id) == 2)
      eg <- expand.grid(1:2, 1:2) %>%
        filter(Var1 != Var2)
      apply(eg, 1, \(inds)
      data.frame(
        .fold_id = paste(inds, collapse = "x"),
        .pred_treatment = p$.pred_treatment[inds[1]],
        .pred_control = 1 - p$.pred_treatment[inds[1]],
        .pred_outcome_0 = p$.pred_outcome_0[inds[2]],
        .pred_outcome_1 = p$.pred_outcome_1[inds[2]],
        .pred_outcome_obs = p$.pred_outcome_obs[inds[2]]
      )) %>%
        do.call(rbind, .)
    })
  }
  if (cf_order == 4) {
    pred_df2 <- pred_df
    pred_df2$.pred <- lapply(pred_df$.pred, \(p){
      stopifnot(length(p$.fold_id) == 3)
      eg <- expand.grid(1:3, 1:3, 1:3) %>%
        filter(Var1 != Var2, Var1 != Var3, Var2 != Var3)
      apply(eg, 1, \(inds)
      data.frame(
        .fold_id = paste(inds, collapse = "x"),
        .pred_treatment = p$.pred_treatment[inds[1]],
        .pred_control = 1 - p$.pred_treatment[inds[2]],
        .pred_outcome_0 = p$.pred_outcome_0[inds[3]],
        .pred_outcome_1 = p$.pred_outcome_1[inds[3]],
        .pred_outcome_obs = p$.pred_outcome_obs[inds[3]]
      )) %>%
        do.call(rbind, .)
    })
  }

  nuisance_tbl <- data %>%
    select(.row, outcome, treatment) %>%
    left_join(pred_df2, by = join_by(.row)) %>%
    unnest(.pred)

  nuisance_tbl
}

#' Fit a weighted pseudo-outcome regression
#' @param nuisance_tbl the output of crossfit_nuisance
#' @param ... passed to crossfit_nuisance, if nuisance_tbl is not
#' supplied.
#' @param pseudo_fun the pseudo-outcome function. See pseudo_fun.R
#' @param weight_fun the weighting function. See weight_fun.R#'
#' @param effect_wf A workflow for fitting the final effect (pseudo-outcome) model.
#' @returns a fitted model; the output of `workflows:::fit.workflow`
#' @rdname crossfit_nuisance
#' @export
fit_wpor <- function(data,
                     nuisance_tbl = NULL,
                     ...,
                     pseudo_fun, weight_fun, effect_wf,
                     standardize_weights = FALSE,
                     verbose = TRUE) {
  if (is.null(nuisance_tbl)) {
    nuisance_tbl <- crossfit_nuisance(
      data = data,
      verbose = verbose,
      ...
    )
  }
  data <- check_dat(data)

  stopifnot(!any(c(".weights", "pseudo") %in% names(nuisance_tbl)))
  stopifnot(all(c(".row") %in% names(nuisance_tbl)))

  nuisance_tbl$pseudo <- nuisance_tbl %>%
    select(formalArgs(pseudo_fun)) %>%
    do.call(pseudo_fun, .)
  wts <- nuisance_tbl %>%
    select(formalArgs(weight_fun)) %>%
    do.call(weight_fun, .)
  if (standardize_weights) wts <- wts / mean(wts)
  nuisance_tbl$.weights <- hardhat::importance_weights(wts)
  if (verbose) message("Fitting effect model")
  dat_effect <- left_join(
    nuisance_tbl[, c(".row", "pseudo", ".weights")],
    data,
    by = join_by(.row)
  ) %>%
    select(-outcome, -treatment)


  if ("workflow" %in% class(effect_wf)) {
    fitted <- effect_wf %>%
      workflows::add_case_weights(.weights) %>%
      fit(dat_effect)
  } else {
    wts_merged <- as.numeric(dat_effect$.weights)
    fitted <- select(dat_effect, -.weights) %>%
      fit(effect_wf, data = ., weights = wts_merged)
  }

  fitted
}

multiway_cf_folds <- function(data, v) {
  new_splits <- list()
  shuffled <- sample(nrow(data), replace = FALSE)
  cut_ind <- as.numeric(cut(1:nrow(data), breaks = v))
  analysis_list <- lapply(1:v, \(ind) shuffled[cut_ind == ind])
  for (i in 1:v) {
    new_splits[[i]] <- make_splits(
      x = list(
        analysis = analysis_list[[i]],
        assessment = setdiff(1:nrow(data), analysis_list[[i]])
      ),
      data = data
    )
  }
  out <- manual_rset(new_splits, paste0("Fold", 1:v))
  assert_shared_data_address(out)
  out
}

assert_shared_data_address <- function(rset) {
  addresses <- rep(NA, nrow(rset))
  for (i in 1:length(rset$id)) {
    dati <- rset$splits[[i]]$data
    addresses[i] <- pryr::address(dati)
    if (i == 1) stopifnot(all(dati$.row == 1:nrow(dati)))
    # use rownames isntead of .row?
    # No, rsample uses integers for in_id regardless of the rownames
  }
  stopifnot(all(addresses == addresses[1]))
}

filter_analysis <- function(rset, ...) {
  assert_shared_data_address(rset)
  new_splits <- list()
  for (i in 1:length(rset$id)) {
    split_i <- rset$splits[[i]]

    keep_id <- dplyr::filter(analysis(split_i), ...)$.row
    new_splits[[i]] <- make_splits(
      x = list(
        analysis = keep_id,
        assessment = assessment(split_i)$.row
      ),
      data = split_i$data
    )
  }
  out <- manual_rset(new_splits, rset$id)
  assert_shared_data_address(out)
  out
}
