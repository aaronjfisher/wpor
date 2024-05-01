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



#' Fit workflow or object on resampled folds
#' @param object must have a fit method, which returns an object with a
#' predict method; which returns a vector of expected values for the predicted outcome.
#' If the object's predict method returns output in a different format
#' (e.g., a fitted parsnip model), then the user must define a
#' `predict_expected_value` method.
#' @param rset e.g., the output of vfold_cv
#' @param do_1 whether to return an additional column of predictions
#' for the assessment fold, in which treatment is set to 1.
#' @param do_0 whether to return an additional column of predictions
#' for the assessment fold, in which treatment is set to 0.
#' @param analysis_rows a subset of .row values to use for training.
#' This is helpful, for example,
#' for fitting a model only on treated patients, but obtaining
#' predictions on all patients.
#' @param ... see `filter_analysis`.
#'
#' This function is based on tune::fit_resamples, but it is not a generic.
#' Thus, for custom fitting procedures that are not workflows,
#' the user still needs to ensure that a fit method exists,
#' but does not need to ensure that a fit_resamples method exists.
#' This version relies only on the `fit` method of the workflow.
fit_on_folds <- function(
    object, rset,
    do_1 = FALSE, do_0 = FALSE,
    analysis_rows = NULL) {
  assert_shared_data_address(rset)
  if (is.null(analysis_rows)) {
    full_data <- rset$splits[[1]]$data
    analysis_rows <- unique(full_data$.row)
    stopifnot(length(analysis_rows) == nrow(full_data))
  }
  rset %>%
    mutate(predictions = lapply(
      rset$splits, \(s){
        mod <- analysis(s) %>%
          dplyr::filter(.row %in% analysis_rows) %>%
          fit(object, .)

        pred_obs <- predict_expected_value(mod, new_data = assessment(s))
        if (do_0) {
          assess_0 <- assessment(s)
          assess_0$treatment[] <- 0
          pred_0 <-
            predict_expected_value(mod, new_data = assess_0)
        } else {
          pred_0 <- NULL
        }
        if (do_1) {
          assess_1 <- assessment(s)
          assess_1$treatment[] <- 1
          pred_1 <-
            predict_expected_value(mod, new_data = assess_1)
        } else {
          pred_1 <- NULL
        }

        tibble::tibble(
          .row = assessment(s)$.row,
          .pred = pred_obs,
          .pred_0 = pred_0,
          .pred_1 = pred_1
        )
      }
    )) %>%
    select(-splits) %>%
    rename(.fold_id = id) %>%
    tidyr::unnest(predictions) %>%
    arrange(.row, .fold_id) %>%
    relocate(.row, .before = everything())
}

check_dat <- function(data) {
  stopifnot(!any(c(".weights", "pseudo", ".row") %in% names(data)))
  data <- mutate(data, .row = 1:nrow(data)) %>%
    relocate(.row, .before = everything())
  data
}

check_wf <- function(
    wf, data,
    lhs_is = NULL,
    rhs_lacks = NULL,
    rhs_has = NULL) {
  get_y_name <- paste0("get_y.", class(wf)[1])
  if (exists(get_y_name)) {
    if (any(get_y(wf, data) != data[[lhs_is]])) {
      stop("Incorrect workflow specification. Left-hand side should be: ", lhs_is)
    }
  }

  if ("workflow" %in% class(wf)) {
    rhs <- all.vars(wf$pre$actions$formula$formula)[-1]
    rhs_lacks <- unique(rhs_lacks, lhs_is) # avoid circular formulas
    if (any(rhs_lacks %in% rhs)) {
      stop("Incorrect workflow specification. Right-hand side omit: ", paste(rhs_lacks, collapse = ", "))
    }
    if (any(!rhs_has %in% rhs)) {
      stop("Incorrect workflow specification. Right-hand side must have: ", paste(rhs_has, collapse = ", "))
    }
  }
}

#' Apply weighted pseudo-outcome regression
#' @param data a data.frame or tibble containing columns `treatment` and `outcome`.
#' @param outcome_1_separate_wf,outcome_0_separate_wf,outcome_single_wf,outcome_marginal_wf,treatment_wf tidymodel workflows
#' for each modeling step. These should have formulas
#'  with `outcome` or `treatment` on the left-hand side.
#' The formula for `outcome_single_wf` should include
#' `treatment` on the right-hand side, but no other formulas
#' should include treatment as a covariate.
#' @param min_prob a truncation probability for the propensity scores.
#' @param v number of folds, passed to vfold_cv
#' @param cf_order either 2, 3, or 4.
#' @param verbose whether to print progress.
#' @returns a tibble of nuisance predictions. If cf_order is >2, there will be more than 1 prediction per row of `data`. The tibble contains: `.row`, the row index of `data`; `.fold_id`, the fold used to train the predictions; `.pred_treatment`, the predicted probability of treatment; `.pred_control`, the predicted probability of control (equal to 1-.pred_treatment if cf_order <= 3); `.pred_outcome_0_separate` the predicted outcome under treatment == 0, `.pred_outcome_1_separate` the predicted outcome under treatment == 1; `.pred_outcome_marginal` the predicted outcome under the observed treatment, i.e., marginalizing over treatment.
#' @export
crossfit_nuisance <- function(
    data,
    treatment_wf,
    outcome_marginal_wf = NA,
    outcome_single_wf = NA,
    outcome_1_separate_wf = NA,
    outcome_0_separate_wf = NA,
    min_prob = 0.01,
    v = 10,
    cf_order = 2,
    verbose = getOption("verbose")) {
  stopifnot(is.factor(data$treatment))
  stopifnot(all(data$treatment %in% 0:1))

  data <- check_dat(data)

  check_wf(outcome_marginal_wf, data, lhs_is = "outcome", rhs_lacks = "treatment")
  check_wf(outcome_single_wf, data, lhs_is = "outcome", rhs_has = "treatment")
  check_wf(outcome_0_separate_wf, data, lhs_is = "outcome", rhs_lacks = "treatment")
  check_wf(outcome_1_separate_wf, data, lhs_is = "outcome", rhs_lacks = "treatment")
  check_wf(treatment_wf, data, lhs_is = "treatment", rhs_lacks = "outcome")

  if (
    all(is.na(outcome_marginal_wf)) &
      all(is.na(outcome_single_wf)) &
      (all(is.na(outcome_1_separate_wf)) |
        all(is.na(outcome_0_separate_wf)))
  ) {
    stop("Users must specify at least one of the following:
      1) outcome_marginal_wf,
      2) outcome_single_wf, or
      3) both outcome_1_separate_wf & outcome_0_separate_wf.")
  }

  if (all(is.na(outcome_marginal_wf))) outcome_marginal_wf <- get_na_workflow()
  if (all(is.na(outcome_single_wf))) outcome_single_wf <- get_na_workflow()
  if (all(is.na(outcome_1_separate_wf))) outcome_1_separate_wf <- get_na_workflow()
  if (all(is.na(outcome_0_separate_wf))) outcome_0_separate_wf <- get_na_workflow()


  if (cf_order == 2) {
    folds <- vfold_cv(data, v) # contains full data
  }
  if (cf_order %in% c(3, 4)) {
    folds <- multiway_cf_folds(data, cf_order)
  }

  # lobstr::obj_size(folds_train_1)
  # lobstr::obj_size(folds)
  # lobstr::obj_size(data)
  if (verbose) message("Cross fitting treatment model")
  E_treatment_tbl <- fit_on_folds(treatment_wf, folds) %>%
    mutate(.pred_treatment = ptrim(.pred, min_prob, 1 - min_prob)) %>%
    select(.row, .fold_id, .pred_treatment)

  if (verbose) message("Cross fitting outcome model")
  E_outcome_marginal <- fit_on_folds(outcome_marginal_wf, folds) %>%
    rename(.pred_outcome_marginal = .pred) %>%
    select(.row, .fold_id, .pred_outcome_marginal)
  E_outcome_1_separate_tbl <- fit_on_folds(outcome_1_separate_wf,
    folds,
    analysis_rows = filter(data, treatment == 1)$.row
  ) %>%
    rename(.pred_outcome_1_separate = .pred) %>%
    select(.row, .fold_id, .pred_outcome_1_separate)
  E_outcome_0_separate_tbl <- fit_on_folds(outcome_0_separate_wf,
    folds,
    analysis_rows = filter(data, treatment == 0)$.row
  ) %>%
    rename(.pred_outcome_0_separate = .pred) %>%
    select(.row, .fold_id, .pred_outcome_0_separate)

  E_outcome_single_tbl <- fit_on_folds(
    outcome_single_wf,
    folds,
    do_1 = TRUE, do_0 = TRUE
  ) %>%
    rename(
      .pred_outcome_0_single = .pred_0,
      .pred_outcome_1_single = .pred_1
    ) %>%
    select(
      .row, .fold_id,
      .pred_outcome_0_single,
      .pred_outcome_1_single
    )

  if (verbose) message("Reshaping nuisance predictions")
  pred_df <- merge(E_outcome_0_separate_tbl, E_outcome_1_separate_tbl) %>%
    merge(E_outcome_marginal) %>%
    merge(E_treatment_tbl) %>%
    merge(E_outcome_single_tbl) %>%
    tibble() %>%
    arrange(.row, .fold_id) %>%
    nest(.by = .row) %>%
    rename(.pred = data)

  # For any given training observation, we will have
  # (cf_order-1) nuisance model predictions, each from
  # a separate fold.
  # pred_df$.pred[[i]] contains a tibble of these nuisance predictions

  # pred_df2, defined below, will have a column .pred that contains all
  # (cf_order - 1)-way combinations of nuisance estimates.
  # These combinations will be labeled in `.fold_id` according
  # to the folds of pred_df that were combined to produce them.

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
        .fold_id = gsub("Fold", "", paste(p$.fold_id[inds], collapse = "x")),
        .pred_treatment = p$.pred_treatment[inds[1]],
        .pred_control = 1 - p$.pred_treatment[inds[1]],
        .pred_outcome_0_separate = p$.pred_outcome_0_separate[inds[2]],
        .pred_outcome_1_separate = p$.pred_outcome_1_separate[inds[2]],
        .pred_outcome_0_single = p$.pred_outcome_0_single[inds[2]],
        .pred_outcome_1_single = p$.pred_outcome_1_single[inds[2]],
        .pred_outcome_marginal = p$.pred_outcome_marginal[inds[2]]
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
        .fold_id = gsub("Fold", "", paste(p$.fold_id[inds], collapse = "x")),
        .pred_treatment = p$.pred_treatment[inds[1]],
        .pred_control = 1 - p$.pred_treatment[inds[2]],
        .pred_outcome_0_separate = p$.pred_outcome_0_separate[inds[3]],
        .pred_outcome_1_separate = p$.pred_outcome_1_separate[inds[3]],
        .pred_outcome_0_single = p$.pred_outcome_0_single[inds[3]],
        .pred_outcome_1_single = p$.pred_outcome_1_single[inds[3]],
        .pred_outcome_marginal = p$.pred_outcome_marginal[inds[3]]
      )) %>%
        do.call(rbind, .)
    })
  }

  nuisance_tbl <- data %>%
    select(.row, outcome, treatment) %>%
    left_join(pred_df2, by = join_by(.row)) %>%
    tidyr::unnest(.pred)

  nuisance_tbl
}


#' Fit a weighted pseudo-outcome regression
#' @param nuisance_tbl the output of crossfit_nuisance
#' @param ... passed to crossfit_nuisance, if nuisance_tbl is not
#' supplied.
#' @param pseudo_fun the pseudo-outcome function. See pseudo_fun.R
#' @param weight_fun the weighting function. See weight_fun.R#'
#' @param effect_wf A workflow for fitting the final effect (pseudo-outcome) model.
#' This workflow should expect the pseudo-outcome to be stored in
#' a data column labeled "pseudo".
#' @returns a fitted model; the output of `workflows:::fit.workflow`
#' @rdname crossfit_nuisance
#' @export
fit_wpor <- function(data,
                     nuisance_tbl = NULL,
                     cf_order = 2,
                     ...,
                     pseudo_fun, weight_fun, effect_wf,
                     standardize_weights = FALSE,
                     verbose = TRUE) {
  data <- check_dat(data)
  if (is.null(nuisance_tbl)) {
    nuisance_tbl <- crossfit_nuisance(
      data = data,
      cf_order = cf_order,
      verbose = verbose,
      ...
    )
  }

  cf_order_nuisance <- length(strsplit(nuisance_tbl$.fold_id[1], "x")[[1]]) + 1
  if (cf_order != cf_order_nuisance) {
    stop("cf_order does not match nuisance_tbl")
  }

  if ("tunefit" %in% class(effect_wf) & cf_order > 2) {
    if (is.null(effect_wf$tune_args$group)) {
      warning('Setting effect_wf$tune_args$group <- ".row"')
      effect_wf$tune_args$group <- ".row"
    } else {
      if (effect_wf$tune_args$group != ".row") stop('effect_wf$tune_args$group should be ".row" to avoid having the same individual in multiple splits.')
    }
  }

  needed_cols <- unique(formalArgs(pseudo_fun), formalArgs(weight_fun))
  missing_cols <- lapply(nuisance_tbl, \(z) all(is.na(z))) %>%
    unlist() %>%
    which() %>%
    names() %>%
    intersect(needed_cols)
  if (length(missing_cols) > 0) {
    stop(
      "Columns ", paste(missing_cols, collapse = ", "),
      " were not found in nuisance table. Please ensure that
      the appropriate workflows were supplied."
    )
  }

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
  stopifnot(all(!is.na(nuisance_tbl$pseudo)))
  stopifnot(all(!is.na(nuisance_tbl$.weights)))

  if (verbose) message("Fitting effect model")
  dat_effect <- left_join(
    nuisance_tbl[, c(".row", "pseudo", ".weights")],
    data,
    by = join_by(.row)
  ) %>%
    select(-outcome, -treatment)
  check_wf(effect_wf, dat_effect, lhs_is = "pseudo", rhs_lacks = c("outcome", "treatment"))
  
  fitted <- effect_wf %>%
    add_weights_column(".weights") %>%
    fit(dat_effect)

  # standardize class of output so that predictions are always in same format??
  fitted
}

#' Higher order cross-fitting instructions with inverted train/test proportions.
#' @export
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
