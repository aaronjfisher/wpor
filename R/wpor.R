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

check_dat <- function(dat) {
  stopifnot(!any(c(".weights", "pseudo", ".row") %in% names(dat)))
  dat <- mutate(dat, .row = 1:nrow(dat)) %>%
    relocate(.row, .before = everything())
  dat
}

#' Apply weighted pseudo-outcome regression
#' @param dat a data.frame or tibble containing columns `treatment` and `outcome`.
#' @param outcome_wf,treatment_wf tidmodel workflows for each modeling step. These should have formulas with
#' `outcome` and `treatment` on the left-hand side, respectively.
#' @param min_prob a truncation probability for the propensity scores.
#' @param v number of folds, passed to vfold_cv
#' @param cf_order either 2, 3, or 4.
#' @param verbose whether to print progress.
#' @returns a tibble of nuisance predictions. If cf_order is >2, there will be more than 1 prediction per row of `dat`. The tibble contains: `.row`, the row index of `dat`; `.fold_id`, the fold used to train the predictions; `.pred_treatment`, the predicted probability of treatment; `.pred_control`, the predicted probability of control (equal to 1-.pred_treatment if cf_order <= 3); `.pred_outcome_0` the predicted outcome under treatment == 0, `.pred_outcome_1` the predicted outcome under treatment == 1; `.pred_outcome_obs` the predicted outcome under the observed treatment, i.e., marginalizing over treatment.
#' @export
crossfit_nuisance <- function(dat,
                              outcome_wf, treatment_wf,
                              min_prob = 0.01,
                              v,
                              cf_order = 2,
                              verbose = TRUE) {
  stopifnot(is.factor(dat$treatment))
  stopifnot(all(dat$treatment %in% 0:1))
  dat <- check_dat(dat)

  if (cf_order == 2) {
    folds_all <- vfold_cv(dat, v) # contains full data
  }
  if (cf_order %in% c(3, 4)) {
    folds_all <- multiway_cf_folds(dat, cf_order)
  }
  folds_train_1 <- filter_training(folds_all, "treatment == 1")
  folds_train_0 <- filter_training(folds_all, "treatment == 0")

  if (verbose) message("Cross fitting treatment model")
  E_treatment_tbl <- crossfit(treatment_wf, folds_all) %>%
    mutate(.pred_treatment = ptrim(.pred_1, min_prob, 1 - min_prob)) %>%
    select(.row, .fold_id, .pred_treatment)

  if (verbose) message("Cross fitting outcome model")
  E_outcome_1_tbl <- crossfit(outcome_wf, folds_train_1) %>%
    rename(.pred_outcome_1 = .pred) %>%
    select(.row, .fold_id, .pred_outcome_1)
  E_outcome_0_tbl <- crossfit(outcome_wf, folds_train_0) %>%
    rename(.pred_outcome_0 = .pred) %>%
    select(.row, .fold_id, .pred_outcome_0)
  E_outcome_obs <- crossfit(outcome_wf, folds_all) %>%
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

  nuisance_tbl <- dat %>%
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
fit_wpor <- function(dat,
                     nuisance_tbl = NULL,
                     ...,
                     pseudo_fun, weight_fun, effect_wf,
                     verbose = TRUE) {
  if (is.null(nuisance_tbl)) {
    nuisance_tbl <- crossfit_nuisance(
      dat = dat,
      verbose = verbose,
      ...
    )
  }
  dat <- check_dat(dat)

  stopifnot(!any(c(".weights", "pseudo") %in% names(nuisance_tbl)))
  stopifnot(all(c(".row") %in% names(nuisance_tbl)))

  nuisance_tbl$pseudo <- nuisance_tbl %>%
    select(formalArgs(pseudo_fun)) %>%
    do.call(pseudo_fun, .)
  wts <- nuisance_tbl %>%
    select(formalArgs(weight_fun)) %>%
    do.call(weight_fun, .)
  wts <- wts / mean(wts)
  nuisance_tbl$.weights <- hardhat::importance_weights(wts)

  if (verbose) message("Fitting effect model")
  dat_effect <- left_join(
    nuisance_tbl[, c(".row", "pseudo", ".weights")],
    dat,
    by = join_by(.row)
  ) %>%
    select(-outcome, -treatment)

  fitted <- effect_wf %>%
    workflows::add_case_weights(.weights) %>%
    fit(dat_effect)

  fitted
}

multiway_cf_folds <- function(dat, v) {
  new_splits <- list()
  shuffled <- sample(nrow(dat), replace = FALSE)
  cut_ind <- as.numeric(cut(1:nrow(dat), breaks = v))
  analysis_list <- lapply(1:v, \(ind) shuffled[cut_ind == ind])
  for (i in 1:v) {
    new_splits[[i]] <- make_splits(
      x = list(
        analysis = analysis_list[[i]],
        assessment = setdiff(1:nrow(dat), analysis_list[[i]])
      ),
      data = dat
    )
  }
  manual_rset(new_splits, paste0("Fold", 1:v))
}

filter_training <- function(rset, condition_string) {
  new_splits <- list()
  dat1 <- rset$splits[[1]]$data
  for (i in 1:length(rset$id)) {
    stopifnot(identical(dat1, rset$splits[[i]]$data))

    in_id <- rset$splits[[i]]$in_id
    keep_id <- dat1[in_id, ] %>%
      filter(
        .,
        eval(rlang::parse_expr(condition_string))
      ) %>%
      pull(.row)
    new_splits[[i]] <- make_splits(
      x = list(
        analysis = keep_id,
        assessment = setdiff(dat1$.row, in_id)
      ),
      data = dat1
    )
  }
  manual_rset(new_splits, rset$id)
}
