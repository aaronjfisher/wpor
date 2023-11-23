#' Tune a workflow and return the best workflow
#'
#' @param wf a parsnip workflow
#' @param data a dataset to train on
#' @param v number of cv folds to use in resamples; overwritten by resamples, if provided.
#' @param size number of param sets to evaluate
#' @param resamples An ‘rset()’ object object used to evaluate wf
#' @param burnin how many folds to examine before discarding
#' poorly performing parameter sets
#' @param save_performance `r lifecycle::badge('experimental')` save performance
#' metrics as an attribute. If using as.tuneflow, this attribute
#' will also be saved in the final, fitted workflow.
#' @export
#' @rdname  as.tuneflow
tune_wf <- function(
    wf, data = NULL,
    v = 10,
    size = 10,
    resamples = rsample::vfold_cv(data, v),
    alpha = 0.05,
    burnin = length(resamples$splits),
    verbose = TRUE,
    save_performance = FALSE) {
  v <- length(resamples$splits)
  stopifnot(burnin <= v)
  stopifnot(burnin >= 2)

  mode <- workflows::extract_spec_parsnip(wf)$mode
  if (mode == "classification") {
    type <- "prob"
    metric <- metric_neg_log_lik
  }
  if (mode == "regression") {
    type <- NULL
    metric <- metric_mse
  }
  form <- wf$pre$actions$formula$formula

  stopifnot("formula" %in% class(form))
  truth_lab <- as.character(form[[2]])

  if (verbose) message("Generating ", size, " parameter combinations")

  grid <- wf %>%
    hardhat::extract_parameter_set_dials() %>%
    dials::finalize(model.frame(form[-2], data)) %>%
    dials::grid_latin_hypercube(size = size)
  # dials::grid_max_entropy(size = size)
  performance <- matrix(NA, v, size)
  skip_ind <- rep(FALSE, size)
  for (i in 1:v) {
    ri <- get_rsplit(resamples, i)
    if (verbose) message("\n\nFitting Fold-", i, ": studying ", sum(!skip_ind), "/", length(skip_ind), " parameter combinations")
    for (j in 1:size) {
      if (skip_ind[j]) next
      if (verbose) message("par:", j, ", ", appendLF = FALSE)
      wfj <- finalize_workflow(wf, grid[j, ])
      fit_ij <- fit(wfj, data = analysis(ri))
      pred_ij <- predict(fit_ij, assessment(ri), type = type)

      performance[i, j] <- metric(
        pred = pred_ij,
        truth = assessment(ri)[[truth_lab]],
        weights = get_weights(wfj, assessment(ri))
      )

      rm(wfj, pred_ij, fit_ij)
      gc() # strangely, this seems to be required to avoid memory issues associated with tidymodels
      # absent this gc command, we get errors such as ` *** caught segfault ***; address 0x4a145d5, cause 'memory not mapped'`
      # TO DO: At some point, try removing gc statement again to see if other changes had remedied it?
      # Be sure to test for size ~= 100.
    }

    # Check which params to skip
    current_best <- which(
      colMeans(performance, na.rm = TRUE)
      ==
        min(colMeans(performance[, !skip_ind], na.rm = TRUE))
    )[1]
    perf_best <- performance[1:i, current_best]
    for (j in 1:size) {
      if (skip_ind[j]) next
      perf_j <- performance[1:i, j]
      if (i >= burnin & min(var(perf_j), var(perf_best)) > 10^-5) {
        skip_ind[j] <- t.test(perf_best, perf_j, alternative = "less")$p.value < alpha
      }
    }
    if (sum(!skip_ind) == 1) {
      if (verbose) message("\n\nStopping Early; 1 candidate left.\n")
      break
    }
  }

  if (verbose) message("\n")
  # matplot(apply(performance, 2, cummean), type = 'l')

  out <- finalize_workflow(wf, grid[current_best, ])
  if (save_performance) {
    lifecycle::signal_stage("experimental", "plot.workflow(save_performance = 'TRUE')")
    attributes(out)[["tune_results"]] <- list(
      performance = performance,
      grid = grid
    )
  }

  return(out)
}

#' @export
plot.workflow <- function(object) {
  lifecycle::signal_stage("experimental", "plot.workflow()")
  perf <- attributes(object)[["tune_results"]]$performance
  if (is.null(perf)) {
    stop("wpor::plot.workflow requires attributes(object)[['tune_results']] to be non-null.")
  }
  matplot(apply(perf, 2, cummean),
    type = "l",
    ylab = "CV Loss", xlab = "CV Fold"
  )
}

get_weights <- function(wf, data) {
  weight_col <- wf$pre$actions$case_weights$col
  if (is.null(weight_col)) {
    return(rep(1, nrow(data)))
  } else {
    weight_name <- weight_col %>% rlang::quo_get_expr()
    return(data[[weight_name]])
  }
}
# tune_wf <- function(
#     wf, data = NULL,
#     v = 10,
#     resamples = rsample::vfold_cv(data, v),
#     control = finetune::control_race(burn_in = 2),
#     ...
#   ){
#   mode <- workflows::extract_spec_parsnip(wf)$mode
#   if(mode == 'classification') metric = yardstick::metric_set(mn_log_loss)
#   if(mode == 'regression') metric = yardstick::metric_set(rmse)
#   tg <- tune_grid(wf, resamples, metrics = metric, ...)
#   #tg <- finetune::tune_race_anova(wf, resamples, metrics = metric, control = control, ...)
#   #pdf("Rplot.pdf"); finetune::plot_race(tg); dev.off()
#   #tg <- finetune::tune_sim_anneal(wf, resamples, iter = 20) # seems too slow
#
#   return(list(
#     final_workflow = finalize_workflow(wf, select_best(tg)),
#     tune_result = tg
#   ))
# }



metric_mse <- function(pred, truth, weights = rep(1, length(truth))) {
  weights <- as.numeric(weights)
  mean(weights * (pred$.pred - truth)^2)
}

metric_neg_log_lik <- function(pred, truth, weights = rep(1, length(truth))) {
  if (is.null(pred$.pred_0)) {
    pred$.pred_0 <- 1 - pred$.pred_1
  }
  if (is.null(pred$.pred_1)) {
    pred$.pred_1 <- 1 - pred$.pred_0
  }
  weights <- as.numeric(weights)
  -mean(weights * (truth == 1) * log(pred$.pred_1) + weights * (truth == 0) * log(1 - pred$.pred_0))
}

#' Create a tuneflow: a workflow with instructions for tuning.
#'
#' `tune_wf`, `as.tuneflow` and `fit.tuneflow` are convenience functions
#' for tuning a workflow.
#'
#' `tune_wf` finalizes a workflow using `finetune::tune_race_anova`,
#' cross-validation, and either RMSE
#' or AUC, depending on the workflow's `mode`.
#'
#' `as.tuneflow` creates an object of class `tuneflow`, which is a list containing
#' (1) a workflow, and (2) instructions on how to tune it, i.e., arguments to be
#' passed to tune_wf.
#'
#' At the time of fitting, `fit.tuneflow` first applies tune_wf
#' to finalize the workflow, using the stored arguments from `as.tuneflow`,
#' and then applies `fit.workflow` to fit the finalized workflow.
#'
#' @param ... args to be passed to tune_wf (e.g., v, grid) before fitting the workflow.
#' @export
#' @examples \dontrun{
#' library(tidymodels)
#' library(dplyr)
#'
#' set.seed(0)
#' train_data <- sim_data(setup = "A", n = 300, p = 6, sigma = 1)$data
#'
#' x_terms <- train_data %>%
#'   select(starts_with("x.")) %>%
#'   colnames()
#' rhs <- paste(x_terms, collapse = " + ")
#' treatment_formula <- formula(paste("treatment ~", rhs))
#'
#' mod_spec <- boost_tree(min_n = tune(), learn_rate = tune()) %>%
#'   set_engine("xgboost")
#'
#' wf <- workflow() %>%
#'   add_model(set_mode(mod_spec, "classification")) %>%
#'   add_formula(treatment_formula)
#'
#' if (FALSE) {
#'   # Optionally, the tuning process can be parallelized by first
#'   # registering a cluster.
#'   cores <- parallel::detectCores(logical = FALSE)
#'   cl <- makePSOCKcluster(cores)
#'   registerDoParallel(cl)
#' }
#'
#' ## Example using tune_wf explicitly
#' set.seed(0)
#' tuned_wf <- tune_wf(wf, data = train_data)
#' fitted1 <- fit(tuned_wf, train_data)
#' pred1 <- predict(fitted1, train_data, "prob")
#'
#' ## Example using tune_wf implicitly
#' set.seed(0)
#' fitted2 <- as.tuneflow(wf) %>%
#'   wpor::fit(train_data)
#' pred2 <- predict(fitted2, train_data, "prob")
#'
#' range(pred1 - pred2) # equivalent results
#' }
as.tuneflow <- function(wf, ...) {
  stopifnot("workflow" %in% class(wf))
  dots <- list(...)
  list(workflow = wf, tune_args = dots) %>%
    add_class("tuneflow")
}


#' Fit a tuneflow object
#'
#' fit.tuneflow returns a tuned, fitted workflow.
#'
#' @export
#' @rdname  as.tuneflow
fit.tuneflow <- function(object, data) {
  tuned <- do.call(tune_wf, c(
    list(
      wf = object$workflow,
      data = data
    ),
    object$tune_args
  ))
  fitted <- fit(tuned, data)
  attributes(fitted)$tune_results <- attributes(tuned)$tune_results

  return(fitted)
}


#' Generate subsamples for tuning
#'
#' This function is useful for pre-tuning hyperparameters
#' for samples of different sizes.
#'
#' @export
subsample <- function(data, n_analysis, times) {
  N <- nrow(data)
  splits <- lapply(1:times, \(i){
    in_id <- sample(N, n_analysis, replace = FALSE)
    make_splits(list(
      analysis = in_id,
      assessment = setdiff(1:N, in_id)
    ), data = data)
  })
  manual_rset(splits, ids = paste(
    "Subsample", 1:times
  ))
}
