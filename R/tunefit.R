#' Tune a workflow or training algorithm
#'
#' @param trainer a parsnip workflow or model specification that can be tuned.
#' @param data a dataset to train on
#' @param v number of cv folds to use in resamples; overwritten by resamples, if provided.
#' @param size number of param sets to evaluate
#' @param resamples An ‘rset()’ object object used to evaluate wf. Defaults to rsample::vfold_cv(data, v).
#' @param burnin how many folds to examine before discarding
#' poorly performing parameter sets. Defaults to length(resamples$splits).
#' @param seed if specified, temporarily changes the seed to ensure 
#' reproducibility when computing resamples. The original seed of the calling
#' environment is restored before the tune_params function completes.
#' @param save_performance `r lifecycle::badge('experimental')` save performance
#' metrics as an attribute. If using as.tunefit, this attribute
#' will also be saved in the final, fitted workflow.
#' @returns a trainer or workflow with parameters updated to be the
#' parameters discovered to have the lowest cross-validated error.
#' @export
#' @rdname  as.tunefit
tune_params <- function(
    trainer,
    data = NULL,
    grid = NULL,
    metric = NULL,
    v = 10,
    resamples = NULL,
    alpha = 0.05,
    burnin = NULL,
    seed = NULL,
    verbose = getOption("verbose"),
    save_performance = FALSE,
    size = 10 # will be overwritten by grid
    ) {
  
  if(!is.null(seed)){
    if (exists(".Random.seed", .GlobalEnv)) {
      previous_seed <- .GlobalEnv$.Random.seed
    } else {
      previous_seed <- NULL
    }
    set.seed(seed)
  }
  
  if(is.null(resamples)) resamples <- rsample::vfold_cv(data, v)
  if(is.null(burnin)) burnin <- length(resamples$splits)
  v <- length(resamples$splits)
  stopifnot(burnin <= v)
  stopifnot(burnin >= 2)

  dat1 <- rsample::get_rsplit(resamples, 1) %>%
    rsample::analysis()

  if (is.null(grid)) {
    if (!"workflow" %in% class(trainer)) {
      stop("if trainer is not a workflow, a grid must be explicitly specified.")
    }
    form <- extract_preprocessor(trainer)
    stopifnot(class(form) == "formula")
    grid <- trainer %>%
      hardhat::extract_parameter_set_dials() %>%
      dials::finalize(model.frame(form[-2], dat1)) %>%
      dials::grid_latin_hypercube(size = size)
  }

  stopifnot(is.data.frame(grid))
  size <- nrow(grid)
  performance <- matrix(NA, v, size)
  skip_ind <- rep(FALSE, size)

  if (is.null(metric)) {
    y <- get_y(trainer, dat1)
    if (all(y %in% 0:1)) {
      metric <- metric_neg_log_lik
    } else {
      metric <- metric_mse
    }
  }

  for (i in 1:v) {
    ri <- rsample::get_rsplit(resamples, i)
    if (verbose) {
      message(
        "\nFold", i,
        ifelse(burnin == v, "", paste0("(running ", sum(1 - skip_ind), "/", size, " param sets)")),
        ": ",
        appendLF = FALSE
      )
    }
    for (j in 1:size) {
      if (skip_ind[j]) next
      if (verbose) message(".", appendLF = FALSE)
      mod_j <- update_params(trainer, as.list(grid[j, ]))
      fit_ij <- fit(mod_j, data = rsample::analysis(ri))
      pred_ij <- predict_expected_value(fit_ij, rsample::assessment(ri))

      performance[i, j] <- metric(
        pred = pred_ij,
        truth = get_y(trainer, rsample::assessment(ri)),
        weights = get_weights(trainer, rsample::assessment(ri))
      )
    }
    if (verbose) message(cli::symbol[["tick"]], appendLF = FALSE)

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
      if (verbose & i < v) message("\nStopping Early; 1 candidate left.", appendLF = FALSE)
      break
    }
  }

  if (verbose) message("\n", appendLF = FALSE)
  # matplot(apply(performance, 2, cummean), type = 'l')

  out <- update_params(trainer, new_params = as.list(grid[current_best, ]))
  if (save_performance) {
    lifecycle::signal_stage("experimental", "plot.workflow(save_performance = 'TRUE')")
    attributes(out)$tune_results <- list(
      performance = performance,
      grid = grid
    )
  }

  if(!is.null(seed)){
    if (!is.null(previous_seed)) {
      .GlobalEnv$.Random.seed <- previous_seed
    } else {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }
  
  return(out)
}







metric_mse <- function(pred, truth, weights = rep(1, length(truth))) {
  weights <- as.numeric(weights)
  mean(weights * (pred - truth)^2)
}

metric_neg_log_lik <- function(
    pred,
    truth,
    weights = rep(1, length(truth)),
    upper_lim = 100) {
  weights <- as.numeric(weights)
  losses <- -log(ifelse(truth == 1, pred, 1 - pred))
  mean(weights * pmin(upper_lim, losses))
}

#' Create a tunefit: a workflow or training algorithm with instructions for tuning.
#'
#' `tune_params`, `as.tunefit` and `fit.tunefit` are convenience functions
#' for tuning a workflow.
#'
#' `tune_params` finalizes a workflow using `finetune::tune_race_anova`,
#' cross-validation, and either RMSE
#' or AUC, depending on the workflow's `mode`.
#'
#' `as.tunefit` creates an object of class `tunefit`, which is a list containing
#' (1) a workflow, and (2) instructions on how to tune it, i.e., arguments to be
#' passed to tune_params.
#'
#' At the time of fitting, `fit.tunefit` first applies tune_params
#' to finalize the workflow, using the stored arguments from `as.tunefit`,
#' and then applies `fit.workflow` to fit the finalized workflow.
#' The as.tunefit class can also be used with custom training algorithms
#' that are not workflows (see the "Fitting WPOR Models" vignette for an example).
#'
#' @param ... args to be passed to tune_params (e.g., v, grid) before fitting the workflow.
#' @export
#' @examples
#' library(tidymodels)
#' library(dplyr)
#'
#' set.seed(0)
#' training <- sim_data(setup = "A", n = 300, p = 6, sigma = 1)
#' training$data <- sim_data(setup = "A", n = 300, p = 6, sigma = 1)$data
#'
#' mod_spec <- boost_tree(min_n = tune(), learn_rate = tune()) %>%
#'   set_engine("xgboost")
#'
#' wf <- workflow() %>%
#'   add_model(set_mode(mod_spec, "classification")) %>%
#'   add_formula(training$formulas$treatment)
#'
#' if (FALSE) {
#'   # Optionally, the tuning process can be parallelized by first
#'   # registering a cluster.
#'   cores <- parallel::detectCores(logical = FALSE)
#'   cl <- makePSOCKcluster(cores)
#'   registerDoParallel(cl)
#' }
#'
#' ## Example using tune_params explicitly
#' tuned_wf <- tune_params(wf, data = training$data, size = 2, seed = 0) # setting size artificially small for example
#' fitted1 <- fit(tuned_wf, training$data)
#' pred1 <- predict(fitted1, training$data, "prob")
#'
#' ## Example using tune_params implicitly
#' fitted2 <- as.tunefit(wf, size = 2, seed = 0) %>%
#'   wpor::fit(training$data)
#' pred2 <- predict(fitted2, training$data, "prob")
#'
#' testthat::expect_equal(pred1, pred2)
as.tunefit <- function(trainer, ...) {
  dots <- list(...)
  list(trainer = trainer, tune_args = dots) %>%
    add_class("tunefit")
}


#' Fit a tunefit trainer
#'
#' fit.tunefit returns a tuned, fitted workflow.
#'
#' @export
#' @rdname  as.tunefit
fit.tunefit <- function(object, data) {
  tuned <- do.call(tune_params, c(
    list(
      trainer = object$trainer,
      data = data
    ),
    object$tune_args
  ))
  fitted <- fit(tuned, data)
  attributes(fitted)$tune_results <- attributes(tuned)$tune_results

  return(fitted)
}
