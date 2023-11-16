
#' Tune a workflow and return the best workflow
#'
#' @param wf a parsnip workflow
#' @param data a dataset to train on
#' @param v number of cv folds to use
#' @param ... passed to tune_race_anova
#' @export
#' @rdname  as.tuneflow
tune_wf <- function(
    wf, data = NULL, 
    v = 10, 
    resamples = rsample::vfold_cv(data, v), 
    control = finetune::control_race(burn_in = 2), 
    ...
  ){
  mode <- extract_spec_parsnip(wf)$mode
  if(mode == 'classification') metric = yardstick::metric_set(mn_log_loss)
  if(mode == 'regression') metric = yardstick::metric_set(rmse)
  #tg <- tune_grid(wf, resamples)
  tg <- finetune::tune_race_anova(wf, resamples, metrics = metric, control = control, ...)
  #pdf("Rplot.pdf"); finetune::plot_race(tg); dev.off()
  #tg <- finetune::tune_sim_anneal(wf, resamples, iter = 20) # seems too slow
  
  return(list(
    final_workflow = finalize_workflow(wf, select_best(tg)),
    tune_result = tg
  ))
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
#' train_data <- sim_data(setup = 'A', n = 300, p = 6, sigma = 1)$data
#' 
#' x_terms <- train_data %>%
#'   select(starts_with("x.")) %>%
#'   colnames
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
#' if(FALSE){
#'   # Optionally, the tuning process can be parallelized by first
#'   # registering a cluster.
#'   cores <- parallel::detectCores(logical = FALSE)
#'   cl <- makePSOCKcluster(cores)
#'   registerDoParallel(cl)
#' }
#' 
#' ## Example using tune_wf explicitly
#' tuned_wf <- tune_wf(wf, data = train_data)$final_workflow
#' fitted1 <- fit(tuned_wf, train_data)
#' pred1 <- predict(fitted1, train_data, 'prob')
#' 
#' ## Example using tune_wf implicitly
#' fitted2 <- as.tuneflow(wf) %>% 
#'   wpor::fit(train_data)
#' pred2 <- predict(fitted1, train_data, 'prob')
#' 
#' range(pred1 - pred2) #equivalent results
#' }
as.tuneflow <- function(wf, ...){
  stopifnot('workflow' %in% class(wf))
  dots <- list(...)
  list(workflow = wf, tune_args = dots) %>%
    add_class("tuneflow")
}


#' Fit a tuneflow object
#' @param ... passed to fit.workflow (e.g., weights)
#' @export
#' @rdname  as.tuneflow
fit.tuneflow <- function(object, data, ...) {
  tuned_wf <- do.call(tune_wf, c(
    list(
      wf = object$workflow,
      data = data
    ), 
    object$tune_args
  ))$final_workflow
  stopifnot(class(tuned_wf) == 'workflow')
  fit(tuned_wf, data)
}

#' Generate subsamples for tuning
#' 
#' This function is useful for pre-tuning hyperparameters
#' for samples of different sizes.
#' 
#' @export
subsample <- function(data, n_analysis, times){
  N <- nrow(data)
  splits <- lapply(1:times, \(i){
    in_id <- sample(N, n_analysis, replace = FALSE)
    make_splits(list(
      analysis = in_id, 
      assessment = setdiff(1:N, in_id)
    ), data = data)
  })
  manual_rset(splits, ids = paste(
    'Subsample', 1:times
  ))
}