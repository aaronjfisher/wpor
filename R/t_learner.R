#' Fit a two-model-learner (t-learner)
#' @param data A data frame containing the data to fit the t-learner on.
#' @param outcome_0_wf,outcome_1_wf Workflows for the outcome models under control and treatment, respectively.
#' @export
t_learner <- function(data, outcome_0_wf, outcome_1_wf = outcome_0_wf) {
  fit1 <- outcome_1_wf %>%
    fit(filter(data, .data$treatment == 1))
  fit0 <- outcome_0_wf %>%
    fit(filter(data, .data$treatment == 0))
  out <- list(fit0 = fit0, fit1 = fit1)
  class(out) <- c("t_learner", class(out))
  out
}

#' Predict the treatment effect using a t-learner
#' @param object A fitted t-learner object.
#' @param new_data A data frame containing the new data to predict on.
#' @param ... Additional arguments passed to predict_expected_value.
#' @export
predict.t_learner <- function(object, new_data, ...) {
  predict_expected_value(object$fit1, new_data, ...) -
    predict_expected_value(object$fit0, new_data, ...)
}
