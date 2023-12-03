#' Fit a two-model-learner (t-learner)
#' @export
t_learner <- function(data, outcome_0_wf, outcome_1_wf = outcome_0_wf) {
  fit1 <- outcome_1_wf %>%
    fit(filter(data, treatment == 1))
  fit0 <- outcome_0_wf %>%
    fit(filter(data, treatment == 0))
  out <- list(fit0 = fit0, fit1 = fit1)
  class(out) <- c("t_learner", class(out))
  out
}

#' @export
predict.t_learner <- function(object, new_data) {
  predict_expected_value(object$fit1, new_data) -
    predict_expected_value(object$fit0, new_data)
}
