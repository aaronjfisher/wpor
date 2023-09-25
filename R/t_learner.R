#' Fit a two-model-learner (t-learner)
#' @export
t_learner <- function(data, outcome_wf) {
  fit1 <- outcome_wf %>%
    fit(filter(data, treatment == 1))
  fit0 <- outcome_wf %>%
    fit(filter(data, treatment == 0))
  out <- list(fit0 = fit0, fit1 = fit1)
  class(out) <- c("t_learner", class(out))
  out
}

#' @export
predict.t_learner <- function(object, new_data) {
  tibble(
    .pred =
      predict(object$fit1, new_data)$.pred -
        predict(object$fit0, new_data)$.pred
  )
}
