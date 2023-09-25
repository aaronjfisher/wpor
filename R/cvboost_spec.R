add_class <- function(obj, class_name) {
  class(obj) <- c(class_name, class(obj))
  return(obj)
}

#' Define a cvboost model specification
#' @param process_x_mat a function to be applied to
#' the result of model.matrix
#' @export
cvboost_spec <- function(formula, mode, control = NULL) {
  list(formula = formula, mode = mode, control = control) %>%
    add_class("cvboost_spec")
}

#' Fit a cvboost model based on a specification
#' @export
fit.cvboost_spec <- function(cvboost_spec, data, weights = NULL) {
  x_mat <- model.matrix(cvboost_spec$formula, data = data)
  y <- model.frame(cvboost_spec$formula, data = data)[[1]]
  if (cvboost_spec$mode == "classification") {
    stopifnot(all(y %in% 0:1))
    y <- as.numeric(y == 1)
  }

  fitted <- do.call(rlearner::cvboost, c(
    list(
      x = x_mat, y = y,
      weights = weights,
      objective = switch(cvboost_spec$mode,
        regression = "reg:squarederror",
        classification = "binary:logistic"
      )
    ),
    cvboost_spec$control
  ))

  list(fitted = fitted, spec = cvboost_spec) %>%
    add_class("cvboost_fit")
}

#' Predict cvboost results on new data
#' @export
predict.cvboost_fit <- function(cvboost_fit, new_data) {
  formula_x <- cvboost_fit$spec$formula[-2] # remove LHS
  newx_mat <- model.matrix(formula_x, data = new_data)
  response <- predict(
    object = cvboost_fit$fitted, newx = newx_mat
  )
  if (cvboost_fit$spec$mode == "regression") {
    return(tibble(.pred = response))
  }
  if (cvboost_fit$spec$mode == "classification") {
    return(tibble(
      .pred_0 = 1 - response,
      .pred_1 = response
    ))
  }
  stop("invalid mode")
}
