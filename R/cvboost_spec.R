#' @importFrom generics fit
#' @export
generics::fit

add_class <- function(obj, class_name) {
  class(obj) <- c(class_name, class(obj))
  return(obj)
}
rm_class <- function(obj, class_name) {
  class(obj) <- setdiff(class(obj), class_name)
  return(obj)
}

#' Define a cvboost model specification
#' @param process_x_mat a function to be applied to
#' the result of model.matrix
#' @export
cvboost_spec <- function(formula, mode, control = NULL, weights_column = NULL) {
  list(
    formula = formula,
    mode = mode,
    control = control,
    weights_column = weights_column
  ) %>%
    add_class("cvboost_spec")
}


#' Fit a cvboost model based on a specification
#' @export
fit.cvboost_spec <- function(object, data) {
  w <- get_weights(object, data)
  fitted <- do.call(rlearner::cvboost, c(
    list(
      x = get_x(object, data),
      y = get_y(object, data),
      weights = w,
      objective = switch(object$mode,
        regression = "reg:squarederror",
        classification = "binary:logistic"
      )
    ),
    object$control
  ))

  list(
    fitted = fitted,
    spec = object,
    param = fitted$best_param,
    nrounds = fitted$best_ntreelimit,
    best_xgb_cvfit = fitted$best_xgb_cvfit
  ) %>%
    add_class("cvboost_fit")
}

#' Predict cvboost results on new data
#' @export
predict.cvboost_fit <- function(object, new_data) {
  newx_mat <- get_x(object$spec, new_data)
  predict(
    object = object$fitted, newx = newx_mat
  )
}


#' Define a boosting specification, tuned by cross-validation
#' @export
tuned_boost_spec <- function(data = NULL, fit_init = NULL, ...) {
  init_spec <- cvboost_spec(...)
  if (is.null(fit_init)) {
    if (is.null(data)) stop("Either data or fit_init must be provided.")
    fit_init <- fit(init_spec, data)
  }
  init_spec$param <- fit_init$param
  init_spec$nrounds <- fit_init$nrounds

  out <- init_spec %>%
    rm_class("cvboost_spec") %>%
    add_class("tuned_boost_spec")
  return(out)
}

#' Fit a tuned, boosting specification
#' @export
fit.tuned_boost_spec <- function(object, data) {
  w <- get_weights(object, data)
  DMatrix <- xgboost::xgb.DMatrix(
    data = get_x(object, data),
    label = get_y(object, data),
    weight = w
  )
  xgb_model <- xgboost::xgb.train(
    data = DMatrix,
    params = object$param,
    nrounds = object$nrounds,
    nthread = 1
  )
  list(
    fitted = xgb_model,
    spec = object
  ) %>%
    add_class("tuned_boost_fit")
}

#' @export
predict.tuned_boost_fit <- function(object, new_data) {
  predict(
    object$fitted,
    newdata = get_x(object$spec, new_data)
  )
}
