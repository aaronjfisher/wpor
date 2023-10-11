#' @export
fit <- function(...) UseMethod("fit")
fit.default <- function(object, ...) parsnip::fit(object, ...)
## see https://rstudio.github.io/r-manuals/r-exts/Generic-functions-and-methods.html

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
cvboost_spec <- function(formula, mode, control = NULL) {
  list(formula = formula, mode = mode, control = control) %>%
    add_class("cvboost_spec")
}

get_x <- function(spec, data) {
  formula_x <- spec$formula[-2] # remove LHS
  model.matrix(formula_x, data = data)
}
get_y <- function(spec, data) {
  y <- model.frame(spec$formula, data = data)[[1]]
  if (spec$mode == "classification") {
    stopifnot(all(y %in% 0:1))
    y <- as.numeric(y == 1)
  }
  y
}

#' Fit a cvboost model based on a specification
#' @export
fit.cvboost_spec <- function(object, data, weights = NULL) {
  fitted <- do.call(rlearner::cvboost, c(
    list(
      x = get_x(object, data),
      y = get_y(object, data),
      weights = weights,
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
  response <- predict(
    object = object$fitted, newx = newx_mat
  )
  postprocess_response(object$spec, response)
}

postprocess_response <- function(spec, response) {
  if (spec$mode == "regression") {
    return(tibble(.pred = response))
  }
  if (spec$mode == "classification") {
    return(tibble(
      .pred_0 = 1 - response,
      .pred_1 = response
    ))
  }
  stop("invalid mode")
}

#' @export
tuned_boost_spec <- function(
    formula, mode, control = NULL,
    data, weights = NULL) {
  init_spec <- cvboost_spec(
    formula = formula, mode = mode, control = control
  )
  fit_init <- fit(init_spec, data, weights = weights)
  init_spec$fit_init <- fit_init

  out <- init_spec %>%
    rm_class("cvboost_spec") %>%
    add_class("tuned_boost_spec")
  return(out)
}

#' @export
fit.tuned_boost_spec <- function(object, data, weights = NULL) {
  DMatrix <- xgboost::xgb.DMatrix(
    data = get_x(object, data),
    label = get_y(object, data),
    weight = weights
  )
  xgb_model <- xgboost::xgb.train(
    data = DMatrix,
    params = object$fit_init$param,
    nrounds = object$fit_init$nrounds
  )
  list(
    fitted = xgb_model,
    spec = object
  ) %>%
    add_class("tuned_boost_fit")
}

#' @export
predict.tuned_boost_fit <- function(object, new_data) {
  response <- predict(
    object$fitted,
    newdata = get_x(object$spec, new_data)
  )
  postprocess_response(object$spec, response)
}
