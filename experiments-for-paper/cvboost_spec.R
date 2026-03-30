library(rlearner)
library(wpor)

add_class <- function(obj, class_name) {
  class(obj) <- c(class_name, class(obj))
  return(obj)
}
rm_class <- function(obj, class_name) {
  class(obj) <- setdiff(class(obj), class_name)
  return(obj)
}


#' Define a cvboost model specification
#'
#' Note, these specifications were used for comparison in the publication associated
#' with this packages, but the preferred workflow is to use tidymodels.
#'
#' @param formula A formula object specifying the model to be fit.
#' @param mode A character string specifying the mode
#' of the model, either "regression" or "classification".
#' @param control A list of control parameters to be
#' passed to the cvboost function.
#' @param weights_column A character string specifying
#' the name of the column in the data frame that contains observation weights.
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
#' @param object A cvboost_spec object.
#' @param data A data frame containing the data to fit the model on.
#' @param ... Additional arguments passed to rlearner::cvboost.
#' @export
fit.cvboost_spec <- function(object, data, ...) {
  w <- wpor::get_weights(object, data)
  if (!requireNamespace("rlearner")) {
    stop("The rlearner package is required to fit a cvboost model.")
  }
  fitted <- do.call(rlearner::cvboost, c(
    list(
      x = get_x(object, data),
      y = get_y(object, data),
      weights = w,
      objective = switch(object$mode,
        regression = "reg:squarederror",
        classification = "binary:logistic"
      ),
      ...
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
#' @param object A fitted cvboost_fit object.
#' @param new_data A data frame containing the new data
#' on which to make predictions.
#' @param ... Additional arguments passed to stats::predict.
#' @export
#' @importFrom stats predict
predict.cvboost_fit <- function(object, new_data, ...) {
  newx_mat <- get_x(object$spec, new_data)
  stats::predict(
    object = object$fitted, newx = newx_mat, ...
  )
}


#' Define a boosting specification, tuned by cross-validation
#'
#' Either data or fit_init must be provided.
#'
#' @param data A data frame containing the data to fit the initial cvboost model on.
#' @param fit_init An initial cvboost fit object, if already available.
#' @param ... Additional arguments passed to cvboost_spec.
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
#' @param object A tuned_boost_spec object.
#' @param data A data frame containing the data to fit the model on.
#' @param ... Additional arguments passed to xgboost::xgb.train.
fit.tuned_boost_spec <- function(object, data, ...) {
  w <- wpor::get_weights(object, data)
  DMatrix <- xgboost::xgb.DMatrix(
    data = get_x(object, data),
    label = get_y(object, data),
    weight = w
  )
  xgb_model <- xgboost::xgb.train(
    data = DMatrix,
    params = object$param,
    nrounds = object$nrounds,
    nthread = 1,
    ...
  )
  list(
    fitted = xgb_model,
    spec = object
  ) %>%
    add_class("tuned_boost_fit")
}


predict.tuned_boost_fit <- function(object, new_data, ...) {
  stats::predict(
    object$fitted,
    newdata = get_x(object$spec, new_data),
    ...
  )
}

add_weights_column.cvboost_spec <- function(object, column, ...) {
  stopifnot(is.character(column))
  object$weights_column <- column
  object
}


get_weights_column.cvboost_spec <- function(object, ...) {
  object$weights_column
}


get_x_general <- function(object, data, ...) {
  formula_x <- object$formula[-2] # remove LHS
  stats::model.matrix(formula_x, data = data)
}
get_y_general <- function(object, data, ...) {
  y <- stats::model.frame(object$formula, data = data)[[1]]
  if (object$mode == "classification") {
    stopifnot(all(y %in% 0:1))
    y <- as.numeric(y == 1)
  }
  y
}


get_x.cvboost_spec <- get_x_general
get_y.cvboost_spec <- get_y_general
get_x.tuned_boost_spec <- get_x_general
get_y.tuned_boost_spec <- get_y_general 


get_weights_column.tuned_boost_spec <- function(object, ...) {
  object$weights_column
}

