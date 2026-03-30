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

#' Define a lightgbm model specification
#' @param object For fit.lightgbm_spec, a lightgbm_spec
#' object created by lightgbm_spec().
#' For predict.lightgbm_fit, a lightgbm_fit object created by fit.lightgbm_spec().
#' @param formula A formula specifying the outcome and predictors for the model.
#' @param mode A character string specifying the mode of
#' the model, either "regression" or "classification".
#' @param params A named list of parameters to be passed
#' to the lightgbm training function.
#' @param weights_column An optional string specifying the
#'  name of a column in the training data frame containing observation weights.
#' @export
#' @examples
#' set.seed(0)
#' training <- sim_data(setup = "A", n = 300, p = 6, sigma = 1)
#'
#' trt_mod <- lightgbm_spec(
#'   formula = training$formulas$treatment,
#'   mode = "classification",
#' )
#'
#' fit_trt <- fit(trt_mod, training$data)
#'
#' pred_trt_train <- predict(fit_trt, training$data)
#' # boxplot(pred_trt_train ~ training$data$treatment)
#'
#' test <- sim_data(setup = "A", n = 2000, p = 6, sigma = 1)
#' pred_trt_test <- predict(fit_trt, test$data)
#' # boxplot(pred_trt_test ~ test$data$treatment)
#'
#'
#' out_mod <- lightgbm_spec(
#'   formula = training$formulas$outcome,
#'   mode = "regression",
#' )
#' fit_out <- fit(out_mod, training$data)
#'
#' pred_out_train <- predict(fit_out, training$data)
#' pred_out_test <- predict(fit_out, test$data)
#' # plot(pred_out_train, training$data$outcome)
#' # plot(pred_out_test, test$data$outcome)
lightgbm_spec <- function(
    formula,
    mode,
    params = list(),
    weights_column = NULL) {
  if (mode == "regression") {
    params$objective <- "regression"
  } else if (mode == "classification") {
    params$objective <- "binary"
  } else {
    stop("invalid mode")
  }
  list(
    formula = formula,
    mode = mode,
    params = params,
    weights_column = weights_column
  ) %>%
    add_class("lightgbm_spec")
}



#' Fit a lightgbm model based on a specification
#' @param data a data frame containing the training data.
#' @param verbose verbosity level for lightgbm training.
#' @param ... for fit.lightgbm_spec, additional arguments
#' passed to the lightgbm::lgb.train.
#' @export
#' @rdname lightgbm_spec
fit.lightgbm_spec <- function(object, data, verbose = -1, ...) {
  x <- get_x(object, data)
  y <- get_y(object, data)
  w <- get_weights(object, data)

  dtrain <- lightgbm::lgb.Dataset(data = x, label = y, weight = w)
  fit <- lightgbm::lgb.train(
    params = object$params,
    data = dtrain,
    verbose = verbose,
    ...
  )

  list(
    fit = fit,
    spec = object
  ) %>%
    add_class("lightgbm_fit")
}

#' Predict lightgbm results on new data
#' @param object a fitted lightgbm_fit object.
#' @param new_data a data frame containing the new data for which to generate predictions.
#' @param ... additional arguments passed to the predict method of the lightgbm model.
#' @rdname lightgbm_spec
#' @export
#' @importFrom stats predict
predict.lightgbm_fit <- function(object, new_data, ...) {
  stopifnot(is.data.frame(new_data))
  newx <- get_x(object$spec, new_data)
  predict(object$fit, newx, ...)
}
