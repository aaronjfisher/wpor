# update_params
# add_weights
# fit #without specifying weights; do in cv folds
# predict




#' Define a lightgbm model specification
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
#' boxplot(pred_trt_train$.pred_1 ~ training$data$treatment)
#'
#' test <- sim_data(setup = "A", n = 2000, p = 6, sigma = 1)
#' pred_trt_test <- predict(fit_trt, test$data)
#' boxplot(pred_trt_test$.pred_1 ~ test$data$treatment)
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
#' plot(pred_out_train$.pred, training$data$outcome)
#' plot(pred_out_test$.pred, test$data$outcome)
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
#' @export
#' @rdname lightgbm_spec
fit.lightgbm_spec <- function(object, data, verbose = -1, ...) {
  x <- get_x(object, data)
  y <- get_y(object, data)
  if (is.null(object$weights_column)) {
    w <- y * 0 + 1
  } else {
    w <- data[[object$weights_column]]
  }
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
#' @rdname lightgbm_spec
#' @export
predict.lightgbm_fit <- function(object, new_data) {
  stopifnot(is.data.frame(new_data))
  newx <- get_x(object$spec, new_data)
  response <- predict(object$fit, newx)
  postprocess_response(object$spec, response)
  # response
}
