#' Define a boosting specification, tuned by cross-validation
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


predict.tuned_boost_fit <- function(object, new_data) {
  predict(
    object$fitted,
    newdata = get_x(object$spec, new_data)
  )
}
