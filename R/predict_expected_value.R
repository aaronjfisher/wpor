#' @export
predict_expected_value <- function(object, new_data, ...) {
  UseMethod("predict_expected_value")
}


#' @export
predict_expected_value.default <- function(object, ...) {
  pred <- predict(object, ...)
  if (all(pred %in% 0:1)) {
    stop("Invalid predict_expected_value.default method; all predictions are 0 or 1 exactly")
  }
  pred
}

#' @export
predict_expected_value.workflow <- function(object, ...) {
  mode <- workflows::extract_spec_parsnip(object)$mode
  if (mode == "classification") {
    ev <- predict(object, type = "prob", ...)$.pred_1
    stopifnot(all(ev <= 1))
    stopifnot(all(ev >= 0))
  } else if (mode == "regression") {
    ev <- predict(object, ...)$.pred
  }
  return(ev)
}
