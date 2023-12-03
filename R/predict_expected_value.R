#' @export
predict_expected_value <- function(object, new_data, ...) {
  UseMethod("predict_expected_value")
}


#' @export
predict_expected_value.default <- function(object, ...) {
  predict(object, ...)
}

#' @export
predict_expected_value.workflow <- function(object, ...) {
  mode <- workflows::extract_spec_parsnip(wf)$mode
  if (mode == "classification") {
    ev <- predict(object, type = "prob", ...)$.pred_1
    stopifnot(all(ev <= 1))
    stopifnot(all(ev >= 0))
  } else if (mode == "regression") {
    ev <- predict(object, ...)$.pred
  }
  return(ev)
}
