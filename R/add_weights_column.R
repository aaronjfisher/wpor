#' @export
add_weights_column <- function(object, ...) {
  UseMethod("add_weights_column")
}

#' @export
add_weights_column.lightgbm_spec <- function(object, column) {
  stopifnot(is.character(column))
  object$weights_column <- column
  object
}

#' @export
add_weights_column.cvboost_spec <- function(object, column) {
  stopifnot(is.character(column))
  object$weights_column <- column
  object
}
#' @export
add_weights_column.workflow <- function(object, column) {
  stopifnot(is.character(column))
  workflows::add_case_weights(object, !!as.symbol(column))
}







#' @export
get_weights_column <- function(object, ...) {
  UseMethod("get_weights_column")
}

#' @export
get_weights_column.lightgbm_spec <- function(object) {
  object$weights_column
}

#' @export
get_weights_column.cvboost_spec <- function(object) {
  object$weights_column
}
#' @export
get_weights_column.workflow <- function(object) {
  wc <- wf$pre$actions$case_weights$col
  if (!is.null(wc)) {
    wc <- rlang::quo_get_expr(wc)
  }
  wc
}

#' @export
get_weights <- function(object, data) {
  weight_col <- get_weights_column(object)
  if (is.null(weight_col)) {
    return(rep(1, nrow(data)))
  } else {
    return(data[[weight_col]])
  }
}
