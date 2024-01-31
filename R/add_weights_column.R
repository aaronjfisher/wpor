#' Specify a column name to use for weights
#' @export
#' @examples
#' training <- sim_data(setup = "A", n = 700, p = 6, sigma = 1)
#' tdata <- training$data
#' tdata$w <- 1
#'
#' gbm <- lightgbm_spec(
#'   formula = training$formulas$treatment,
#'   mode = "classification",
#' )
#' gbmw <- add_weights_column(gbm, "w")
#' w <- get_weights(gbmw, tdata)
#' stopifnot(identical(w, tdata$w))
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
add_weights_column.tunefit <- function(object, column) {
  weighted_trainer <- add_weights_column(object$trainer, column)
  object$trainer <- weighted_trainer
  object
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
get_weights_column.tuned_boost_spec <- function(object) {
  object$weights_column
}
#' @export
get_weights_column.cvboost_spec <- function(object) {
  object$weights_column
}
#' @export
get_weights_column.workflow <- function(object) {
  wc <- object$pre$actions$case_weights$col
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
