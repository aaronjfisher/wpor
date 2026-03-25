#' Get the design matrix and response vector from a
#' model specification or workflow and data
#' @param object A model specification or workflow object.
#' @param data A data frame containing the data to
#' extract the design matrix and response vector from.
#' @param ... Placeholder for generic arguments.
#' @export
get_x <- function(object, data, ...) {
  UseMethod("get_x")
}

#' @rdname get_x
#' @export
get_y <- function(object, data, ...) {
  UseMethod("get_y")
}


get_x_general <- function(object, data, ...) {
  formula_x <- object$formula[-2] # remove LHS
  stats::model.matrix(formula_x, data = data)
}

#' @rdname get_x
#' @export
get_x.lightgbm_spec <- get_x_general

#' @rdname get_x
#' @export
get_x.tuned_boost_spec <- get_x_general

#' @rdname get_x
#' @export
get_x.cvboost_spec <- get_x_general



get_y_general <- function(object, data, ...) {
  y <- stats::model.frame(object$formula, data = data)[[1]]
  if (object$mode == "classification") {
    stopifnot(all(y %in% 0:1))
    y <- as.numeric(y == 1)
  }
  y
}

#' @rdname get_x
#' @export
get_y.lightgbm_spec <- get_y_general

#' @rdname get_x
#' @export
get_y.tuned_boost_spec <- get_y_general

#' @rdname get_x
#' @export
get_y.cvboost_spec <- get_y_general

#' @export
get_y.workflow <- function(object, data, ...) {
  form <- object$pre$actions$formula$formula
  stopifnot("formula" %in% class(form))
  data[[as.character(form[[2]])]]
}
