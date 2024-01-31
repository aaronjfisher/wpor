#' @export
get_x <- function(object, data, ...) {
  UseMethod("get_x")
}
#' @export
get_y <- function(object, data, ...) {
  UseMethod("get_y")
}



#' @export
get_x.lightgbm_spec <-
  get_x.tuned_boost_spec <-
  get_x.cvboost_spec <- function(object, data) {
    formula_x <- object$formula[-2] # remove LHS
    model.matrix(formula_x, data = data)
  }


#' @export
get_y.lightgbm_spec <-
  get_y.tuned_boost_spec <-
  get_y.cvboost_spec <- function(object, data) {
    y <- model.frame(object$formula, data = data)[[1]]
    if (object$mode == "classification") {
      stopifnot(all(y %in% 0:1))
      y <- as.numeric(y == 1)
    }
    y
  }

#' @export
get_y.workflow <- function(object, data) {
  form <- object$pre$actions$formula$formula
  stopifnot("formula" %in% class(form))
  data[[as.character(form[[2]])]]
}
