#' @export
update_params <- function(object, ...) {
  UseMethod("update_params")
}

#' @export
update_params.lightgbm_spec <- function(object, new_params) {
  stopifnot(is.list(new_params))
  new_names <- names(new_params)
  object$params[new_names] <- new_params
  object
}

#' @export
update_params.workflow <- function(object, new_params) {
  tune::finalize_workflow(object, new_params)
}



#' @export
get_params <- function(object, ...) {
  UseMethod("update_params")
}

#' @export
get_params.lightgbm_spec <- function(object, new_params) {
  object$params
}
