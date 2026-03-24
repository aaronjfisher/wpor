#' Update the parameters of a model specification or workflow
#' @param object A model specification or workflow object to update.
#' @param new_params A named list of new parameters to update in the model specification or workflow.
#' @param ... Placeholder for future arguments, not currently used.
#' @export
update_params <- function(object, new_params, ...) {
  UseMethod("update_params")
}

#' @export
#' @rdname update_params
update_params.lightgbm_spec <- function(object, new_params, ...) {
  stopifnot(is.list(new_params))
  new_names <- names(new_params)
  object$params[new_names] <- new_params
  object
}

#' @export
#' @rdname update_params
update_params.workflow <- function(object, new_params, ...) {
  if (length(list(...)) > 0) {
    stop("Additional arguments passed to update_params.workflow are currently ignored.")
  }
  tune::finalize_workflow(object, new_params)
}
