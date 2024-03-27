#' A space-filling workflow that always outputs NA predictions.
#'
#' These are applied as defaults when a user does not supply a workflow.
#' @export
#' @examples
#' wf <- get_na_workflow()
#' fitted <- fit(wf, mtcars)
#' preds <- predict(fitted, mtcars)
#' stopifnot(all(is.na(preds)))
#' stopifnot(length(preds) == nrow(mtcars))
get_na_workflow <- function() {
  na_workflow <- list()
  class(na_workflow) <- c("na_workflow", "list")
  return(na_workflow)
}

#' @export
fit.na_workflow <- function(...) {
  na_model <- list()
  class(na_model) <- c("na_model", "list")
  return(na_model)
}

#' @export
predict.na_model <- function(object, new_data, ...) {
  return(rep(NA, nrow(new_data)))
}
