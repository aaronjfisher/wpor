utils::globalVariables(".")


#' Trim values in a numeric vector to be within a specified range
#' @param x A numeric vector to be trimmed.
#' @param min_val The minimum value to which elements of `x` should be trimmed.
#' @param max_val The maximum value to which elements of `x` should be trimmed.
#' @importFrom dplyr %>%
ptrim <- function(x, min_val, max_val) {
  stopifnot(min_val <= max_val)
  pmin(x, max_val) %>%
    pmax(min_val)
}
