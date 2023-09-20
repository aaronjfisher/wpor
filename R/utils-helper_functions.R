ptrim <- function(x, min_val, max_val) {
  stopifnot(min_val <= max_val)
  pmin(x, max_val) %>%
    pmax(min_val)
}
