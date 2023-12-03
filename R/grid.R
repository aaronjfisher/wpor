#' Generate a random grid of tuning parameters
#' @param size number of rows for the grid.
#' @param marginal_len the number of unique parameter values to draw from
#' for each dimension.
#' @returns a data.frame with rows that can be used with update.params(as.list(row)).
#' @export
random_grid <- function(size, ...) {
  dots <- list(...)
  out <- lapply(dots, \(d) sample(d, size, replace = TRUE)) %>%
    data.frame()
  out <- out[!duplicated(out), ]
  rownames(out) <- 1:nrow(out)
  if (nrow(out) < size) {
    warning("Due to removed duplicate rows, nrow(grid) < size.")
  }
  out
}

#' @export
#' @rdname random_grid
lightgbm_grid <- function(size, marginal_len = 20) {
  random_grid(
    size = size,
    num_trees = round(log_seq(10, 1000, length = marginal_len)),
    learning_rate = log_seq(10^-5, .25, length = marginal_len),
    max_leaves = round(log_seq(2, 50, length = marginal_len)),
    bagging_fraction = seq(.1, 1, length = marginal_len),
    min_data_in_leaf = round(log_seq(4, 50, length = marginal_len)),
    min_gain_to_split = log_seq(10^-10, 10^-2, length = marginal_len)
  )
}

#' Generate a sequence that is evenly spaced on the log scale
#' log_seq is helpful function for creating random grids
#' @export
log_seq <- function(from, to, length) {
  lfrom <- log(from)
  lto <- log(to)
  seq0 <- seq(lfrom, lto, length.out = length)
  exp(seq0)
}
# log_seq(1, 1000, 5)
