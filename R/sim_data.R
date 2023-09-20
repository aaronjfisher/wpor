#' Simulate data based on various articles
#' @export
sim_data <- function(setup, n, p, sigma, add_splines) {
  if (setup == "A") {
    X <- matrix(runif(n * p, min = 0, max = 1), n, p)
    b <- sin(pi * X[, 1] * X[, 2]) + 2 * (X[, 3] - 0.5)^2 + X[, 4] + 0.5 * X[, 5]
    eta <- 0.1
    e <- pmax(eta, pmin(sin(pi * X[, 1] * X[, 2]), 1 - eta))
    tau <- (X[, 1] + X[, 2]) / 2
  } else if (setup == "B") {
    X <- matrix(rnorm(n * p), n, p)
    b <- pmax(0, X[, 1] + X[, 2], X[, 3]) + pmax(0, X[, 4] + X[, 5])
    e <- rep(0.5, n)
    tau <- X[, 1] + log(1 + exp(X[, 2]))
  } else if (setup == "C") {
    X <- matrix(rnorm(n * p), n, p)
    b <- 2 * log(1 + exp(X[, 1] + X[, 2] + X[, 3]))
    e <- 1 / (1 + exp(X[, 2] + X[, 3]))
    tau <- rep(1, n)
  } else if (setup == "D") {
    X <- matrix(rnorm(n * p), n, p)
    b <- (pmax(X[, 1] + X[, 2] + X[, 3], 0) + pmax(X[, 4] + X[, 5], 0)) / 2
    e <- 1 / (1 + exp(-X[, 1]) + exp(-X[, 2]))
    tau <- pmax(X[, 1] + X[, 2] + X[, 3], 0) - pmax(X[, 4] + X[, 5], 0)
  } else if (setup == "E") {
    X <- matrix(runif(n, -1, 1), n, 1)
    e <- 0.1 + 0.8 * (X > 0)
    b <- (X <= -.5) * 0.5 * (X + 2)^2 + (X / 2 + 0.875) * (X > -1 / 2 & X < 0) +
      (X > 0 & X < .5) * (-5 * (X - 0.2)^2 + 1.075) + (X > .5) * (X + 0.125)
    tau <- rep(0, n)
  } else if (setup == "F") {
    lwr <- .05
    x_vec <- (runif(n, lwr, 1 - lwr))
    e <- sqrt(x_vec)
    tau <- rep(1, n)
    b <- (x_vec <= -.5) * 0.5 * (x_vec + 2)^2 + (x_vec / 2 + 0.875) * (x_vec > -1 / 2 & x_vec < 0) +
      (x_vec > 0 & x_vec < .5) * (-5 * (x_vec - 0.2)^2 + 1.075) + (x_vec > .5) * (x_vec + 0.125)
    X <- splines::ns(x_vec, 5)
  } else if (setup == "RCT") {
    x_vec <- (runif(n, lwr, 1 - lwr))
    e <- rep(1 / 2, n)
    tau <- rep(0, n)
    b <- (x_vec <= -.5) * 0.5 * (x_vec + 2)^2 + (x_vec / 2 + 0.875) * (x_vec > -1 / 2 & x_vec < 0) +
      x_vec > 0 +
      (x_vec > 0 & x_vec < .5) * (-5 * (x_vec - 0.2)^2 + 1.075) + (x_vec > .5) * (x_vec + 0.125)
    X <- splines::ns(x_vec, 5)
  } else {
    stop("bad setup")
  }

  mu0 <- b - tau / 2
  mu1 <- b + tau / 2
  eta <- mu1 * e + mu0 * (1 - e)
  a <- rbinom(n, 1, e)
  y <- (1 - a) * mu0 + a * mu1 + sigma * rnorm(n)
  stopifnot(all(abs(mu1 - mu0 - tau) < 10^-9))

  # For lasso, we add spline features, as in Q&W19
  make_matrix <- function(x) stats::model.matrix(~ . - 1, x)


  X_ns <- do.call(cbind, lapply(1:p, function(col) {
    matrix(splines::ns(X[, col], df = p + 1), n, p + 1)
  }))
  dim_ns <- dim(X_ns)[2]
  X_ns <- stats::model.matrix(~ . * . - 1, data.frame(X_ns)) # pairwise interaction (not including squared term for each column)
  X_ns_sq <- do.call(cbind, lapply(1:dim_ns, function(col) {
    matrix(X_ns[, col]^2)
  })) # squared term for each column
  X_ns <- cbind(X_ns, X_ns_sq) %>%
    data.frame() %>%
    make_matrix()

  list(x = X, x_ns = X_ns, eta = eta, mu1 = mu1, mu0 = mu0, tau = tau, e = e, y = y, a = a)
}
