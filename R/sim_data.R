#' Simulate data based on Nie & Wager, 2018.
#' See https://github.com/xnie/rlearner/blob/6806396960e672214e2ef36e16c76bbb58ef9114/experiments_for_paper/run_simu.R
#' @export
sim_data <- function(setup, n, p, sigma) {
  stopifnot(p >= 5 | setup %in% c('E','F'))
  if (setup == "A") {
    get.params <- function() {
      X <- matrix(runif(n * p, min = 0, max = 1), n, p)
      b <- sin(pi * X[, 1] * X[, 2]) + 2 * (X[, 3] - 0.5)^2 + X[, 4] + 0.5 * X[, 5]
      eta <- 0.1
      e <- pmax(eta, pmin(sin(pi * X[, 1] * X[, 2]), 1 - eta))
      tau <- (X[, 1] + X[, 2]) / 2
      list(X = X, b = b, tau = tau, e = e)
    }
  } else if (setup == "B") {
    get.params <- function() {
      X <- matrix(rnorm(n * p), n, p)
      b <- pmax(0, X[, 1] + X[, 2], X[, 3]) + pmax(0, X[, 4] + X[, 5])
      e <- rep(0.5, n)
      tau <- X[, 1] + log(1 + exp(X[, 2]))
      list(X = X, b = b, tau = tau, e = e)
    }
  } else if (setup == "C") {
    get.params <- function() {
      X <- matrix(rnorm(n * p), n, p)
      b <- 2 * log(1 + exp(X[, 1] + X[, 2] + X[, 3]))
      e <- 1 / (1 + exp(X[, 2] + X[, 3]))
      tau <- rep(1, n)
      list(X = X, b = b, tau = tau, e = e)
    }
  } else if (setup == "D") {
    get.params <- function() {
      X <- matrix(rnorm(n * p), n, p)
      b <- (pmax(X[, 1] + X[, 2] + X[, 3], 0) + pmax(X[, 4] + X[, 5], 0)) / 2
      e <- 1 / (1 + exp(-X[, 1]) + exp(-X[, 2]))
      tau <- pmax(X[, 1] + X[, 2] + X[, 3], 0) - pmax(X[, 4] + X[, 5], 0)
      list(X = X, b = b, tau = tau, e = e)
    }
  } else if (setup == "E") { #Kennedy
    get.params <- function() {
      X <- matrix(runif(n,-1,1), n, 1)
      colnames(X) <- 'x.1'
      e <- 0.1 + 0.8*(X>0)
      b <- (X <= -.5)*0.5*(X+2)^2 + (X/2+0.875)*(X>-1/2 & X<0) +
        (X>0 & X<.5)*(-5*(X-0.2)^2 +1.075) + (X>.5)*(X+0.125)
      tau <- rep(0, n)  
      list(X = X, b = b, tau = tau, e = e)
    }
  } else if (setup == "F") { #simple illustration  
    get.params <- function() {
      lwr <- .05
      x_vec <- runif(n, lwr, 1-lwr)
      X <- matrix(x_vec, ncol = 1)
      colnames(X) <- 'x.1'
      e <- x_vec
      b <- rep(1, n)
      tau <- rep(1, n)
      list(X = X, b = b, tau = tau, e = e)
    }
  } else {
    stop("bad setup")
  }

  params <- get.params()
  treatment_num <- rbinom(n, 1, params$e)
  treatment_factor <- factor(treatment_num, levels = 0:1)
  outcome <- params$b + (treatment_num - 0.5) * params$tau + sigma * rnorm(n)
  colnames(outcome) <- NULL
  
  list(
    data = data.frame(
      outcome = outcome,
      treatment = treatment_factor,
      x = params$X
    ),
    params = params
  )
}
