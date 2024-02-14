#' IHDP Dataset
#'
#' This package contains 100 iterations of the IHDP dataset from Shalit et al. (2017),
#' which is adapted from Hill (2011). This dataset is copied from https://www.fredjo.com/.
#'
#' The dataset is a list contains 100 simulations of 672 training observations
#' and 75 test observations. It contains arrays of treatments (t),
#' covariates (x), true conditional effects (mu0 and mu1), factual outcomes (yf),
#' and counterfactual outcomes (ycf).
#'
#' References:
#'
#' \itemize{
#' \item{}{Shalit, U., Johansson, F.D. &amp; Sontag, D.. (2017). Estimating individual treatment effect: generalization bounds and algorithms. <i>Proceedings of the 34th International Conference on Machine Learning</i>, in <i>Proceedings of Machine Learning Research</i> 70:3076-3085}
#' \item{}{Hill, Jennifer L. Bayesian nonparametric modeling for causal inference. Journal of Computational and Graphical Statistics, 20(1), 2011.}
#' }
"ihdp"
