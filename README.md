# wpor: a package for weighted pseudo-outcome regression

This package implements the meta-learner methods discussed in [Fisher (2023)](https://arxiv.org/abs/2307.09700), including 

* Inverse Variance Weighted Doubly Robust Learning,
* Standard Doubly Robust (DR) Learning,
* Robinson (R) Learning,
* U Learning, 
* Covariance-based Learning,
* Inverse Variance Weighted, Covariance-based Learning, and
* Two-Model (T) Learning. 


## Installation

```r
devtools::install_github('aaronjfisher/wpor')
```

## Specifying the nuisance models and the conditional effect model

As WPOR is a meta algorithm that is agnostic to the models used, we aimed to make `wpor` compatible with a wide variety of chocies. Rather than hard-coding a handful of options for nuisance models, `wpor` integrates with the collection of training algorithms in the `tidymodels` set of packages, namely `parsnip` and `workflows`. These packages provide a standardized syntax for many popular training algorithms. The simplest way to specify a training algorithm in `wpor` is to define a  "workflow" object (see examples in the "Fitting WPOR Models" vignette).

For examples of how to extend `wpor` to use custom algorithms not available in `parsnip`, see the "Custom models" vignette.

## Parameter tuning

In general when implementing cross-fitting with hyper-parameter tuning, there are two common approaches:

* Strict cross-fitting with nested tuning: within each iteration of cross-fitting, run cross-validation to select tuning parameters.
* Approximate cross-fitting with pre-tuned parameters: Before running cross-fitting, pre-tune the parameters with cross-validation. Next, use the same tuning parameters over all iterations of cross-fitting. This technically breaks the independencies that cross-fitting aims to ensure, as all data folds are used in selecting tuning parameters. At the time of this writing, this approximate approach is what is used in the `rlearner` [package](https://github.com/xnie/rlearner/blob/6806396960e672214e2ef36e16c76bbb58ef9114/R/rboost.R#L56-L68).

`wpor` includes convenience functions for either approach. For the approximate method, see `?tune_params`. For the strict method, we suggest using the object class `tunefit`, which packages a workflow or training algorithm together with arguments for how that algorithm should be tuned (see `?as.tunefit`). These instructions are then applied within in each iteration of cross-fitting.


