# wpor: a package for weighted pseudo-outcome regression

This package implements the meta-learner methods discussed in [Fisher (2023)](https://arxiv.org/abs/2307.09700). 


## Installation

```r
devtools::install_github('aaronjfisher/wpor')
```

## Specifying workflows for the nuisance models and the conditional effect model

Rather than hard-coding a handful of options for nuisance models, we aimed to make `wpor` integrate easily with existing collections of R training algorithms that share a common syntax. The most popular of these collections is the `tidymodels` set of packages, namely `parsnip` and `workflows`. The simplest way to specify a training algorithm is to define a  "workflow" object (see examples in the "Fitting WPOR Models" vignette)

For instructions on how to use nuisance models not included in `parsnip`, see the section "Extentions for algorithms not available in `parsnip`", below.

## Convenience functions for parameter tuning

In general when implementing cross-fitting with hyper-parameter tuning, there are two common approaches:

* Strict cross-fitting with nested tuning: within each iteration of cross-fitting, run cross-validation to select tuning parameters.
* Approximate cross-fitting: pre-tune parameters via cross-validation before running running cross-fitting. Then use the same tuning parameters over all iterations of cross-fitting. At the time of this writing, this approach is what is used in the `rlearner` [package](https://github.com/xnie/rlearner/blob/6806396960e672214e2ef36e16c76bbb58ef9114/R/rboost.R#L56-L68).

`wpor` includes convenience functions for either approach. For the approximate method, see `?tune_params`. 

For the strict method, we suggest using the object class `tunefit`, which packages a workflow or training algorithm together with arguments for how that algorithm it should be tuned (see `?as.tunefit`). These instructions are then applied within in each iteration of cross-fitting.



## Extentions for algorithms not available in `tidymodels`

While it is possible to integrate new training algorithms in `parsnip` syntax, the process is somewhat complex, as `parsnip` manages many aspects of the modeling process. In `wpor`, our goal was the mix the convenience of the pre-existing `parsnip` library with the flexibility of lightweight extensions. 

With this in mind, the `wpor` package internally uses only a handful of methods for `workflow` objects. Users who wish to use custom model fitting algorithms need only define these methods for their algorithm's object class. The methods are as follows.


* Methods required for any algorithm (referred to below as `trainer`)
	* `fit(trainer, data, ...)`: returns a trained model object with a `predict` method. The `predict` method should return a vector of expected values for the predicted outcome. For example, given a training data frame `data1`, a test data frame `data2`, and a training algorithm object `trainer`, running `fitted <- fit(object, data1)` should produce a fitted model, and running `predict(fitted, data2)` should produce a vector of predictions for the expected value of the outcome. 
		* Examples: `fit.lightgbm_spec`, `predict.lightgbm_fit`
	* Alternatively, if a `predict` method already exists that produces output in a different format and users do not wish to overwrite this method, they can instead define a method `predict_expected_value(fitted, data2)` that produces a vector of predictions for the expected value.
		* Example: `predict_expected_value.workflow`
* Methods for algorithms used in the pseudo-outcome regression (POR) step
	* `add_weights_column(trainer, column)`: specifies a column name of the training data that will be treated as a weight column.
		* Example: `add_weights_column.workflow`
	* `get_weights_column(trainer, column)`: extract the column of weights.
		* Example: `get_weights_column.workflow`
* Methods for algorithms that wish to use the `wpor::tune_params` or `wpor::as.tunefit` functions
	* `get_y(trainer, data)`: extract the column of outcomes to be predicted.
		* Example: `get_y.workflow`
	* `update_params(trainer, new_params)`: replace the tuning parameters of the trainer with the list `new_params`. 
		* Example: `update_params.workflow`









