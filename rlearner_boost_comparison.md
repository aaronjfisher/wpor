---
title: "R-Learner Boosting Comparison"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{R-Learner Boosting Comparison}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


## Simulate data


```r
library("wpor")
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library("tidymodels")
#> ── Attaching packages ──────────────────────────────────────── tidymodels 1.1.1 ──
#> ✔ broom        1.0.5     ✔ rsample      1.2.0
#> ✔ dials        1.2.0     ✔ tibble       3.2.1
#> ✔ ggplot2      3.4.3     ✔ tidyr        1.3.0
#> ✔ infer        1.0.5     ✔ tune         1.1.2
#> ✔ modeldata    1.2.0     ✔ workflows    1.1.3
#> ✔ parsnip      1.1.1     ✔ workflowsets 1.0.1
#> ✔ purrr        1.0.2     ✔ yardstick    1.2.0
#> ✔ recipes      1.0.8
#> ── Conflicts ─────────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ purrr::is_null() masks testthat::is_null()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ tidyr::matches() masks rsample::matches(), dplyr::matches(), testthat::matches()
#> ✖ recipes::step()  masks stats::step()
#> • Use tidymodels_prefer() to resolve common conflicts.

set.seed(456)
p <- 6
n_train <- 500
n_test <- 20000
sigma <- 1
setup <- "B"
train_data <- sim_data(
  setup = setup, n = n_train, p = p, sigma = sigma
)$data
test_list <- sim_data(
  setup = setup, n = n_test, p = p, sigma = sigma
)
test_data <- test_list$data
```


## Define workflows for each nuisance model

```r
cn <- colnames(train_data)
x_terms <- cn[starts_with("x.", vars = cn)]
rhs <- paste(x_terms, collapse = " + ")

treatment_formula <- formula(paste("treatment ~", rhs))
outcome_formula <- formula(paste("outcome ~", rhs))
effect_formula <- formula(paste("pseudo ~", rhs))

treatment_wf <- cvboost_spec(treatment_formula, "classification")
outcome_wf <- cvboost_spec(outcome_formula, "regression")
effect_wf <- cvboost_spec(effect_formula, "regression")

treatment_wf
#> $formula
#> treatment ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> $mode
#> [1] "classification"
#> 
#> $control
#> NULL
#> 
#> attr(,"class")
#> [1] "cvboost_spec" "list"
outcome_wf
#> $formula
#> outcome ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> $mode
#> [1] "regression"
#> 
#> $control
#> NULL
#> 
#> attr(,"class")
#> [1] "cvboost_spec" "list"
effect_wf
#> $formula
#> pseudo ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> $mode
#> [1] "regression"
#> 
#> $control
#> NULL
#> 
#> attr(,"class")
#> [1] "cvboost_spec" "list"
```

## Fit weighted pseudo-outcome regression



