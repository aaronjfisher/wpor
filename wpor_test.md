---
title: "Testing WPOR"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Testing WPOR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


## Simulate data


```r
library("wpor")
library("dplyr")
library("tidymodels")

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

rf_mod <-
  rand_forest(trees = 100) %>%
  set_engine("ranger")

treatment_wf <- workflow() %>%
  add_model(set_mode(rf_mod, "classification")) %>%
  add_formula(treatment_formula)
outcome_wf <- workflow() %>%
  add_model(set_mode(rf_mod, "regression")) %>%
  add_formula(outcome_formula)
effect_wf <- workflow() %>%
  add_model(set_mode(rf_mod, "regression")) %>%
  add_formula(effect_formula)

treatment_wf
#> ══ Workflow ══════════════════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: rand_forest()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────────────────
#> treatment ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> ── Model ─────────────────────────────────────────────────────────────────────────
#> Random Forest Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 100
#> 
#> Computational engine: ranger
outcome_wf
#> ══ Workflow ══════════════════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: rand_forest()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────────────────
#> outcome ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> ── Model ─────────────────────────────────────────────────────────────────────────
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   trees = 100
#> 
#> Computational engine: ranger
effect_wf
#> ══ Workflow ══════════════════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: rand_forest()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────────────────
#> pseudo ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6
#> 
#> ── Model ─────────────────────────────────────────────────────────────────────────
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   trees = 100
#> 
#> Computational engine: ranger
```

## Fit weighted pseudo-outcome regression


```r
fitted <- fit_wpor(
  data = train_data,
  outcome_wf = outcome_wf,
  treatment_wf = treatment_wf,
  effect_wf = effect_wf,
  pseudo_fun = pseudo_U,
  weight_fun = weight_U_AX,
  min_prob = 0.01,
  v = 5,
  cf_order = 2
)
#> Cross fitting treatment model
#> Cross fitting outcome model
#> Reshaping nuisance predictions
#> Fitting effect model

## MSE
mean((
  predict(fitted, test_data)$.pred
    - test_list$params$tau
)^2)
#> [1] 0.6646484
```

Alternatively, the nuisance predictions can be pre-computed. This is helpful when comparing options for `pseudo_fun` and/or `weight_fun`.


```r
nuisance_tbl <- crossfit_nuisance(
  data = train_data,
  outcome_wf = outcome_wf,
  treatment_wf = treatment_wf,
  min_prob = 0.01,
  v = 5,
  cf_order = 2
)
#> Cross fitting treatment model
#> Cross fitting outcome model
#> Reshaping nuisance predictions

fitted2 <- fit_wpor(
  data = train_data,
  nuisance_tbl = nuisance_tbl,
  effect_wf = effect_wf,
  pseudo_fun = pseudo_DR,
  # pseudo_fun = pseudo_cov,
  # pseudo_fun = pseudo_U,
  # weight_fun = weight_DR_X,
  weight_fun = weight_DR_AX,
  # weight_fun = weight_1,
  # weight_fun = weight_U_AX,
  # weight_fun = weight_U_X,
)
#> Fitting effect model

mean((
  predict(fitted2, test_data)$.pred
    - test_list$params$tau
)^2)
#> [1] 0.7115013
```

# Two-Learner (T-Learner) approach



