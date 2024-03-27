# Create a dataset where x only matters in an "active" subgroup
# Weighting the active group should produce a small penalty
# Weighting the non-active group should produce a large penalty
library(tidymodels)
library(dplyr)

set.seed(0)
n <- 100
p <- 10
x <- matrix(rnorm(n * p), n, p)
active <- rbinom(n, 1, .5)
y <- rnorm(n) + rowSums(tcrossprod(active, rep(1, p)) * x * 10)
train <- cbind(y, data.frame(x), active) %>%
  mutate(
    w_active = parsnip::importance_weights(active),
    w_nonactive = parsnip::importance_weights(1 - active)
  )
tibble()
# ggplot(train, aes(x = rowSums(x),y=y, col = as.factor(active))) + geom_point()

x_terms <- train %>%
  select(starts_with("X")) %>%
  colnames()
rhs <- paste(x_terms, collapse = " + ")
form <- formula(paste("treatment ~", rhs))



test_that("weights are used in tune_params for lasso, but not in standard tune::tune_grid", {
  wf_unweighted <- workflow() %>%
    add_model(linear_reg(
      penalty = tune(),
      engine = "glmnet"
    )) %>%
    add_formula(y ~ .)

  wf_active <- wf_unweighted %>%
    workflows::add_case_weights(w_active)
  wf_nonactive <- wf_unweighted %>%
    workflows::add_case_weights(w_nonactive)

  rs <- rsample::vfold_cv(train, v = 2)
  my_tuned_active <- tune_params(wf_active, data = train, resamples = rs, size = 10)
  my_tuned_nonactive <- tune_params(wf_nonactive, data = train, resamples = rs, size = 10)

  my_active_penalty <- workflows::extract_spec_parsnip(my_tuned_active)$args$penalty %>%
    rlang::eval_tidy(expr = .)
  my_nonactive_penalty <- workflows::extract_spec_parsnip(my_tuned_nonactive)$args$penalty %>%
    rlang::eval_tidy(expr = .)

  expect_true(my_active_penalty * 100 < my_nonactive_penalty)


  set.seed(2)
  standard_active_penalty <- (tune_grid(
    wf_active,
    resamples = rs,
    grid = 10
  ) %>%
    select_best("rmse"))$penalty
  standard_nonactive_penalty <- (tune_grid(
    wf_nonactive,
    resamples = rs,
    grid = 10
  ) %>%
    select_best("rmse"))$penalty
  expect_false(standard_active_penalty * 10 < standard_nonactive_penalty)
})



test_that("xgboost (parsnip) fit uses weights", {
  wf_unweighted <- workflow() %>%
    add_model(boost_tree(engine = "xgboost", mode = "regression")) %>%
    add_formula(y ~ .)

  wf_active <- wf_unweighted %>%
    workflows::add_case_weights(w_active)
  wf_nonactive <- wf_unweighted %>%
    workflows::add_case_weights(w_nonactive)

  fitted_active <- fit(wf_active, data = train)
  fitted_nonactive <- fit(wf_nonactive, data = train)

  train_active <- filter(train, active == 1)
  pred_active_from_active <- predict(fitted_active, train_active)
  pred_active_from_nonactive <- predict(fitted_nonactive, train_active)
  error_right_weights <- mean((pred_active_from_active$.pred - train_active$y)^2)
  error_wrong_weights <- mean((pred_active_from_nonactive$.pred - train_active$y)^2)

  expect_true(error_right_weights * 20 < error_wrong_weights)
})





test_that("Test that tune_params works for lightgbm & workflows without errors", {
  # This test used to be a vignette, but
  # I removed this vignette in favor of
  # ?tune_params and ?as.tunefit examples.

  # Simulate data


  set.seed(0)
  training <- sim_data(setup = "A", n = 700, p = 6, sigma = 1)
  tdata <- training$data
  tdata$w <- 1

  gbm <- lightgbm_spec(
    formula = training$formulas$treatment,
    mode = "classification",
  )
  gbm

  # head(get_y(gbm, tdata))
  # head(get_x(gbm, tdata))
  update_params(gbm, list(learning_rate = .1))


  gbm_grid <- lightgbm_grid(5)

  expect_no_error({
    tuned_gbm <- tune_params(
      gbm,
      grid = gbm_grid,
      metric = NULL,
      resamples = rsample::vfold_cv(tdata, 4),
      alpha = 0.05,
      burnin = 3,
      verbose = TRUE,
      save_performance = TRUE
    )
  })
  # matplot(attributes(tuned_gbm)$tune_results$performance, type = "l")


  # Define a tidymodels workflow
  mod_spec <- boost_tree(min_n = tune(), learn_rate = tune()) %>%
    set_engine("xgboost")
  #'
  wf <- workflow() %>%
    add_model(set_mode(mod_spec, "classification")) %>%
    add_formula(training$formulas$treatment)


  expect_no_error({
    tuned_wf <- tune_params(
      wf,
      metric = NULL,
      resamples = rsample::vfold_cv(tdata, 5),
      alpha = 0.05,
      burnin = 3,
      verbose = TRUE,
      save_performance = TRUE,
      size = 5
    )

    fit_tuned_wf <- fit(tuned_wf, tdata)
  })
})
