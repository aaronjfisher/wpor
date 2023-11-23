test_that("weights are used in tune_wf, but not in standard tune::tune_grid", {
  library(tidymodels)
  library(dplyr)

  # Create a dataset where x only matters in an "active" subgroup
  # Weighting the active group should produce a small penalty
  # Weighting the non-active group should produce a large penalty
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
  my_tuned_active <- tune_wf(wf_active, data = train, resamples = rs, size = 10)
  my_tuned_nonactive <- tune_wf(wf_nonactive, data = train, resamples = rs, size = 10)

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
