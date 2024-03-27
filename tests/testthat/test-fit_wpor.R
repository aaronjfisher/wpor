library("dplyr")
library("tidymodels")

options("verbose" = TRUE)

set.seed(0)
p <- 6
n_train <- 500
n_test <- 20000
sigma <- 1
setup <- "C"
train_data <- sim_data(
  setup = setup, n = n_train, p = p, sigma = sigma
)$data
test_list <- sim_data(
  setup = setup, n = n_test, p = p, sigma = sigma
)
test_data <- test_list$data

cn <- colnames(train_data)
x_terms <- cn[starts_with("x.", vars = cn)]
rhs <- paste(x_terms, collapse = " + ")

treatment_formula <- formula(paste("treatment ~", rhs))
outcome_marginal_formula <- formula(paste("outcome ~", rhs))
outcome_single_formula <- formula(paste("outcome ~ treatment + ", rhs))
effect_formula <- formula(paste("pseudo ~", rhs))

treatment_wf <- workflow() %>%
  add_model(set_mode(logistic_reg(), "classification")) %>%
  add_formula(treatment_formula)
outcome_marginal_wf <- workflow() %>%
  add_model(set_mode(linear_reg(), "regression")) %>%
  add_formula(outcome_marginal_formula)
outcome_single_wf <- workflow() %>%
  add_model(set_mode(linear_reg(), "regression")) %>%
  add_formula(outcome_single_formula)
effect_wf <- workflow() %>%
  add_model(set_mode(linear_reg(), "regression")) %>%
  add_formula(effect_formula)


test_that("Errors happen iff not enough workflows are specified", {
  expect_no_error({
    fitted <- fit_wpor(
      data = train_data,
      outcome_marginal_wf = outcome_marginal_wf,
      outcome_1_separate_wf = NA,
      outcome_0_separate_wf = NA,
      outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_single,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
  expect_no_error({
    fitted <- fit_wpor(
      data = train_data,
      # outcome_marginal_wf = outcome_marginal_wf,
      # outcome_1_separate_wf = NA,
      # outcome_0_separate_wf = NA,
      outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_single,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
  expect_error({
    fitted <- fit_wpor(
      data = train_data,
      # outcome_marginal_wf = outcome_marginal_wf,
      # outcome_1_separate_wf = NA,
      # outcome_0_separate_wf = NA,
      # outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_single,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
  expect_error({
    fitted <- fit_wpor(
      data = train_data,
      # outcome_marginal_wf = outcome_marginal_wf,
      # outcome_1_separate_wf = NA,
      # outcome_0_separate_wf = NA,
      outcome_single_wf = outcome_single_wf,
      # treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_single,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
  expect_error({
    fitted <- fit_wpor(
      data = train_data,
      outcome_marginal_wf = outcome_marginal_wf,
      outcome_1_separate_wf = NA,
      outcome_0_separate_wf = NA,
      # outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_single,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
  expect_error({
    fitted <- fit_wpor(
      data = train_data,
      # outcome_marginal_wf = outcome_marginal_wf,
      outcome_1_separate_wf = NA,
      outcome_0_separate_wf = NA,
      outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_U,
      weight_fun = weight_U_X,
      v = 2
    )
  })
  expect_error({
    fitted <- fit_wpor(
      data = train_data,
      outcome_marginal_wf = outcome_marginal_wf,
      # outcome_1_separate_wf = NA,
      # outcome_0_separate_wf = NA,
      outcome_single_wf = outcome_single_wf,
      treatment_wf = treatment_wf,
      effect_wf = effect_wf,
      pseudo_fun = pseudo_DR_separate,
      weight_fun = weight_DR_X,
      v = 2
    )
  })
})
