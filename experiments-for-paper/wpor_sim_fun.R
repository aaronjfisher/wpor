#devtools::install_github('aaronjfisher/wpor')
devtools::load_all('..')
library("dplyr")
library("tidymodels")
library("pbapply")
library("rlearner")

simulate_from_df <- function(sim_df, nthread = 1, verbose = FALSE){
  
  n_test <- 10000
  cf_order <- 2
  v <- 5
  min_prob <- 0.01
  
  # pseudo_fun = pseudo_DR
  # pseudo_fun = pseudo_cov,
  # pseudo_fun = pseudo_U,
  # weight_fun = weight_DR_X,
  # weight_fun = weight_DR_AX
  # weight_fun = weight_1,
  # weight_fun = weight_U_AX,
  # weight_fun = weight_U_X,
  mse_NA <- expand.grid(
    pseudo = c('pseudo_U','pseudo_DR','T'),#rlearner_package
    weights = c(
      'weight_1',
      'weight_U_X', 
      'weight_U_AX',
      'weight_DR_X', 
      'weight_DR_AX',
      'weight_DR_alt_AX'),
    mse = NA
  ) %>%
    filter(
      !(pseudo == 'T' & weights != 'weight_1'),
      !(pseudo == 'rlearner_package' & weights != 'weight_U_AX'),
      !(pseudo == 'pseudo_U' & weights == 'weight_DR_X'),
      !(pseudo == 'pseudo_U' & weights == 'weight_DR_AX'),
      !(pseudo == 'pseudo_U' & weights == 'weight_DR_alt_AX'),
      !(pseudo == 'pseudo_DR' & weights == 'weight_U_X'),
      !(pseudo == 'pseudo_DR' & weights == 'weight_U_AX')
      ) %>%
    arrange(pseudo, weights)
  
  
  
  for (i in 1:nrow(sim_df)){
    if(i==1) pb <- timerProgressBar(0, nrow(sim_df), width=15)
    if(verbose) message('\n')
    print(sim_df[i,])
    if(verbose) message(Sys.time())
    
    set.seed(sim_df$seed[i])
    n_obs <- sim_df$n_obs[i]
    ######## simulate data
    
    train_data <- with(sim_df[i,], sim_data(
      setup = setup, n = n_obs, p = p, sigma = sigma
    )$data)
    test_list <- with(sim_df[i,], sim_data(
      setup = setup, n = n_test, p = p, sigma = sigma
    ))
    test_data <- test_list$data
    
    
    
    x_terms <- train_data %>%
      select(starts_with("x.")) %>%
      colnames
    rhs <- paste(x_terms, collapse = " + ")
    
    treatment_formula <- formula(paste("treatment ~", rhs))
    outcome_formula <- formula(paste("outcome ~", rhs))
    effect_formula <- formula(paste("pseudo ~", rhs))
    
    
    
    if(sim_df$learners[i]=='random_forest'){
      rf_mod <-
        rand_forest(trees = 100) %>%
        set_engine("ranger")
      
      treatment_wf <- workflow() %>%
        add_model(set_mode(rf_mod, "classification")) %>%
        add_formula(treatment_formula)
      t_learner_wf <-
      outcome_wf <- workflow() %>%
        add_model(set_mode(rf_mod, "regression")) %>%
        add_formula(outcome_formula)
      effect_wf <- workflow() %>%
        add_model(set_mode(rf_mod, "regression")) %>%
        add_formula(effect_formula)
    }
    if(sim_df$learners[i]=='boost'){
      if(verbose) message('tuning specifications...')
      treatment_wf <- tuned_boost_spec(
        treatment_formula,
        "classification",
        data = train_data,
        control = list(nthread = nthread)
      )
      outcome_wf <- tuned_boost_spec(
        outcome_formula,
        "regression",
        data = train_data,
        control = list(nthread = nthread)
      )
      t_learner_wf <- cvboost_spec(outcome_formula, "regression",
                                   control = list(nthread = nthread))
      effect_wf <- cvboost_spec(effect_formula, "regression",
                                control = list(nthread = nthread))
    }
    
    nuisance_tbl <- crossfit_nuisance(
      data = train_data,
      outcome_wf = outcome_wf,
      treatment_wf = treatment_wf,
      min_prob = min_prob,
      v = v,
      cf_order = cf_order,
      verbose = verbose
    )
    
    mse_i <- mse_NA
    for(j in 1:nrow(mse_i)){
      if(verbose) message(mse_i$pseudo[j],'...',message(Sys.time()))
      if(mse_i$pseudo[j] == 'rlearner_package'){
        if(sim_df$learners[i] !='boost') next
        fitted_j <- rlearner::rboost(
          x = as.matrix(train_data[,x_terms]),
          y = train_data$outcome,
          w = as.numeric(train_data$treatment==1),
          nthread = nthread
        )
      } else if (mse_i$pseudo[j] == 'T') {
        fitted_j <- t_learner(
          data = train_data,
          outcome_wf = t_learner_wf
        )
      } else {
        pseudo_fun <- switch(as.character(mse_i$pseudo[j]),
          'pseudo_U' = pseudo_U,
          'pseudo_DR' = pseudo_DR
        )
        weight_fun <- switch(as.character(mse_i$weights[j]),
          'weight_1' = weight_1,
          'weight_U_X' = weight_U_X,
          'weight_U_AX' = weight_U_AX,
          'weight_DR_X' = weight_DR_X,
          'weight_DR_AX' = weight_DR_AX,
          'weight_DR_alt_AX' = weight_DR_alt_AX
        )
        fitted_j <- fit_wpor(
          data = train_data,
          nuisance_tbl = nuisance_tbl,
          effect_wf = effect_wf,
          pseudo_fun = pseudo_fun,
          weight_fun = weight_fun,
          verbose = FALSE
        )
      }
      if(mse_i$pseudo[j] == 'rlearner_package'){
        pred <- predict(fitted_j, as.matrix(test_data[,x_terms]))
      } else {
        pred <- predict(fitted_j, test_data)$.pred
      }
      mse_i$mse[j] <- mean( ( pred - test_list$params$tau )^2 )
    }
   
    sim_df$mse[[i]] <- mse_i
    setTimerProgressBar(pb, i)
  }
    
  
  
  sim_df
}

# s0 <- simulate_seed(0)
# gc()
