#renv::install('aaronjfisher/wpor')
#renv::install('..')
#renv::snapshot()
#devtools::document('..')
#devtools::install('..')
#unloadNamespace('wpor')
#devtools::load_all('..')
library('wpor')
library("dplyr")
library("tidymodels")
library("pbapply")
library("rlearner")
library("workflows")

simulate_from_df <- function(
    sim_df, 
    mse_NA,
    nthread = 1, 
    verbose = FALSE,
    time_limit = Inf,
    burnin = 3,
    size,
    v = 10, # This is not the major source of slowdown, tuning is.
    ...){
  
  setTimeLimit(elapsed=time_limit)
  
  n_test <- 10000
  min_prob <- 0.01
  
  
  
  for (i in 1:nrow(sim_df)){
    if(i==1) pb <- timerProgressBar(0, nrow(sim_df), width=15)
    start_i <- Sys.time()
    if(verbose){
      message('\n')
      print(sim_df[i,])
    }
    
    seed_i <- sim_df$seed[i]
    set.seed(seed_i)
    n_obs <- sim_df$n_obs[i]
    cf_order <- sim_df$cf_order[i]
    burnin_i <- burnin
    if(cf_order > 2) burnin_i <- cf_order
    
    #      _                 _       _
    #  ___(_)_ __ ___  _   _| | __ _| |_ ___
    # / __| | '_ ` _ \| | | | |/ _` | __/ _ \
    # \__ \ | | | | | | |_| | | (_| | ||  __/
    # |___/_|_| |_| |_|\__,_|_|\__,_|\__\___|
    

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
    outcome_marginal_formula <- formula(paste("outcome ~", rhs))
    outcome_single_formula <- formula(paste("outcome ~ treatment + ", rhs))
    effect_formula <- formula(paste("pseudo ~", rhs))

    if(sim_df$learners[i] == 'rlearner_package'){
      fitted_j <- rlearner::rboost(
        x = as.matrix(train_data[,x_terms]),
        y = train_data$outcome,
        w = as.numeric(train_data$treatment==1),
        nthread = nthread,
        num_search_rounds = size)
      
      pred <- predict(fitted_j, as.matrix(test_data[,x_terms]))
      
      mse_i <- tibble(
        pseudo='pseudo_U',
        weights = 'weight_U_AX',
        pred_mse =  mean( ( pred - test_list$params$tau )^2 ),
        ate_error =  mean(pred) - mean(test_list$params$tau)
        )

      sim_df$mse[[i]] <- mse_i
      sim_df$tune_time[i] <- 0
      sim_df$crossfit_time[i] <- NA
      sim_df$por_time[i] <- NA
      setTimerProgressBar(pb, i)
      next
    } 
    
    #  _
    # | |_ _   _ _ __   ___
    # | __| | | | '_ \ / _ \
    # | |_| |_| | | | |  __/
    #  \__|\__,_|_| |_|\___|
    
    
    if(sim_df$learners[i] %in% c('parsnip_random_forest', 'parsnip_boost')){
      if(sim_df$learners[i] == 'parsnip_random_forest'){
        mod_spec <- rand_forest(trees = tune(), min_n = tune())
      }
      if(sim_df$learners[i] == 'parsnip_boost'){
        mod_spec <- boost_tree(
          trees = tune(), 
          min_n = tune(), 
          mtry = tune(),
          learn_rate = tune(),
          sample_size = tune(),
          loss_reduction = tune(),
          stop_iter = tune(),
          tree_depth = tune()
        )
      }
      tune_parsnip <- function(mode, formula, data=train_data, ...){
          if(cf_order>2){
            nuisance_resamples <-  multiway_cf_folds(data, cf_order)
          } else {
            nuisance_resamples <- NULL
          }
          workflow() %>%
           add_model(set_mode(mod_spec, mode)) %>%
           add_formula(formula) %>%
           tune_params(
            data = data, verbose = verbose, seed = seed_i, 
            size = size, v=v, burnin = burnin_i, 
            resamples = nuisance_resamples, 
            ...)
      }
      
      if(verbose) message(Sys.time(),': ','tuning treatment...')
      treatment_wf <- tune_parsnip("classification", treatment_formula, ...)
      out_marginal_wf <- tune_parsnip("regression",outcome_marginal_formula, ...)
      out_single_wf <- tune_parsnip("regression", outcome_single_formula, ...)
      if(verbose) message(Sys.time(),': ','tuning outcome 0...')
      outcome_0_separate_wf <- tune_parsnip("regression", outcome_marginal_formula, filter(train_data, treatment == 0), ...)
      if(verbose) message(Sys.time(),': ','tuning outcome 1...')
      outcome_1_separate_wf <- tune_parsnip("regression", outcome_marginal_formula, filter(train_data, treatment == 1), ...)
      if(verbose) message(Sys.time(),': ','tuning outcome marginal...')
      outcome_marginal_wf <- tune_parsnip("regression", outcome_marginal_formula, ...)
      if(verbose) message(Sys.time(),': ','tuning outcome single...')
      outcome_single_wf <- tune_parsnip("regression", outcome_single_formula, ...)
      effect_wf <- workflow() %>%
        add_model(set_mode(mod_spec, "regression")) %>%
        add_formula(effect_formula) %>%
        as.tunefit(verbose = verbose, size = size, seed = seed_i, v=v, burnin = burnin_i, group = '.row', ...)

    } else if(sim_df$learners[i] == c('lightgbm')){

      if(verbose) message(Sys.time(),': ','tuning treatment...')
      
      tune_gbm <- function(data, ...){
        if(cf_order>2){
            nuisance_resamples <-  multiway_cf_folds(data, cf_order)
          } else {
            nuisance_resamples <- NULL
          }
        tune_params(
          data = data,
          verbose = verbose, 
          grid = lightgbm_grid(size, num_threads = 1),
          seed = seed_i,
          burnin = burnin_i,
          resamples = nuisance_resamples,
          ...)
      }
      treatment_wf <- lightgbm_spec(formula = treatment_formula, mode = "classification") %>%
        tune_gbm(data = train_data, ...)

      out_marginal_wf <- lightgbm_spec(formula = outcome_marginal_formula, mode = "regression")
      out_single_wf <- lightgbm_spec(formula = outcome_single_formula, mode = "regression")
      
      if(verbose) message(Sys.time(),': ','tuning outcome 0...')
      outcome_0_separate_wf <- tune_gbm(out_marginal_wf, data = filter(train_data, treatment == 0), ...)
      if(verbose) message(Sys.time(),': ','tuning outcome 1...')
      outcome_1_separate_wf <- tune_gbm(out_marginal_wf, data = filter(train_data, treatment == 1), ...)
      if(verbose) message(Sys.time(),': ','tuning outcome marginal...')
      outcome_marginal_wf <- tune_gbm(out_marginal_wf, data = train_data, ...)
      if(verbose) message(Sys.time(),': ','tuning outcome single...')
      outcome_single_wf <- tune_gbm(out_single_wf, data = train_data, ...)
      effect_wf <- lightgbm_spec(formula = effect_formula, mode = "regression") %>%
        as.tunefit(verbose = verbose, size = size, grid = lightgbm_grid(size), seed = seed_i, v=v, burnin = burnin_i, group = '.row', ...)

    } else if(sim_df$learners[i]=='cvboost'){

      if(verbose) message(Sys.time(),': ','tuning treatment...')
      treatment_wf <- tuned_boost_spec(
        formula = treatment_formula,
        mode = "classification",
        data = train_data,
        control = list(nthread = nthread, num_search_rounds = size)
      )
      if(verbose) message(Sys.time(),': ','tuning outcome marginal...')
      outcome_marginal_wf <- tuned_boost_spec(
        formula = outcome_marginal_formula,
        mode = "regression",
        data = train_data,
        control = list(nthread = nthread, num_search_rounds = size)
      )
      if(verbose) message(Sys.time(),': ','tuning outcome single...')
      outcome_single_wf <- tuned_boost_spec(
        formula = outcome_single_formula,
        mode = "regression",
        data = train_data,
        control = list(nthread = nthread, num_search_rounds = size)
      )
      if(verbose) message(Sys.time(),': ','tuning outcome 1...')
      outcome_1_separate_wf <- tuned_boost_spec(
        formula = outcome_marginal_formula,
        mode = "regression",
        data = filter(train_data, treatment == 1),
        control = list(nthread = nthread, num_search_rounds = size)
      )
      if(verbose) message(Sys.time(),': ','tuning outcome 0...')
      outcome_0_separate_wf <- tuned_boost_spec(
        formula = outcome_marginal_formula,
        mode = "regression",
        data = filter(train_data, treatment == 0),
        control = list(nthread = nthread, num_search_rounds = size)
      )
      effect_wf <- cvboost_spec(
        formula = effect_formula, 
        mode = "regression",
        control = list(nthread = nthread, num_search_rounds = size))
    }
    tune_time_i <- Sys.time()
    
    #                          __ _ _
    #   ___ _ __ ___  ___ ___ / _(_) |_
    #  / __| '__/ _ \/ __/ __| |_| | __|
    # | (__| | | (_) \__ \__ \  _| | |_
    #  \___|_|  \___/|___/___/_| |_|\__|
    
    nuisance_tbl <- crossfit_nuisance(
      data = train_data,
      outcome_marginal_wf = outcome_marginal_wf,
      outcome_single_wf = outcome_single_wf,
      outcome_1_separate_wf = outcome_1_separate_wf,
      outcome_0_separate_wf = outcome_0_separate_wf,
      treatment_wf = treatment_wf,
      min_prob = min_prob,
      v = v,
      cf_order = cf_order,
      verbose = verbose
    )
    crossfit_time_i <- Sys.time()
    
    # Store nuisance MSE
    sim_df$crossfit_outcome_marginal_mse[i] = with(
      nuisance_tbl,
      mean((outcome - .pred_outcome_marginal)^2)
    )
    sim_df$crossfit_outcome_1_separate_mse[i] = with(
      filter(nuisance_tbl, treatment == 1),
      mean((outcome - .pred_outcome_1_separate)^2)
    )
    sim_df$crossfit_outcome_0_separate_mse[i] = with(
      filter(nuisance_tbl, treatment == 0),
      mean((outcome - .pred_outcome_0_separate)^2)
    )
    sim_df$crossfit_outcome_1_single_mse[i] = with(
      filter(nuisance_tbl, treatment == 1),
      mean((outcome - .pred_outcome_1_single)^2)
    )
    sim_df$crossfit_outcome_0_single_mse[i] = with(
      filter(nuisance_tbl, treatment == 0),
      mean((outcome - .pred_outcome_0_single)^2)
    )
    sim_df$crossfit_treatment_nll[i] = 
      mean(-log(dbinom(as.numeric(nuisance_tbl$treatment==1), 1, nuisance_tbl$.pred_treatment)))
    with(nuisance_tbl,
      mean((as.numeric(treatment==1) - .pred_treatment)^2)
    )
    with(nuisance_tbl,
         mean((as.numeric(treatment==0) - .pred_control)^2)
    )
      # mean(-log(
      #   (nuisance_tbl$treatment==1)*nuisance_tbl$.pred_treatment +
      #   (nuisance_tbl$treatment==0)*(1-nuisance_tbl$.pred_treatment)
      #   ))
    
    #  ____   ___  ____
    # |  _ \ / _ \|  _ \
    # | |_) | | | | |_) |
    # |  __/| |_| |  _ <
    # |_|    \___/|_| \_\
    
    mse_i <- mse_NA
    
    for(j in 1:nrow(mse_i)){
      if(verbose) message(Sys.time(),': ', mse_i$pseudo[j],', ', mse_i$weights[j])
      if (mse_i$pseudo[j] == 'T') {
        fitted_j <- t_learner(
          data = train_data,
          outcome_1_wf = outcome_1_separate_wf,
          outcome_0_wf = outcome_0_separate_wf
        )
      } else {

        fitted_j <- fit_wpor(
          data = train_data,
          nuisance_tbl = nuisance_tbl,
          effect_wf = effect_wf,
          pseudo_fun = mse_i$pseudo[j],
          weight_fun = mse_i$weights[j],
          standardize_weights = FALSE,
          verbose = FALSE,
          cf_order = cf_order
        )
      }
      
      
      pred <- predict_expected_value(fitted_j, test_data)
      mse_i$pred_mse[j] <- mean( ( pred - test_list$params$tau )^2 )
      mse_i$ate_error[j] =  mean(pred) - mean(test_list$params$tau)

    }
    stopifnot(all(dim(mse_NA) == dim(mse_i)))
    por_time_i <- Sys.time()
    
    sim_df$mse[[i]] <- mse_i
    sim_df$tune_time[i] <- difftime(tune_time_i, start_i, units = 'min')
    sim_df$crossfit_time[i] <- difftime(crossfit_time_i, tune_time_i, units = 'min')
    sim_df$por_time[i] <- difftime(por_time_i, crossfit_time_i, units = 'min')
    setTimerProgressBar(pb, i)
  }
  
  
  
  sim_df
}

