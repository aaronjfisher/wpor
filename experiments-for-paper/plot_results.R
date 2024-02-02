library(tidyverse)

# job_path <- '2023-11-19_pretuned_cvboost_100'
# job_path <- '2023-11-20_cvboost_active-tune_200'
# job_path <- '2023-11-24_parsnip_boost_active-tune_100'
# job_path <- '2023-11-23_pretuned_parsnip_and_cv_200'
#job_path <- '2023-11-27_parsnip_boost_active-tune_mini-test_100'
# job_path <- '2023-11-27_active-tune_200'
# job_path <- '2023-12-03_active-tune_100'
# job_path <- '2023-12-04_lightgbm_250'
# job_path <- '2024-01-27_lightgbm_250'
#job_path <- '2024-01-31_single_250'
job_path <- '2024-02-02_narrow_400'
slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/')
results <- readRDS(paste0(slurm_dir, 'combined_results.rds'))
str(readRDS(paste0(slurm_dir, 'more_args.RDS')))

sum_results <- results %>%
  filter(!is.na(setup)) %>%
  tidyr::unnest(mse) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_DR_single', 'DR', pseudo)) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_U', 'U', pseudo)) %>%
  mutate(pseudo = factor(
    pseudo, 
    levels = c('DR', 'pseudo_DR_separate', 'U', 'T'))
  ) %>%
  group_by(
    n_obs, learners, p, 
    sigma, setup, nuisance, 
    pseudo, weights) %>%
  summarize(mean_mse = mean(mse),
            n_sim = n(),
            se_mse = sd(mse)/sqrt(n()),
            lower_mse = pmax(0.01, mean_mse - qnorm(0.975) * se_mse),
            upper_mse = mean_mse + qnorm(0.975) * se_mse) %>%
  mutate(
    n_obs = as.numeric(n_obs)
    )

my_learner = 'lightgbm'
#my_learner = 'parsnip_random_forest'


mypdf <- function(file, ...){
  pdf(file= paste0('plots/',Sys.Date(),'_', file,'.pdf'), ...)
}

weight_labels = 
  list('weight_1' = 1,
       'weight_U_AX' = expression('Var(U|A,X)'^{-1}),
       'weight_U_X' = expression('Var(U|X)'^{-1}),
       'weight_DR_AX' = expression('Var(DR|A,X)'^{-1}),
       'weight_DR_X' = expression('Var(DR|X)'^{-1}))


mypdf(paste0('main-comparison_',my_learner), width = 4.5, height = 8.5)
sum_results %>%
  filter(
         pseudo != 'rlearner_package',
         learners == my_learner,
         pseudo %in% c('DR','U','T')
         ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
         ) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse)), alpha = .3, col=NA) +
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_labels) +
  scale_colour_discrete('Weights', labels = weight_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'bottom')
dev.off()

mypdf(paste0('expanded-comparison_',my_learner), width = 6, height = 8.5)
sum_results %>%
  filter(
    pseudo != 'rlearner_package',
    learners == my_learner,
    pseudo %in% c('DR','U','T')
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X','weight_DR_AX','weight_U_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  #geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse)), alpha = .3, col=NA) + #!! NARROW !!
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_labels) +
  scale_colour_discrete('Weights', labels = weight_labels) +
  labs(
    title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'bottom') 
dev.off()

results_table <- sum_results %>%
  filter(
    pseudo != 'rlearner_package',
    weights %in% c('weight_1', 'weight_U_AX','weight_DR_X'),
    #(weights != 'weight_1' | pseudo == 'T'),
    learners == my_learner,
    n_obs == 1000) %>%
  ungroup() %>%
  mutate(log_mse_lab = paste0(signif(log(mean_mse), 2), ' (',signif(log(lower_mse), 2), ', ', signif(log(upper_mse), 2),')')) %>%
  select(setup, pseudo, weights, log_mse_lab)

results_table

## Neither U nor DR learner dominates if *separate* models
# are used for E(Y|X,1) and E(Y|X,A), but
# the DR dominates if we use a single model.
sum_results %>%
  filter(
        learners == my_learner,
         pseudo %in% c('DR','pseudo_DR_separate','U'),
         weights %in% c('weight_U_AX', 'weight_DR_X')) %>% 
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, lty = pseudo, fill = weights)) +
  geom_line() + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse), col = weights), alpha = .3, col = NA) +
  facet_grid(setup~.)
dev.off()

## T learners does similar to DR on E,F, slightly better on D, but much worse on A,B,C.
sum_results %>%
  filter(pseudo %in% c('DR','T'),
         weights %in% c('weight_1','weight_DR_X'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), lty = weights, col = pseudo)) +
  geom_line() + 
  facet_grid(setup~.) +
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse), col = pseudo, group = paste0(weights,pseudo)), alpha = .3, col = NA) 
dev.off()

filter(results_table,
       pseudo %in% c('DR','T')) %>% as.data.frame

### Aaron's cvboost R learner matches rlearner package 
  ## lightgbm is somewhat different though.
sum_results %>%
  filter(learners %in% c('cvboost','rlearner_package','lightgbm'),
         pseudo %in% c('U','rlearner_package'),
         weights %in% c('weights_U_AX', 'weight_U_AX')) %>% 
  ggplot(aes(x = n_obs, y = (mean_mse), col = learners, lty = learners)) +
  geom_line() + 
  facet_grid(setup~.)
dev.off()


### DR learners generally benefit from weighting by X, though possibly not by AX
sum_results %>%
  filter(pseudo%in%c('DR','pseudo_DR_separate'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()

### U learners generally do best when weighting by AX, and worst when weighting by 1
sum_results %>%
  filter(pseudo == 'U', learners == my_learner) %>%
  ggplot(aes(x = n_obs, y =log(mean_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()

