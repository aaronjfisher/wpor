library(tidyverse)

# job_path <- '2023-11-19_pretuned_cvboost_100'
# job_path <- '2023-11-20_cvboost_active-tune_200'
# job_path <- '2023-11-24_parsnip_boost_active-tune_100'
# job_path <- '2023-11-23_pretuned_parsnip_and_cv_200'
#job_path <- '2023-11-27_parsnip_boost_active-tune_mini-test_100'
job_path <- '2023-11-27_active-tune_200'
slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/')
results <- readRDS(paste0(slurm_dir, 'combined_results.rds'))
str(readRDS(paste0(slurm_dir, 'more_args.RDS')))

sum_results <- results %>%
  filter(!is.na(setup)) %>%
  tidyr::unnest(mse) %>%
  group_by(
    n_obs, learners, p, 
    sigma, setup, nuisance, 
    pseudo, weights) %>%
  summarize(mean_mse = mean(mse),
            n_sim = n(),
            se_mse = mean(mse)/sqrt(n()),
            lower_mse = mean_mse - qnorm(0.975) * se_mse,
            upper_mse = mean_mse + qnorm(0.975) * se_mse) %>%
  mutate(n_obs = as.numeric(n_obs))


#my_learner <- 'parsnip_random_forest'
#my_learner <- 'parsnip_boost'
my_learner <- 'cvboost'

sum_results %>%
  filter(
         pseudo != 'rlearner_package',
         learners == my_learner,
         weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), 
             col = weights, fill = weights,
             lty = learners)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse)), alpha = .3, col=NA) +
  facet_grid(setup~pseudo) + theme_bw() +
  labs(
    title = paste('Log MSE on Simulated Data -',my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  )

sum_results %>%
  filter(
    pseudo != 'rlearner_package',
    weights %in% c('weight_1', 'weight_U_AX','weight_DR_X'),
    #(weights != 'weight_1' | pseudo == 'T'),
    n_obs == 1000) %>%
  ungroup() %>%
  mutate(mse_lab = paste0(signif(mean_mse, 2), ' (',signif(lower_mse, 2), ', ', signif(upper_mse, 2),')')) %>%
  select(setup, pseudo, weights, mse_lab)


## Neither U nor DR learner dominates
sum_results %>%
  filter(
        learners == my_learner,
         pseudo %in% c('pseudo_DR','pseudo_U'),
         weights %in% c('weight_U_AX', 'weight_DR_X')) %>% 
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, lty = pseudo, fill = weights)) +
  geom_line() + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse), col = weights), alpha = .3, col = NA) +
  facet_grid(setup~.)


## T learners does similar to DR on D,E,F, but much worse on A,B,C.
sum_results %>%
  filter(pseudo %in% c('pseudo_DR','T'),
         learners ==my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), lty = weights, col = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)

## T learners does similar to U on D,E,F, but much worse on A,B,C.
sum_results %>%
  filter(pseudo %in% c('pseudo_U','T')) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), lty = weights, col = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)


### Aaron's R learner matches rlearner package
sum_results %>%
  filter(learners == 'cvboost', 
         pseudo %in% c('pseudo_U','rlearner_package'),
         weights =='weight_U_AX') %>%
  ggplot(aes(x = n_obs, y = (mean_mse), col = pseudo, lty = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)


### DR learners generally benefit from weighting by X, though possibly not by AX
sum_results %>%
  filter(pseudo == 'pseudo_DR',learners ==my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)

### U learners generally do best when weighting by AX, and worst when weighting by 1
sum_results %>%
  filter(pseudo == 'pseudo_U', learners ==my_learner) %>%
  ggplot(aes(x = n_obs, y =log(mean_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)


