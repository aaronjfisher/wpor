library(tidyverse)
library(rslurm)

job_path <- '2023-11-08'
results <- readRDS(paste0(job_path,'_combined_results.rds'))


sum_results <- results %>%
  filter(!is.na(setup)) %>%
  tidyr::unnest(mse) %>%
  group_by(
    n_obs, learners, p, 
    sigma, setup, nuisance, 
    pseudo, weights) %>%
  summarize(mean_mse = mean(mse),
            se_mse = mean(mse)/sqrt(n()),
            lower_mse = mean_mse - qnorm(0.975) * se_mse,
            upper_mse = mean_mse + qnorm(0.975) * se_mse) %>%
  mutate(n_obs = as.numeric(n_obs))

sum_results %>%
  filter(setup == 'A') %>%
  ggplot(aes(x = n_obs, y = mean_mse, col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = lower_mse, ymax = upper_mse), alpha = .3, col=NA) +
  facet_grid(pseudo~learners)

sum_results %>%
  filter(learners == 'boost', pseudo != 'rlearner_package') %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(group = weights, ymin = log(lower_mse), ymax = log(upper_mse)), alpha = .3, col=NA) +
  facet_grid(setup~pseudo)

sum_results %>%
  filter(learners == 'boost',
         pseudo != 'rlearner_package',
         weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse)), alpha = .3, col=NA) +
  facet_grid(setup~pseudo)


## U vs DR: usually equal, U does better on setup C
sum_results %>%
  filter(learners == 'boost',
         pseudo %in% c('pseudo_DR','pseudo_U'),
         weights %in% c('weight_U_AX', 'weight_DR_X')) %>% 
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, lty = pseudo, fill = weights)) +
  geom_line() + 
  geom_ribbon(aes(ymin = log(lower_mse), ymax = log(upper_mse), col = weights), alpha = .3, col = NA) +
  facet_grid(setup~.)


## T learners do worse than DR learners, except when there is no smoothness
sum_results %>%
  filter(learners == 'boost', pseudo %in% c('pseudo_DR','T')) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, lty = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)

## T learners do worse than U learners, except when there is no smoothness
sum_results %>%
  filter(learners == 'boost', pseudo %in% c('pseudo_U','T')) %>%
  ggplot(aes(x = n_obs, y = log(mean_mse), col = weights, lty = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)


### Aaron's R learner matches rlearner package
sum_results %>%
  filter(learners == 'boost', 
         pseudo %in% c('pseudo_U','rlearner_package'),
         weights =='weight_U_AX') %>%
  ggplot(aes(x = n_obs, y = (mean_mse), col = pseudo, lty = pseudo)) +
  geom_line() + 
  facet_grid(setup~.)


### DR learners always benefit from weighting by X, though possibly not by AX
sum_results %>%
  filter(learners == 'boost', pseudo == 'pseudo_DR') %>%
  ggplot(aes(x = n_obs, y = mean_mse, col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)

### U learners always do best when weighting by AX, and worst when weighting by 1
sum_results %>%
  filter(learners == 'boost', pseudo == 'pseudo_U') %>%
  ggplot(aes(x = n_obs, y = mean_mse, col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)


