library(tidyverse)
library(latex2exp)

job_path <- '2024-02-26_ate_rfgbm_500'

slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/')
results <- readRDS(paste0(slurm_dir, 'combined_results.rds'))
str(readRDS(paste0(slurm_dir, 'more_args.RDS')))

sum_results <- results %>%
  filter(!is.na(setup)) %>%
  tidyr::unnest(mse) %>% 
  mutate(pseudo = ifelse(pseudo=='pseudo_DR_single', 'DR', pseudo)) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_cov', 'Cov', pseudo)) %>%
  pivot_longer(
    cols = c(
      'pred_mse',
      'avg_pred_ate_se',
      'weighted_avg_PO_ate_se'),
    names_to = 'estimator_type',
    values_to = 'sq_err'
  ) %>% 
  filter(!is.na(sq_err)) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_U', 'U', pseudo)) %>%
  mutate(
    pseudo = factor(
      pseudo, 
      levels = c('DR','Cov', 'pseudo_DR_separate', 'U', 'T')),
    estimand = ifelse(
      estimator_type == 'pred_mse',
      'CATE', 'ATE'),
    ate_estimator_type = factor(
      estimator_type,
      levels = c('avg_pred_ate_se','weighted_avg_PO_ate_se'),
      labels = c('prediction avg', 'PO avg'))
  ) %>%
  group_by(
    learners, n_obs, p, sigma, setup, nuisance, 
    pseudo, weights, estimand, ate_estimator_type, estimator_type
    ) %>%
  summarize(
    mse = mean(sq_err),
    n_sim = n(),
    std_err = sd(sq_err)/sqrt(n()),
    lower = pmax(0.0001, mse - qnorm(0.975) * std_err),
    upper = mse + qnorm(0.975) * std_err) %>%
  mutate(
    n_obs = as.numeric(n_obs)
    )

sum_cate <- filter(sum_results, estimand == 'CATE')
sum_ate <- filter(sum_results, estimand == 'ATE')

my_learner = 'lightgbm'
#my_learner = 'parsnip_random_forest'


mypdf <- function(file, ...){
  pdf(file= paste0('plots/',Sys.Date(),'_', file,'.pdf'), ...)
}


#https://uliniemann.com/blog/2022-02-21-math-annotations-in-ggplot2-with-latex2exp/
#TeX(r"($(A-\hat{\pi}(X))^2$ )")

# weight_labels = 
#   list('weight_1' = 1,
#        'weight_U_AX' = expression('Var(U|A,X)'^{-1}),
#        'weight_U_X' = expression('Var(U|X)'^{-1}),
#        'weight_DR_AX' = expression('Var(DR|A,X)'^{-1}),
#        'weight_DR_X' = expression('Var(DR|X)'^{-1}))
weight_simple_labels = 
  list('weight_1' = 1,
       'weight_U_AX' = TeX(r"($(\textit{A}-\hat{\pi}(\textit{X}))^2$ )"),
       'weight_DR_X' = TeX(r"(\hat{\pi}(\textit{X})\,(1-\hat{\pi}(\textit{X})))"),
       'sample_avg' = 'Sample Avg')




mypdf(paste0('main-comparison_',my_learner), width = 4.5, height = 8.5)

sum_cate %>%
  filter(
         pseudo != 'rlearner_package',
         learners == my_learner,
         pseudo %in% c('DR','Cov','U','T')
         ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
         ) %>%
  ggplot(aes(x = n_obs, y = log(mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)), alpha = .3, col=NA) +
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'bottom')
dev.off()




mypdf(paste0('ate-comparison_',my_learner), width = 7.5, height = 8.5)

sum_ate %>%
  filter(
    pseudo != 'rlearner_package',
    learners == my_learner,
    pseudo %in% c('DR','Cov','U','T')
    ,weights %in% c('weight_1','weight_U_AX','weight_DR_X','sample_avg')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mse),
             lty = ate_estimator_type,
             col = weights, fill = weights)) +
  geom_line() +
  geom_ribbon(aes(
      ymin = log(lower), ymax = log(upper),
      group = paste0(ate_estimator_type, weights)
    ), alpha = .3, col=NA) +
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
  ) +
  theme(legend.position = 'bottom')
dev.off()




mypdf(paste0('poster-comparison_',my_learner,'-E'), width = 5, height =2.5)
sum_cate %>%
  filter(
    pseudo %in% c('DR','U'),
    learners == my_learner,
    setup == 'E'
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)), alpha = .3, col=NA) +
  facet_grid(.~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'right')
dev.off()


weight_expanded_labels = 
  list('weight_1' = 1,
       'weight_U_AX' = TeX(r"($(A-\hat{\pi}^2)$ )"),
       'weight_U_X' = TeX(r"($\frac{(\hat{\pi}\hat{\kappa})^{2}}{\hat{\pi}^{3}+\hat{\kappa}^{3}}$ )"),
       'weight_DR_AX' = TeX(
         r"(\frac{A}{\hat{\pi}^{2}}+\frac{1-A}{\hat{\kappa}^{2}})"
         ),
       'weight_DR_X' = TeX(r"(\hat{\nu})"))



mypdf(paste0('expanded-comparison_',my_learner), width = 6, height = 8.5)
sum_cate %>%
  filter(
    pseudo != 'rlearner_package',
    learners == my_learner,
    pseudo %in% c('DR','U','T')
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X','weight_DR_AX','weight_U_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)), alpha = .3, col=NA) + #!! NARROW !!
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_expanded_labels) +
  scale_colour_discrete('Weights', labels = weight_expanded_labels) +
  labs(
    title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'bottom') 
dev.off()

results_table <- sum_cate %>%
  filter(
    pseudo != 'rlearner_package',
    weights %in% c('weight_1', 'weight_U_AX','weight_DR_X'),
    #(weights != 'weight_1' | pseudo == 'T'),
    learners == my_learner,
    n_obs == 1000) %>%
  ungroup() %>%
  mutate(log_pred_mse_lab = paste0(signif(log(mse), 2), ' (',signif(log(lower), 2), ', ', signif(log(lower), 2),')')) %>%
  select(setup, pseudo, weights, log_pred_mse_lab)

results_table

## Neither U nor DR learner dominates if *separate* models
# are used for E(Y|X,1) and E(Y|X,A), but
# the DR dominates if we use a single model.
sum_cate %>%
  filter(
        learners == my_learner,
         pseudo %in% c('DR','pseudo_DR_separate','U'),
         weights %in% c('weight_U_AX', 'weight_DR_X')) %>% 
  ggplot(aes(x = n_obs, y = log(mse), col = weights, lty = pseudo, fill = weights)) +
  geom_line() + 
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper), col = weights), alpha = .3, col = NA) +
  facet_grid(setup~.)
dev.off()

## T learners does similar to DR on E,F, slightly better on D, but much worse on A,B,C.
sum_cate %>%
  filter( (pseudo == 'DR' & weights == 'weight_DR_X') |
          (pseudo == 'T' & weights == 'weight_1'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mse), col = pseudo, fill = pseudo)) +
  geom_line() + 
  facet_grid(setup~.) +
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper), col = pseudo, group = paste0(weights,pseudo)), alpha = .3, col = NA) 
dev.off()

filter(results_table,
       pseudo %in% c('DR','T')) %>% as.data.frame

### Aaron's cvboost R learner matches rlearner package 
  ## lightgbm is somewhat different though.
sum_cate %>%
  filter(learners %in% c('cvboost','rlearner_package','lightgbm'),
         pseudo %in% c('U','rlearner_package'),
         weights %in% c('weights_U_AX', 'weight_U_AX')) %>% 
  ggplot(aes(x = n_obs, y = (mse), col = learners, lty = learners)) +
  geom_line() + 
  facet_grid(setup~.)
dev.off()


### DR learners generally benefit from weighting by X, though possibly not by AX
sum_cate %>%
  filter(pseudo%in%c('DR','pseudo_DR_separate'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()

### U learners generally do best when weighting by AX, and worst when weighting by 1
sum_cate %>%
  filter(pseudo == 'U', learners == my_learner) %>%
  ggplot(aes(x = n_obs, y =log(mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()

