library(tidyverse)
library(latex2exp)

job_path <- '2024-05-23_600'
slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/')

#### Combine & re-save results
sum(grepl('.RDS', dir(slurm_dir))) - 3 # number of completed jobs
sjob <- readRDS(paste0(slurm_dir, 'sjob.rds'))
job_out <- rslurm::get_slurm_out(sjob, "raw", wait = FALSE)
results <- do.call(rbind, job_out)

saveRDS(results, paste0(slurm_dir, 'combined_results.rds'))
####

#results <- readRDS(paste0(slurm_dir, 'combined_results.rds'))
str(readRDS(paste0(slurm_dir, 'more_args.RDS')))

n_errors <- sum(is.na(results$setup))
if(n_errors>0){
  x_list <- readRDS(paste0(slurm_dir, 'x.RDS'))
  bad_x <- x_list[which(is.na(results$setup))] %>%
    do.call(rbind, .)
  stopifnot(all(bad_x$learners == 'cvboost'))
  warning(n_errors, ' jobs using cvboost produced errors')
    # cvboost seems to cause errors, perhaps due to memory issues?
    # the results reported in the paper instead use lightgbm.
}

sum_results <- results %>%
  filter(!is.na(setup)) %>%
  tidyr::unnest(mse) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_DR_single', 'DR', pseudo)) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_cov', 'Cov', pseudo)) %>%
  mutate(pseudo = ifelse(pseudo=='pseudo_U', 'U', pseudo)) %>%
  mutate(pseudo = factor(
    pseudo, 
    levels = c('DR','Cov', 'pseudo_DR_separate', 'U', 'T'))
  ) %>%
  group_by(
    n_obs, learners, p, 
    sigma, setup, nuisance, 
    pseudo, weights) %>%
  summarize(mean_pred_mse = mean(pred_mse),
            n_sim = n(),
            se_pred_mse = sd(pred_mse)/sqrt(n()),
            min_mse = min(pred_mse),
            max_mse = max(pred_mse),
            lower_pred_mse = pmax(0.0001, mean_pred_mse - qnorm(0.975) * se_pred_mse),
            upper_pred_mse = mean_pred_mse + qnorm(0.975) * se_pred_mse) %>%
  mutate(
    n_obs = as.numeric(n_obs)
    )




my_learner = 'lightgbm'
#my_learner = 'parsnip_random_forest'


mypdf <- function(file, ...){
  pdf(file= paste0('plots/',Sys.Date(),'_', file,'_',job_path,'.pdf'), ...)
}


#https://uliniemann.com/blog/2022-02-21-math-annotations-in-ggplot2-with-latex2exp/
#TeX(r"($(A-\hat{\pi}(X))^2$ )")

weight_simple_labels = 
  list('weight_1' = 1,
       'weight_U_AX' = TeX(r"($(\textit{A}-\hat{\pi}(\textit{X}))^2$ )"),
       'weight_DR_X' = TeX(r"(\hat{\pi}(\textit{X})\,(1-\hat{\pi}(\textit{X})))"))




mypdf(paste0('main-comparison_',my_learner), width = 5, height = 8.5)

sum_results %>%
  filter(
         pseudo != 'rlearner_package',
         learners == my_learner,
         pseudo %in% c('DR','Cov','U','T')
         ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
         ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) +
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



mypdf(paste0('poster-comparison_',my_learner,'-E'), width = 5, height =2.5)
sum_results %>%
  filter(
    pseudo %in% c('DR','U'),
    learners == my_learner,
    setup == 'E'
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) +
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
         r"($\frac{A}{\hat{\pi}^{2}}+\frac{1-A}{\hat{\kappa}^{2}}$)"
         ),
       'weight_DR_X' = TeX(r"(\hat{\nu})"))



mypdf(paste0('expanded-comparison_',my_learner), width = 6, height = 8.5)
sum_results %>%
  filter(
    pseudo != 'rlearner_package',
    learners == my_learner,
    pseudo %in% c('DR','Cov','U','T')
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X','weight_DR_AX','weight_U_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) + #!! NARROW !!
  facet_grid(setup~pseudo) + theme_bw() +
  scale_fill_discrete('Weights', labels = weight_expanded_labels) +
  scale_colour_discrete('Weights', labels = weight_expanded_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size (n)',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'bottom') 
dev.off()


mypdf(paste0('slides-RL-',my_learner,'-F_',job_path), width = 4, height = 3)
sum_results %>%
  filter(
    pseudo == 'U',
    learners == my_learner,
    setup == 'F'
    ,weights %in% c('weight_1', 'weight_U_AX')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) +
  theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'right')
dev.off()


mypdf(paste0('slides-all-',my_learner,'-F_',job_path), width = 4, height = 6)
sum_results %>%
  filter(
    pseudo %in% c('DR','Cov','U'),
    learners == my_learner,
    setup == 'F'
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) +
  facet_grid(pseudo~.)+
  theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'right')
dev.off()

mypdf(paste0('slides-all-',my_learner,'-A:F_',job_path), width = 12, height = 5.5)
sum_results %>%
  filter(
    pseudo %in% c('DR','Cov','U'),
    learners == my_learner,
    #setup !='F'
    ,weights %in% c('weight_1', 'weight_U_AX','weight_DR_X')
  ) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), 
             col = weights, fill = weights)) +
  geom_line(aes(group = weights)) + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse)), alpha = .3, col=NA) +
  facet_grid(pseudo~setup)+
  theme_bw() +
  scale_fill_discrete('Weights', labels = weight_simple_labels) +
  scale_colour_discrete('Weights', labels = weight_simple_labels) +
  labs(
    #title = paste('Log MSE on Simulated Data -', my_learner),
    x= 'Sample size',
    y= 'Log(mean squared error)'
  ) +
  theme(legend.position = 'right')
dev.off()









results_table <- sum_results %>%
  filter(
    pseudo != 'rlearner_package',
    weights %in% c('weight_1', 'weight_U_AX','weight_DR_X'),
    #(weights != 'weight_1' | pseudo == 'T'),
    learners == my_learner,
    n_obs == 1000) %>%
  ungroup() %>%
  mutate(log_pred_mse_lab = paste0(signif(log(mean_pred_mse), 2), ' (',signif(log(lower_pred_mse), 2), ', ', signif(log(upper_pred_mse), 2),')')) %>%
  select(setup, pseudo, weights, log_pred_mse_lab)

results_table

## Neither U nor DR learner dominates if *separate* models
# are used for E(Y|X,1) and E(Y|X,A), but
# the DR dominates if we use a single model.
sum_results %>%
  filter(
        learners == my_learner,
         pseudo %in% c('DR','pseudo_DR_separate','U'),
         weights %in% c('weight_U_AX', 'weight_DR_X')) %>% 
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), col = weights, lty = pseudo, fill = weights)) +
  geom_line() + 
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse), col = weights), alpha = .3, col = NA) +
  facet_grid(setup~.)
dev.off()

## T learners does similar to DR on E, slightly better on D & F, but much worse on A,B,C.
sum_results %>%
  filter( (pseudo == 'DR' & weights == 'weight_DR_X') |
          (pseudo == 'T' & weights == 'weight_1'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), col = pseudo, fill = pseudo)) +
  geom_line() + 
  facet_grid(setup~.) +
  geom_ribbon(aes(ymin = log(lower_pred_mse), ymax = log(upper_pred_mse), col = pseudo, group = paste0(weights,pseudo)), alpha = .3, col = NA) 
dev.off()

filter(results_table,
       pseudo %in% c('DR','T')) %>% as.data.frame

### Aaron's cvboost R learner matches rlearner package 
  ## lightgbm is somewhat different though.
sum_results %>%
  filter(learners %in% c('cvboost','rlearner_package','lightgbm'),
         pseudo %in% c('U','rlearner_package'),
         weights %in% c('weights_U_AX', 'weight_U_AX')) %>% 
  ggplot(aes(x = n_obs, y = (mean_pred_mse), col = learners, lty = learners)) +
  geom_line() + 
  facet_grid(setup~.)
dev.off()


### DR learners generally benefit from weighting by X, though possibly not by AX
sum_results %>%
  filter(pseudo%in%c('DR','pseudo_DR_separate'),
         learners == my_learner) %>%
  ggplot(aes(x = n_obs, y = log(mean_pred_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()

### U learners generally do best when weighting by AX, and worst when weighting by 1
sum_results %>%
  filter(pseudo == 'U', learners == my_learner) %>%
  ggplot(aes(x = n_obs, y =log(mean_pred_mse), col = weights)) +
  geom_line() + 
  facet_grid(pseudo~setup)
dev.off()



## Workspace

filter(results, learners == 'lightgbm') %>%
  tidyr::unnest(mse) %>%
  filter(weights != 'weight_1') %>% 
  as.data.frame %>% 
  filter(pred_mse > 5)
