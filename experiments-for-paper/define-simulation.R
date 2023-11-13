# s0 <- slurm_fun(0)
# gc()
nsim <- 100

results_full <- expand.grid(
  learners = c('random_forest', 'boost'),
  n_obs = c(1000, 500, 250),
  p = 6,#c(6,12),
  sigma = 1,#c(0.5, 1, 2),#, 3),
  setup = c('A','B','C','D','E','F'),
  nuisance = c('estimated'),
  seed = 1:nsim,
  tune_time = NA,
  crossfit_time = NA,
  por_time = NA
) %>% 
  tibble() %>%
  mutate(id = 1:n())
results_full$mse <- vector('list', nrow(results_full))

