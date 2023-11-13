#unloadNamespace('wpor')
#devtools::load_all('..')
library(dplyr)
library(wpor)
source('define-simulation.R')
source('wpor_sim_fun.R')


tune_df <- results_full %>% 
  filter(seed == 1) %>%
  mutate(seed = -100)
tune_df

tuned_df <- simulate_from_df(
  tune_df,
  nthread = parallel::detectCores(), 
  verbose = FALSE,
  return_specifications_only = TRUE,
  include_rlearner_comparison = FALSE
  #pretuned = tuned_df
  )

saveRDS(tuned_df, 'tuned_df.rds')
#tuned_df <- readRDS('tuned_boost_df.rds')

print(object.size(tune_df), units = 'Mb')
print(object.size(tuned_df), units = 'Mb')


