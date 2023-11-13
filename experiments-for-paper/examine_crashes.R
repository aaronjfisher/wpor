library(tidyverse)
library(rslurm)

source('define-simulation.R')
source('wpor_sim_fun.R')

job_path <- '2023-11-08'
results_completed <- readRDS(paste0(job_path, '_combined_results.rds')) %>%
  filter(!is.na(learners))

incomplete_tbl <- filter(
  results_full, 
  !id %in% results_completed$id)


incomplete_list <- list()
for(i in 1:nrow(incomplete_tbl)){
  incomplete_list[[i]] <- incomplete_tbl[i,]
}

length(incomplete_list)
head(incomplete_list,1)
tail(incomplete_list,1)

sjob <- slurm_map(
  x = incomplete_list, 
  nodes = 40, # doesn't include multi-threading; see cpus_per_node argument
  f = simulate_from_df,
  pkgs = c("wpor", "dplyr", "tidymodels", "pbapply", "rlearner"),
  jobname = paste0(job_path,'_ivw_patch_', nsim),
  slurm_options = list(
    partition = 'M-16Cpu-123GB',
    'mail-user'=Sys.getenv('USEREMAIL'),
    'mail-type'='ALL'
  ),
  verbose = TRUE,
  cpus_per_node = 16
)



get_job_status(sjob)[1:2] # this can fail if other jobs with the same name are in queue
saveRDS(sjob, paste0(job_path,'_sjob_patch.rds'))
yaml::write_yaml(sjob, paste0(job_path,'_sjob_patch.yaml'))


#sjob <- readRDS(paste0(job_path, '_sjob_patch.rds'))
job_out <- get_slurm_out(sjob, "raw", wait = FALSE)
results2 <- do.call(rbind, job_out) %>%
  mutate(time = crossfit_time + por_time + tune_time) %>% 
  relocate(time, .after = seed) %>%
  select(-c('crossfit_time', 'por_time', 'tune_time')) %>%
  rbind(., results_completed) %>% 
  filter(!is.na(learners)) %>%
  arrange(as.numeric(id))
stopifnot(all(!is.na(results2$learners)))

saveRDS(results2, paste0(job_path, '_combined_results.rds'))
