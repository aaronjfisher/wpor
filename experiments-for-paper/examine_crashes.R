library(tidyverse)
library(rslurm)

source('wpor_sim_fun.R')
simultaneous <- 50
#job_path <- '2023-11-20_cvboost_active-tune_200'
#job_path <- 'pretune_weighted'
#job_path <- '2023-11-21_parsnip_boost_active-tune_100'
#job_path <- '2023-11-22_pretuned_parsnip_and_cv_200'
#job_path <- '2023-11-23_pretuned_parsnip_and_cv_200'
#job_path <- '2023-11-24_parsnip_boost_active-big-tune_100'
job_path <- "2023-11-27_active-tune_200" 

slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/')
patch_name <- paste0(job_path,'_patch')
patch_dir <- paste0('_rslurm_', gsub('-','', patch_name),'/')

x_full <- readRDS(paste0(slurm_dir,'x.RDS'))
df_full <- do.call(rbind, x_full)
more_args <- readRDS(paste0(slurm_dir,'more_args.RDS'))

completed_df <- readRDS(paste0(slurm_dir, 'combined_results.rds')) %>%
  filter(!is.na(learners))

incomplete_tbl <- filter(
  df_full, 
  !id %in% completed_df$id) 


incomplete_list <- list()
for(i in 1:nrow(incomplete_tbl)){
  incomplete_list[[i]] <- incomplete_tbl[i,]
}

dim(completed_df)
length(incomplete_list)
head(incomplete_list,1)
tail(incomplete_list,1)

{
  stopifnot(!dir.exists((patch_dir)))
  sjob <- do.call(slurm_map, c(list(
    x = incomplete_list, 
    f = simulate_from_df,
    nodes = length(incomplete_list),
    job_array_task_limit = simultaneous,
    pkgs = c("wpor", "dplyr", "tidymodels", "pbapply", "rlearner"),
    jobname = patch_name,
    slurm_options = list(
      partition = 'M-16Cpu-123GB',
      'mail-user'=Sys.getenv('USEREMAIL'),
      'mail-type'='ALL'
      #,'exclusive'=TRUE
    ),
    cpus_per_node = 1
  ),
  more_args
  ))
  saveRDS(sjob, paste0(patch_dir,'sjob.rds'))
  yaml::write_yaml(sjob, paste0(patch_dir,'sjob.yaml'))
}

# test <- do.call(simulate_from_df, c(list(
#   incomplete_list[[1]]),
#   more_args
# ))


get_job_status(sjob)[1:2] # this can fail if other jobs with the same name are in queue


#sjob <- readRDS(paste0(patch_dir, 'sjob.rds'))
job_out <- get_slurm_out(sjob, "raw", wait = FALSE)
results2 <- do.call(rbind, job_out) %>%
  rbind(., completed_df) %>% 
  filter(!is.na(learners)) %>%
  arrange(as.numeric(id))
stopifnot(all(!is.na(results2$learners)))

saveRDS(results2, paste0(slurm_dir, 'combined_results.rds'))
# unlink(patch_dir, recursive = TRUE)

