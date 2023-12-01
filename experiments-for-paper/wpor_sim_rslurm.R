library("dplyr")
library("rslurm")
source('wpor_sim_fun.R')

size = 10
alpha = 0.05
v = 10
burnin = 3
nsim <- 200

#tuned_df <- readRDS('pretune_results.rds')
tuned_df <- NULL
#tuned_df <- readRDS('_rslurm_pretune_weighted/combined_results.rds')
#tuned_df

include_rlearner_comparison <- NULL


simultaneous <- 90

nodes <- nrow(results_tbl)
#(job_path <- paste0(Sys.Date(),'_pretuned_parsnip_and_cv_', nsim))
(job_path <- paste0(Sys.Date(),'_active-tune_', nsim))
#(job_path <- paste0(Sys.Date(),'_crossfit_test_', nsim))
(slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/'))


if(!dir.exists('sbatch-array')){
  dir.create('sbatch-array')
  writeLines(con = 'sbatch-array/.gitignore', text = c(
    '*.out',
    '*.rds',
    '*.txt'))
}

results_tbl <- expand.grid(
  learners = c('parsnip_boost','cvboost'), #'parsnip_random_forest', 'cvboost' 'parsnip_boost'
  n_obs = c(250,500,1000),#, 1000, 250
  p = 6,#c(6,12),
  sigma = 1,#c(0.5, 1, 2),#, 3),
  setup = c('A','B','C','D','E','F'),
  nuisance = c('estimated'),
  seed = 1:nsim,
  tune_time = NA,
  crossfit_time = NA,
  por_time = NA,
  crossfit_outcome_obs_mse = NA,
  crossfit_outcome_1_mse = NA,
  crossfit_outcome_0_mse = NA,
  crossfit_treatment_nll = NA
) %>% 
  tibble() %>%
  mutate(id = 1:n())
results_tbl$mse <- vector('list', nrow(results_tbl))


mse_NA <- expand.grid(
  pseudo = c('pseudo_U','pseudo_DR','T', 'rlearner_package'),
  weights = c(
    'weight_1'
    ,'weight_U_X' 
    ,'weight_U_AX'
    ,'weight_DR_X'
    ,'weight_DR_AX'
    ,'weight_DR_alt_AX'
  ),
  mse = NA,
  stringsAsFactors = FALSE
) %>%
  filter(
    !(pseudo == 'T' & weights != 'weight_1'),
    !(pseudo == 'rlearner_package' & weights != 'weight_U_AX'),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_X'),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_AX'),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_alt_AX'),
    !(pseudo == 'pseudo_DR' & weights == 'weight_U_X'),
    !(pseudo == 'pseudo_DR' & weights == 'weight_U_AX')
  ) %>%
  arrange(pseudo, weights) %>%
  as_tibble()




print(object.size(results_tbl), units = 'auto')

results_list <- list()
for(i in 1:nrow(results_tbl)){
  results_list[[i]] <- results_tbl[i,]
}
dim(results_tbl)
length(results_list)
head(results_list,1)
tail(results_list,1)
mse_NA

if(FALSE){
  #workspace for testing
  s1 <- simulate_from_df(
    sim_df = results_tbl[1:3,],
    pretuned = tuned_df,
    mse_NA = mse_NA,
    verbose = TRUE,
    include_rlearner_comparison = FALSE,
    size = size, alpha = alpha, v = v, burnin = burnin
  )
  s1$tune_time
  s1$crossfit_time
  s1$por_time
  s1$mse
}


{
  sjob <- slurm_map(
    x = results_list, 
    nodes = nodes,
    f = simulate_from_df,
    job_array_task_limit = simultaneous,
    pkgs = c("wpor", "dplyr", "tidymodels", "pbapply", "rlearner", "workflows"),
    jobname = job_path,
    cpus_per_node = 1, 
      # avoid mclapply as it's harder to debug than standard 
      # slurm multi-core parallelization, 
      # and even when you're allocated more cores, it doesn't go faster. 
    slurm_options = list(
      partition = 'M-16Cpu-123GB'
      ,'mail-user'=Sys.getenv('USEREMAIL')
      ,'mail-type'='ALL'
      #,'exclusive'=TRUE
      ,mem = '6G'# mem to be shared across all CPUs on each node.
    ),
    mse_NA = mse_NA,
    size = size, alpha = alpha, v = v, burnin = burnin,
    pretuned = tuned_df,
    include_rlearner_comparison = include_rlearner_comparison
  )
  saveRDS(sjob, paste0(slurm_dir,'sjob.rds'))
  yaml::write_yaml(sjob,  paste0(slurm_dir,'sjob.yaml'))
}


# ## !! pure mclapply doesn't seem to work well! too slow.
# library(parallel)
# (cores <- ceiling(detectCores() *.8))
# cl <- makePSOCKcluster(cores)
# 
# 
# mc_out <- parLapply(
#   cl = cl,
#   X = results_list,
#   fun = simulate_from_df,
#   #size = size,
#   #mc.cores = cores,
#   mse_NA = mse_NA,
#   pretuned = tuned_df,
#   verbose = TRUE
# )
# stopCluster(cl)
# saveRDS(mc_out, 'test_mc.rds')
# 
# mc_out <- mclapply(
#   results_list,
#   simulate_from_df,
#   mc.cores = cores,
#   mse_NA = mse_NA,
#   pretuned = tuned_df,
#   verbose = FALSE
# )
# mc_out <- parLapply(
#   cl = cl,
#   X = list(a=1, b=2), 
#   fun = function(a) a %>% print
# )

job_tbl <- tibble(get_job_status(sjob)[[2]])
job_tbl$time <- NA
job_tbl$time[nchar(job_tbl$TIME) %in% c(4,5)] <- 
  as.difftime(job_tbl$TIME[nchar(job_tbl$TIME) %in% c(3,4)], '%M:%S',
              units = 'min')
job_tbl$time[nchar(job_tbl$TIME) %in% c(7,8)] <- 
  as.difftime(job_tbl$TIME[nchar(job_tbl$TIME)%in% c(7,8)], '%H:%M:%S',units = 'min')
to_cancel <- filter(job_tbl, time > 70)$JOBID
for(jname in to_cancel) system(paste('scancel', jname))
message('cancelled ', length(to_cancel), ' tasks')


# sacct -j 484153 --format jobid%15,AveVMSize,MaxVMsize,ReqCPUS,AllocCPUS,ReqMem,state,Elapsed --units=G 
message('\n-- Resources used by array job: --\n')
system(paste(
  'sacct -j',
  sjob$jobid,
  '--format jobid%15,AveVMSize,MaxVMsize,ReqCPUS,AllocCPUS,ReqMem,state,Elapsed --units=G '
))
## 20231110_parsnip_ivw_100: Req: 120G; Avg: 160. Need fewer cpus per node? None crashed though.

sum(grepl('.RDS', dir(slurm_dir))) # number of completed jobs
# slurm_dir <- "_rslurm_20231127_activetune_200/"
# sjob <- readRDS(paste0(slurm_dir, 'sjob.rds'))
job_out <- get_slurm_out(sjob, "raw", wait = FALSE)
results <- do.call(rbind, job_out)
# results <- data.frame()
# for(i in 1:length(job_out)){
#   results <- rbind(results, job_out[[i]])
# }

saveRDS(results, paste0(slurm_dir, 'combined_results.rds'))
