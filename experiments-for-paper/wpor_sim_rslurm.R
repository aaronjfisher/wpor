library("rslurm")
source('wpor_sim_fun.R')
source('define-simulation.R')
tuned_df <- NULL# readRDS('tuned_df.rds')

sequential_length = 4
cpus_per_node = 16
# sequential_length * cpus_per_node * 90
job_path <- '2023-11-10_pretune'

nrow(results_full) / (cpus_per_node * sequential_length)
print(object.size(results_full), units = 'auto')

cuts <- cut(
  1:nrow(results_full),
  breaks = seq(0,nrow(results_full),by = sequential_length)
)
results_list <- list()
for(cl in levels(cuts)){
  results_list[[cl]] <- results_full[cuts == cl,]
}
dim(results_full)
length(results_list)
head(results_list,1)
tail(results_list,1)

sjob <- slurm_map(
  x = results_list, 
  nodes = min(nsim, 80), # doesn't include multi-threading; see cpus_per_node argument
  f = simulate_from_df,
  pkgs = c("wpor", "dplyr", "tidymodels", "pbapply", "rlearner"),
  jobname = paste0(job_path,'_ivw_', nsim),
  slurm_options = list(
    partition = 'M-96Cpu-742GB',
    'mail-user'=Sys.getenv('USEREMAIL'),
    'mail-type'='ALL'#,
    #mem = '5G'# mem to be shared across all CPUs on each node.
  ),
  verbose = TRUE,
  pretuned = tuned_df,
  cpus_per_node = cpus_per_node
)
# simulate_from_df(tail(results_list,1)[[1]], pretuned = tuned_df, verbose = TRUE,
#                  nthread = parallel::detectCores())

## !! pure mclapply doesn't seem to work well! too slow.
# library(parallel)
# mc_out <- mclapply(
#   X = as.list(1:nsim), 
#   FUN = slurm_fun,
#   mc.cores = min(detectCores(), nsim)
# )


get_job_status(sjob)[1:2] # this can fail if other jobs with the same name are in queue


saveRDS(sjob, paste0(job_path,'_sjob.rds'))
yaml::write_yaml(sjob, paste0(job_path,'_sjob.yaml'))


message('\n-- Resources used by array job: --\n')
system(paste(
  'sacct -j',
  sjob$jobid,
  '--format jobid%15,AveVMSize,MaxVMsize,ReqCPUS,AllocCPUS,ReqMem,state,Elapsed --units=G '
))

# job_path <- '2023-11-07'
# sjob <- readRDS(paste0(job_path, '_sjob.rds'))
job_out <- get_slurm_out(sjob, "raw", wait = FALSE)
results <- do.call(rbind, job_out)
# results <- data.frame()
# for(i in 1:length(job_out)){
#   results <- rbind(results, job_out[[i]])
# }

saveRDS(results, paste0(job_path, '_combined_results.rds'))
