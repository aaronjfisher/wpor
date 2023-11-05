library("rslurm")
source('wpor_sim_fun.R')

# s0 <- slurm_fun(0)
# gc()
nsim <- 100
sequential_length = 5
cpus_per_node = 16
# sequential_length * cpus_per_node * 90

results_full <- expand.grid(
  learners = c('random_forest','boost'),
  n_obs = c(1000, 500, 250),
  p = 6,#c(6,12),
  sigma = 1,#c(0.5, 1, 2),#, 3),
  setup = c('A','B','C','D','E','F'),
  nuisance = c('estimated'),
  seed = 1:nsim
) %>% 
  tibble() %>%
  mutate(id = 1:n())
results_full$mse <- vector('list', nrow(results_full))

nrow(results_full) / (cpus_per_node * sequential_length)

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
  jobname = paste0(Sys.Date(),'_ivw_', nsim),
  slurm_options = list(
    partition = 'M-96Cpu-742GB',
    'mail-user'=Sys.getenv('USEREMAIL'),
    'mail-type'='ALL'#,
    #mem = '5G'# mem to be shared across all CPUs on each node.
  ),
  cpus_per_node = cpus_per_node
)
#simulate_from_df(results_list[[1]])

## !! pure mclapply doesn't seem to work well! too slow.
# library(parallel)
# mc_out <- mclapply(
#   X = as.list(1:nsim), 
#   FUN = slurm_fun,
#   mc.cores = min(detectCores(), nsim)
# )


get_job_status(sjob)[1:2] # this can fail if other jobs with the same name are in queue

saveRDS(sjob, paste0(Sys.Date(),'_big_sjob.rds'))
yaml::write_yaml(sjob, paste0(Sys.Date(),'_big_sjob.yaml'))

#sjob <- readRDS('2023-10-26_big_sjob.rds')
#get_job_status(sjob)[1:2] # this can fail if other jobs with the same name are in queue
#cancel_slurm(sjob)
#show_current_warnings() 

message('\n-- Resources used by array job: --\n')
system(paste(
  'sacct -j',
  sjob$jobid,
  '--format jobid%15,AveVMSize,MaxVMsize,ReqCPUS,AllocCPUS,ReqMem,state,Elapsed --units=G '
))

job_out <- get_slurm_out(sjob, "raw", wait = FALSE)
results <- do.call(rbind, job_out)
head(results)

lapply(job_out, dim)
job_out[[48]]
