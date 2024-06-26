library("dplyr")
library("rslurm")
source('wpor_sim_fun.R')

size = 25
alpha = 0.05
v = 10
burnin = 5
nsim <- 600
nodes <- 1000


simultaneous <- 50

(job_path <- paste0(Sys.Date(),'_', nsim))

(slurm_dir <- paste0('_rslurm_', gsub('-','', job_path),'/'))



results_tbl <- expand.grid(
  learners = c('lightgbm', 'parsnip_random_forest', 'rlearner_package', 'cvboost'),
  n_obs = c(250,500,1000),
  p = c(10),
  sigma = 1,
  setup = c('A','B','C','D','E','F'),
  nuisance = c('estimated'),
  cf_order = 2,
  seed = 1:nsim,
  tune_time = NA,
  crossfit_time = NA,
  por_time = NA,
  crossfit_outcome_marginal_mse = NA,
  crossfit_outcome_1_separate_mse = NA,
  crossfit_outcome_0_separate_mse = NA,
  crossfit_outcome_1_single_mse = NA,
  crossfit_outcome_0_single_mse = NA,
  crossfit_treatment_nll = NA
) %>% 
  tibble() %>%
  mutate(id = 1:n()) %>%
  filter(seed <= 50 | learners == 'lightgbm')
results_tbl$mse <- vector('list', nrow(results_tbl))

nrow(results_tbl)/nodes

mse_NA <- expand.grid(
  pseudo = c('pseudo_U',
             'pseudo_DR_single',
             'pseudo_DR_separate', 
             'pseudo_cov', 
             'T'),
  weights = c(
    'weight_1'
    ,'weight_U_X'
    ,'weight_U_AX'
    ,'weight_DR_X'
    ,'weight_DR_AX'
    ,'weight_DR_alt_AX' 
  ),
  pred_mse = NA,
  ate_error = NA,
  stringsAsFactors = FALSE
) %>%
  filter(
    !(pseudo == 'T' & weights != 'weight_1'),
    (pseudo != 'pseudo_cov' | weights %in%
       c('weight_DR_X', 'weight_1')),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_X'),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_AX'),
    !(pseudo == 'pseudo_U' & weights == 'weight_DR_alt_AX'),
    !(pseudo == 'pseudo_DR_single' & weights == 'weight_U_X'),
    !(pseudo == 'pseudo_DR_single' & weights == 'weight_U_AX'),
    !(pseudo == 'pseudo_DR_separate' & weights == 'weight_U_X'),
    !(pseudo == 'pseudo_DR_separate' & weights == 'weight_U_AX')
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
  # workspace for testing
  # source('wpor_sim_fun.R')
  s1 <- 
    filter(results_tbl, setup == 'A', seed == 343, n_obs == 500) %>%
      simulate_from_df(
      sim_df = .,
      mse_NA = mse_NA,
      verbose = TRUE,
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
    pkgs = c("wpor", "dplyr", "tidymodels", "pbapply", "rlearner", "workflows", "lightgbm"),
    jobname = job_path,
    cpus_per_node = 96, 
    r_template = 'slurm_map_renv.txt',
    libPaths = .libPaths(),
    slurm_options = list(
      partition = 'M-96Cpu-742GB'
      ,'mail-user'=Sys.getenv('USEREMAIL')
      ,'mail-type'='ALL'
      #,'exclusive'=TRUE
      ,mem = '700G'# mem to be shared across all CPUs on each node.
    ),
    mse_NA = mse_NA,
    size = size, alpha = alpha, v = v, burnin = burnin
  )
  saveRDS(sjob, paste0(slurm_dir,'sjob.rds'))
  yaml::write_yaml(sjob,  paste0(slurm_dir,'sjob.yaml'))
}


message('\n-- Resources used by array job: --\n')
system(paste(
  'sacct -j',
  sjob$jobid,
  '--format jobid%15,AveVMSize,MaxVMsize,ReqCPUS,AllocCPUS,ReqMem,state,Elapsed --units=G '
))

