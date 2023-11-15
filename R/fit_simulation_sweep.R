library(autoOcc)
library(foreach)
library(doParallel)
library(parallel)
library(unmarked)

# pull in simulated data
source("./R/simulation_sweep.R")

# pull in utility functions
source("./R/simulation_utils.R")


# number of different site / 
nt_options <- nrow(sim_list$targets$others)

# fit simulations, save the output, and remove object to clear up RAM
for(r in 1:4){
  cat(
    paste0(
      "\nOcc scenario: ", r,"\n"
    )
  )
  pb <- txtProgressBar(max = nt_options)
  
  for(s in 1:nt_options){
    setTxtProgressBar(pb, s)
    file_name <- paste0(
      "./data/sweep_fits/auto",r,
      "_n",sim_list$targets$others$n[s],
      "_t",sim_list$targets$others$nseason[s],
      ".RDS"
    )
    if(!file.exists(file_name)){
      # fit autologistic
      fit_auto <- fit_sweep(
        data = sim_list$auto[[r]][[s]]$data,
        ncores = 6,
        nsim = sim_list$targets$others$nsim[s]
      )
      saveRDS(
        fit_auto,
        file_name
      )
      rm(fit_auto)
      gc()
    }
    file_name <- paste0(
      "./data/sweep_fits/dynamic",r,
      "_n",sim_list$targets$others$n[s],
      "_t",sim_list$targets$others$nseason[s],
      ".RDS"
    )
    if(!file.exists(file_name)){
    
      fit_dynamic <- fit_sweep(
        data = sim_list$dynamic[[r]][[s]]$data,
        ncores = 6,
        nsim = sim_list$targets$others$nsim[s],
        auto = FALSE
      )
      saveRDS(
        fit_dynamic,
        file_name
      )
      rm(fit_dynamic)
      gc()
    }
  }
}
