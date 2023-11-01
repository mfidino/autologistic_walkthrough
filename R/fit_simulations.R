library(autoOcc)
library(foreach)
library(doParallel)
library(parallel)

# simulate the data
source("./R/run_simulations.R")

# load function to fit model
source("./R/simulation_utils.R")

# fit simulations, save the output, and remove object to clear up RAM
pb <- txtProgressBar(max = length(all_sims))
for(r in 1:length(all_sims)){
  setTxtProgressBar(pb, r)
  for(s in 1:2){
    fit <- fit_simulations(
      data = all_sims[[r]][[s]],
      ncores = 10,
      nsim = all_sims[[r]]$par_list$nsim
    )
    saveRDS(
      fit,
      paste0(
        "./data/",
        all_sims[[r]]$type,
        "_",
        c("_30sites", "_50sites")[s],
        ".RDS"
      )
    )
    rm(fit)
    gc()
  }
}
