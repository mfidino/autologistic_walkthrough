library(autoOcc)
library(foreach)
library(doParallel)
library(parallel)

# simulate the data
source("./R/run_simulations.R")

# register cluster

num_cores <- parallel::detectCores() - 2

cl <- parallel::makeCluster(
  num_cores
)

doParallel::registerDoParallel(
  cl
)

parallel::clusterEvalQ(
  cl, 
  library("autoOcc")
)

# make a list to store all of the results
result_list <- vector(
  "list",
  length = length(all_sims)
)


for(i in 1:length(result_list)){
  result_list[[i]] <- list(
    n30 = vector(
      "list",
      length = all_sims[[1]]$par_list$nsim
    ),
    n50 = vector(
      "list",
      length = all_sims[[1]]$par_list$nsim
    )
  )
}
hm <- autoOcc::auto_occ(
  ~x~x,
  y = all_sims[[5]]$n50[[1]]$y,
  det_covs = all_sims[[5]]$n50[[1]]$x,
  occ_covs = all_sims[[5]]$n50[[1]]$x
)

test <- all_sims[[5]]$n50[1:10]



my_iter <- 1:all_sims[[1]]$par_list$nsim

pb <- txtProgressBar(max = length(result_list))

# do loop, and farm out to a bunch of cores
#  for the model fitting.
for(r in 1:length(result_list)){
  setTxtProgressBar(pb, r)
  for(s in 1:2){
    result <- foreach::foreach(i = my_iter) %dopar% {
      autoOcc::auto_occ(
        ~x~x,
        y = all_sims[[r]][[s]][[i]]$y,
        det_covs = all_sims[[r]][[s]][[i]]$x,
        occ_covs = all_sims[[r]][[s]][[i]]$x
      )
    }
    result_list[[r]][[s]] <- result
  }
}

