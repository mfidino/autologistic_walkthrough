fit_simulations <- function(data, ncores = 10, nsim){
  num_cores <- 10
  
  cl <- parallel::makeCluster(
    ncores
  )
  
  doParallel::registerDoParallel(
    cl
  )
  
  parallel::clusterEvalQ(
    cl, 
    library("autoOcc")
  )
  my_iter <- 1:nsim
  
  result <- foreach::foreach(i = my_iter) %dopar% {
    autoOcc::auto_occ(
      ~x~x,
      y = data[[i]]$y,
      det_covs = data[[i]]$x,
      occ_covs = data[[i]]$x
    )
  }
  
  parallel::stopCluster(cl)
  return(result)
}