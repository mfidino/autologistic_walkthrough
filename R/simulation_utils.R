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

rmse <- function(xi, x){
  tmp <- sum(
    (xi - x)^2
  ) / length(xi)
  tmp <- sqrt(tmp)
  return(tmp)
}

calc_rmse <- function(one_coef, truth){
  cnames <- unique(one_coef$parameter)
  tmp <- data.frame(
    parameter = cnames,
    rmse = NA,
    coverage = NA
  )
  tpars <- c(
    truth$psi, truth$theta, truth$rho
  )
  for(i in 1:nrow(tmp)){
    tmp_coef <- one_coef[
      one_coef$parameter == tmp$parameter[i],
    ]
    tmp$rmse[i] <- rmse(
      tmp_coef$Est,
      tpars[i]
    )
    tmp$coverage[i] <- mean(
      tmp_coef$lower <= tpars[i] &
        tmp_coef$upper >= tpars[i]
    )
  }
  return(tmp)
}
