fit_simulations <- function(data, ncores = 10, nsim){
  num_cores <- ncores
  
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



fit_sweep <- function(data, ncores = 10, nsim, auto = TRUE){
  
  cl <- parallel::makeCluster(
    ncores
  )
  
  doParallel::registerDoParallel(
    cl
  )
  
  parallel::clusterEvalQ(
    cl,
    {
      library("autoOcc")
      library("unmarked")
    }
  )
  my_iter <- 1:nsim
  if(auto){
    result <- foreach::foreach(i = my_iter, .errorhandling = "pass") %dopar% {
        autoOcc::auto_occ(
          ~x~x,
          y = data[[i]]$y,
          det_covs = data[[i]]$x,
          occ_covs = data[[i]]$x
        )
    }
  }else{
    result <- foreach::foreach(i = my_iter, .errorhandling = "pass") %dopar% {
      dims <- dim(data[[i]]$y)
      nsite <- dims[1]
      nprimary <- dims[2]
      nrep <- dims[3]
      tmp_y <- tmp_obs <- matrix(
        NA,
        ncol = nprimary * nrep,
        nrow = nsite
      )
      j_loc <- rep(
        1:nprimary,
        each = nrep
      )
      for(j in 1:nprimary){
        tmp_y[,which(j_loc == j)] <- data[[i]]$y[,j,]
      }
      tmp_y <- data.frame(tmp_y)
      tmp_obs <- data.frame(
        matrix(
          data[[i]]$x$x,
          ncol = nprimary * nrep,
          nrow = nsite
        )
      )
      tmp_obs <- list(
        x = tmp_obs
      )
      umf <- unmarkedMultFrame(
        y = tmp_y,
        siteCovs = data.frame(
          x = data[[i]]$x$x
        ),
        obsCovs = tmp_obs,
        numPrimary = nprimary
      )
      
      dyn_fit <- colext(~x,~x,~x,~x, umf)
      dyn_fit
    }
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

calc_rmse <- function(one_coef, truth, auto = TRUE){
  cnames <- unique(one_coef$parameter)
  tmp <- data.frame(
    parameter = cnames,
    rmse = NA,
    coverage = NA,
    ci_width = NA,
    signif = NA
  )
  if(auto){
    tpars <- c(
      truth$psi, truth$theta, truth$rho
    )
  } else {
    tpars <- c(
      truth$psi, truth$gamma, truth$eps, truth$rho
    )
  }
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
    tmp$ci_width[i] <- mean(
      abs(tmp_coef$upper - tmp_coef$lower)
    )
    tmp$signif[i] <- mean(
      tmp_coef$p < 0.05
    )
    tmp$rel_bias[i] <- 
      (mean(tmp_coef$Est) - tpars[i]) / abs(tpars[i])
  }
  return(tmp)
}

pred_rmse <- function(one_coef, truth){
  # first generate expected occupancy 
  xmat <- cbind(1, unique(one_coef$x))
  
  gamma <- plogis(
    xmat %*% truth$gamma
  )
  eps <- plogis(
    xmat %*% truth$eps
  )
  ex_occ <- gamma / (gamma + eps)
  tmp <- data.frame(
    xval = xmat[,2],
    rmse = NA,
    ci_width = NA
  )
  one_coef$truth <- rep(ex_occ, max(one_coef$simulation))
  one_coef$fac <- rep(1:nrow(xmat), max(one_coef$simulation))
  one_coef$fac <- stringr::str_pad(
    one_coef$fac,
    width = 3,
    pad = "0"
  )
  
  
  one_coef <- split(
    one_coef,
    factor(one_coef$fac)
  )
  one_coef <- lapply(
    one_coef,
    function(x){
      data.frame(
        truth = unique(x$truth),
        mean = mean(x$estimate),
        rmse = rmse(x$estimate, unique(x$truth)),
        ci_width = mean(
          x$upper - x$lower
        ),
        x = unique(x$x)
      )
    }
  )
  one_coef <- dplyr::bind_rows(one_coef)
  return(one_coef)
}


underlined <- function(x, y, label, ...){
  text(x, y, label, ...)
  sw <- strwidth(label)
  sh <- strheight(label)
  lines(x + c(-sw/1.5, sw/1.5), rep(y - 2*sh/2, 2))
}
