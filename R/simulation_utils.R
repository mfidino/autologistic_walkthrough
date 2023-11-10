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
    coverage = NA,
    ci_width = NA
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
    tmp$ci_width[i] <- mean(
      abs(tmp_coef$upper - tmp_coef$lower)
    )
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
