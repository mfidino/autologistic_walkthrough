dynamic_simulate <- function(par_list){
  
  sim_list <- list(
    n30 = vector(
      "list",
      length = par_list$nsim
    ),
    n50 = vector(
      "list",
      length = par_list$nsim
    ),
    par_list = par_list
  )
  pb <- txtProgressBar(max = par_list$nsim)
  for(sim in 1:par_list$nsim){
    setTxtProgressBar(pb, sim)
    for(n in 1:length(par_list$n)){
      # set the seed
      if(n == 1){
        set.seed(312 + sim)
      } else {
        set.seed(60647 + sim)
      }
      # latent state
      z <- matrix(
        NA,
        ncol = par_list$nseason,
        nrow = par_list$n[n]
      )
      # covariate
      x <- cbind(1, rnorm(par_list$n[n]))
      
      # probs of each linear predictor
      psi_prob <- plogis(x %*% par_list$psi)
      gamma_prob <- plogis(x %*% par_list$gamma)
      eps_prob <- plogis(x %*% par_list$eps)
      rho_prob <- plogis(x %*% par_list$rho)
      
      x <- data.frame(x = x[,2])
      
      # fill in latent state, first season
      z[,1] <- rbinom(
        par_list$n[n],
        1,
        psi_prob
      )
      # fill in latent state, other seasons
      for(yr in 2:par_list$nseason){
        tmp_prob <- (
          (1 - z[,yr-1]) * gamma_prob
        ) + (
          z[,yr-1] * eps_prob
        )
        z[,yr] <- rbinom(
          par_list$n[n],
          1,
          tmp_prob
        )
      }
      # Create observed data
      y <- array(
        NA,
        dim = c(
          par_list$n[n],
          par_list$nseason,
          par_list$nrep
        )
      )
      for(yr in 1:par_list$nseason){
        y[,yr,] <- matrix(
            rbinom(
              par_list$n[n] * par_list$nrep,
              1,
              rep(rho_prob, par_list$nrep) * rep(z[,yr], par_list$nrep)
            ),
            ncol = par_list$nrep,
            nrow = par_list$n[n]
        )
      }
      if(n == 1){
        sim_list$n30[[sim]] <- list(
          y = y,
          x = x,
          z = z
        )
      } else {
        sim_list$n50[[sim]] <- list(
          y = y,
          x = x,
          z = z
        )
      }
    }
  }
 return(sim_list) 
}


autologistic_simulate <- function(par_list){
  
  sim_list <- list(
    n30 = vector(
      "list",
      length = par_list$nsim
    ),
    n50 = vector(
      "list",
      length = par_list$nsim
    ),
    par_list = par_list
  )
  pb <- txtProgressBar(max = par_list$nsim)
  for(sim in 1:par_list$nsim){
    setTxtProgressBar(pb, sim)
    for(n in 1:length(par_list$n)){
      # set the seed
      if(n == 1){
        set.seed(312 + sim)
      } else {
        set.seed(60647 + sim)
      }
      # latent state
      z <- matrix(
        NA,
        ncol = par_list$nseason,
        nrow = par_list$n[n]
      )
      # covariate
      x <- cbind(1, rnorm(par_list$n[n]))
      
      # probs of each linear predictor
      psi_prob <- plogis(x %*% par_list$psi)
      psi_prob_theta <- plogis(x %*% par_list$psi + par_list$theta)
      rho_prob <- plogis(x %*% par_list$rho)
      
      x <- data.frame(x = x[,2])
      
      # fill in latent state, first season
      z[,1] <- rbinom(
        par_list$n[n],
        1,
        psi_prob
      )
      # fill in latent state, other seasons
      for(yr in 2:par_list$nseason){
        tmp_prob <- (
          (1 - z[,yr-1]) * psi_prob
        ) + (
          z[,yr-1] * psi_prob_theta
        )
        z[,yr] <- rbinom(
          par_list$n[n],
          1,
          tmp_prob
        )
      }
      # Create observed data
      y <- array(
        NA,
        dim = c(
          par_list$n[n],
          par_list$nseason,
          par_list$nrep
        )
      )
      for(yr in 1:par_list$nseason){
        y[,yr,] <- matrix(
          rbinom(
            par_list$n[n] * par_list$nrep,
            1,
            rep(rho_prob, par_list$nrep) * rep(z[,yr], par_list$nrep)
          ),
          ncol = par_list$nrep,
          nrow = par_list$n[n]
        )
      }
      if(n == 1){
        sim_list$n30[[sim]] <- list(
          y = y,
          x = x,
          z = z
        )
      } else {
        sim_list$n50[[sim]] <- list(
          y = y,
          x = x,
          z = z
        )
      }
    }
  }
  return(sim_list) 
}

