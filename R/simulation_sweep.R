
if(
  file.exists("./data/sim_sweep.RDS")
) {
  sim_list <- readRDS(
    "./data/sim_sweep.RDS"
  )
} else {
  # read in functions to simulate
  source("./R/simulate.R")

  my_meds <- expand.grid(
    gamma = seq(0.2,0.8, 0.01),
    eps = seq(0.2,0.8, 0.01)
  )
  my_meds$psi <- my_meds$gamma / (my_meds$gamma + my_meds$eps)
  my_meds$psi <- as.character(my_meds$psi)
  
  my_meds$type <- NA
  
  my_meds$type[my_meds$psi == "0.2"] <- "0.2"
  my_meds$type[my_meds$psi == "0.3"] <- "0.3"
  my_meds$type[my_meds$psi == "0.4"] <- "0.4"
  my_meds$type[my_meds$psi == "0.5"] <- "0.5"
  my_meds <- my_meds[complete.cases(my_meds),]
  
  my_meds <- my_meds[order(my_meds$type),]
  my_meds <- split(
    my_meds,
    factor(my_meds$type)
  )
  
  for(i in 1:length(my_meds)){
    tmp <- my_meds[[i]]
    if(nrow(tmp)>5){
      to_keep <- seq(1,nrow(tmp), length.out = 5)
      tmp <- tmp[to_keep,]
    }
    my_meds[[i]] <- tmp
  }
  
  # based on the above, we are using these values for
  #  average expected occupancy.
  geps <- data.frame(
    gamma = qlogis(c(0.2, 0.3, 0.52, 0.65)),
    eps = qlogis(c(0.8, 0.7, 0.78, 0.65))
  )
  
  # Figure it out for the autologistic model now too
  ex_occ <- seq(0.2, 0.5, 0.1)
  theta <- 1
  
  to_test <- qlogis(seq(0.1, 0.9, 0.001))
  
  results <- vector("list", 4)
  for(i in 1:4){
    tmp <- plogis(to_test) / (
      plogis(to_test) + (1 - plogis(to_test + theta))
    )
    tmp <- as.character(round(tmp,2))
    to_keep <- which(tmp == ex_occ[i])
    results[[i]] <- round(mean(to_test[to_keep]),2)
  }
  
  geps$psi <- unlist(results)
  geps$theta <- 1
  
  other_vars <- expand.grid(
    init_psi = 1,
    slope = c(1),
    rho = c(-0.9),
    n = seq(30, 100, 10),
    nseason = seq(4,12, 2),
    nsim = 550
  )
  
  # the list of simulations to run
  sim_list <- list(
    dynamic = list(
      o2 = vector("list", nrow(other_vars)),
      o3 = vector("list", nrow(other_vars)),
      o4 = vector("list", nrow(other_vars)),
      o5 = vector("list", nrow(other_vars))
    ),
    auto = list(
      o2 = vector("list", nrow(other_vars)),
      o3 = vector("list", nrow(other_vars)),
      o4 = vector("list", nrow(other_vars)),
      o5 = vector("list", nrow(other_vars))
    )
  )
  for(i in 1:4){
    dyn_pars <- list(
      psi = NA,
      gamma = NA,
      eps = NA,
      rho =  NA,
      n = NA,
      nseason = NA,
      nrep = 4,
      nsim = 550
    )
    aut_pars <- list(
      psi = NA,
      theta = NA,
      rho =  NA,
      n = NA,
      nseason = NA,
      nrep = 4,
      nsim = 550
    )
    
    for(j in 1:nrow(other_vars)){
      cat(
        paste0(
          "\n Occupancy scenario: ",i,
          ", n: ", other_vars$n[j],
          ", t: ", other_vars$nseason[j],
          ".\n"
        )
      )
      dyn_pars$psi <- c(
        other_vars$init_psi[j], 0
      )
      dyn_pars$gamma <- c(
        geps$gamma[i], other_vars$slope[j]
      )
      dyn_pars$eps <- c(
        geps$eps[i], other_vars$slope[j]
      )
      dyn_pars$rho <- c(
        other_vars$rho[j], 0.5
      )
      dyn_pars$n <- other_vars$n[j]
      dyn_pars$nseason <- other_vars$nseason[j]
      sim_list$dynamic[[i]][[j]] <- dynamic_simulate2(
        dyn_pars
      )
      cat("\n")
      aut_pars$psi <- c(
        geps$psi[i], other_vars$slope[i]
      )
      aut_pars$theta <- geps$theta[i]
      aut_pars$rho <- c(
        other_vars$rho[j], 0.5
      )
      aut_pars$n <- other_vars$n[j]
      aut_pars$nseason <- other_vars$nseason[j]
      sim_list$auto[[i]][[j]] <- autologistic_simulate2(
        aut_pars
      )
    }
  }
  
  sim_list$targets <- list(
    latent = geps,
    others = other_vars
  )
  saveRDS(
    sim_list,
    "./data/sim_sweep.RDS"
  )
  
  to_go <- ls()
  to_go <- to_go[-which(to_go == "sim_list")]
  rm(list = to_go)
}
