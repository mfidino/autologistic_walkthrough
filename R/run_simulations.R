
# Do dynamic model first
source("./R/simulate.R")

all_sims <- vector(
  "list",
  length = 6
)


#### Different signs, large effect ####
pars <- list(
  psi = c(1,0.5),
  gamma = c(0.5,1 ),
  eps = c(-0.5, -1),
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\n\n##############################\n")
cat("Dynamic occupancy model\n")
cat("##############################\n\n")
cat("\nDynamic: Different signs, large effect\n")

dyn_sim <- dynamic_simulate(
  par_list = pars
)
dyn_sim$type <- "dynamic_different_signs_large_effect"

all_sims[[1]] <- dyn_sim

#### Different signs, small effect ####
pars <- list(
  psi = c(1,0.5),
  gamma = c(0.5,0.3 ),
  eps = c(-0.5, -0.3),
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\nDynamic: Different signs, small effect\n")
dyn_sim <- dynamic_simulate(
  par_list = pars
)
dyn_sim$type <- "dynamic_different_signs_small_effect"

all_sims[[2]] <- dyn_sim


#### Same signs, large effect ####
pars <- list(
  psi = c(1,0.5),
  gamma = c(0.5,1 ),
  eps = c(0.5, 1),
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\nDynamic: Same signs, large effect\n")
dyn_sim <- dynamic_simulate(
  par_list = pars
)
dyn_sim$type <- "dynamic_same_signs_large_effect"

all_sims[[3]] <- dyn_sim

#### Same signs, small effect ####
pars <- list(
  psi = c(1,0.5),
  gamma = c(0.5,0.3 ),
  eps = c(0.5, 0.3),
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\nDynamic: Same signs, small effect\n")
dyn_sim <- dynamic_simulate(
  par_list = pars
)
dyn_sim$type <- "dynamic_same_signs_small_effect"

all_sims[[4]] <- dyn_sim

#### auto-logistic, large effect ####
cat("\n\n##############################\n")
cat("Autologistic occupancy model\n")
cat("##############################\n\n")

pars <- list(
  psi = c(0.5,1),
  theta = 1,
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\nAutologistic: large effect\n")
at_sim <- autologistic_simulate(
  par_list = pars
)

at_sim$type <- "autologistic_large_effect"


all_sims[[5]] <- at_sim
#### auto-logistic, small effect ####

pars <- list(
  psi = c(0.5,0.3),
  theta = 1,
  rho =  c(-0.9, 0.5),
  n = c(30, 50),
  nseason = 8,
  nrep = 4,
  nsim = 550
)
cat("\nAutologistic: small effect\n")
at_sim <- autologistic_simulate(
  par_list = pars
)

at_sim$type <- "autologistic_small_effect"

all_sims[[6]] <- at_sim

names(all_sims) <- sapply(
  all_sims,
  function(x) x$type
)
rm(
   list = c(
     "at_sim", "dyn_sim", "pars", "autologistic_simulate", "dynamic_simulate"
   )
)
