# pull in simulated data
source("./R/simulation_sweep.R")
source("./R/simulation_utils.R")

my_files <- list.files(
  "./data/sweep_fits/",
  pattern = "*.RDS"
)

model_info <- vector(
  "list",
  length = length(my_files)
)
results <- vector(
  "list",
  length = length(my_files)
)

for(i in 1:length(my_files)){
  one_file <- readRDS(
    paste0(
      "./data/sweep_fits/",
      my_files[i]
    )
  )
  # parse the file name
  tmp <- strsplit(
    my_files[i],
    "_"
  )
  model_info[[i]]$model <- ifelse(
    length(grep("auto", tmp[[1]][1])) == 1,
    "auto",
    "dynamic"
  )
  model_info[[i]]$scenario <- as.numeric(
    gsub(
      "[a-z]",
      "",
      tmp[[1]][1]
    )
  )
  model_info[[i]]$nsite <- as.numeric(
    gsub(
      "[a-z]",
      "",
      tmp[[1]][2]
    )
  )
  model_info[[i]]$nseason <- as.numeric(
    gsub(
      "[A-z]|\\.",
      "",
      tmp[[1]][3]
    )
  )
  # locate which row of targets we are looking at
  my_row <- which(
    sim_list$targets$others$n == model_info[[i]]$nsite &
      sim_list$targets$others$nseason == model_info[[i]]$nseason 
  )
  # get latent state targets
  latent_targets <- sim_list$targets$latent[
    model_info[[i]]$scenario,
  ]
  # get other targets (slope terms mostly)
  other_targets <- sim_list$targets$others[my_row,]
  if(model_info[[i]]$model == "auto"){
    # get coefficients
    my_coefs <- lapply(
      one_file,
      function(x)x@estimates
    )
    # check if they are all complete cases
    to_keep <- sapply(
      my_coefs,
      function(x) all(complete.cases(x))
    )
    if(any(to_keep == FALSE)){
      stop()
    }
    my_coefs <- dplyr::bind_rows(my_coefs)
    one_coef <- my_coefs
    this_truth <- list(
      psi = c(
        latent_targets$psi,
      other_targets$slope,
      latent_targets$theta
    ),
    rho = c(
      other_targets$rho, 0.5
      )
    )
  }
  if(model_info[[i]]$model == "dynamic"){
    # get coefficients
    my_coefs <- lapply(
      one_file,
      function(x) x@estimates@estimates
    )
    tmp <- vector("list", 4)
    um_coef_frame <- vector("list", length = length(my_coefs))
    for(j in 1:length(my_coefs)){
      for(k in 1:4){
        tmp[[k]] <- my_coefs[[j]][[k]]@estimates
      }
      ests <- unlist(tmp)
      SEs <- unmarked:::SE(one_file[[j]])
      Z <- ests/SEs
      p <- 2*pnorm(abs(Z), lower.tail = FALSE)
      um_coef_frame[[j]] <- data.frame(
        parameter = names(ests),
        Est = ests,
        SE = SEs,
        lower = qnorm(0.025, ests, SEs),
        upper = qnorm(0.975, ests, SEs),
        p = p
      )
      um_coef_frame[[j]]$parameter <- row.names(
        um_coef_frame[[j]]
      )
      row.names(um_coef_frame[[j]]) <- NULL
    }
    my_coefs <- um_coef_frame
    
    }
    # check if they are all complete cases
    to_keep <- sapply(
      my_coefs,
      function(x) all(complete.cases(x))
    )
    if(any(to_keep == FALSE)){
      stop()
    }
    my_coefs <- dplyr::bind_rows(my_coefs)
    this_truth <- list(
      psi = c(
        other_targets$init_psi,
        other_targets$slope
      ),
      gamma = c(
        latent_targets$gamma,
        other_targets$slope
      ),
      eps = c(
        latent_targets$eps,
        other_targets$slope
      ),
      rho = c(
        other_targets$rho,
        0.5
      )
    )
  }
    rmse_result <- calc_rmse(
      one_coef = my_coefs,
      truth = this_truth,
      auto = FALSE
    )
    rmse_result$nsite <- model_info[[i]]$nsite
    rmse_result$nseason <- model_info[[i]]$nseason
    rmse_result$model <- model_info[[i]]$model
    rmse_result$scenario <- model_info[[i]]$scenario
    
    results[[i]] <- rmse_result

  
  }
  