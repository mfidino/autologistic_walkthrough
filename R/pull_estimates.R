# pull model parameters
library(dplyr)
my_files <- list.files(
  "./data",
  "*.RDS",
)
pb <- txtProgressBar(max= length(my_files))
for(i in 1:length(my_files)){
  setTxtProgressBar(pb, i)
  fit <- readRDS(
    paste0(
      "./data/",
      my_files[i]
    )
  )
  # first check to see if model converged
  all_there <- sapply(
    fit,
    function(x) all(
      complete.cases(x@estimates)
    )
  )
  resave <- FALSE
  if(any(!all_there)){
    fit <- fit[-which(!all_there)]
    resave <- TRUE
  }
  if(length(fit)< 500){
    stop("run more simulations to get to 500.")
  }
  if(length(fit) > 500){
    fit <- fit[1:500]
    resave <- TRUE
  }
  #if(resave){
  #  saveRDS(
  #    fit,
  #    paste0(
  #      "./data/",
  #      my_files[i]
  #    )
  #  )
  #}

  my_coefs <- lapply(
    fit,
    function(x) x@estimates
  )
  my_coefs <- dplyr::bind_rows(my_coefs)
  my_coefs$simulation <- rep(
    1:length(fit),
    each = length(
      unique(my_coefs$parameter)
    )
  )
  
  write.csv(
    my_coefs,
    paste0(
      "./data/coefs_",
      gsub(
        "RDS$",
        "csv",
        my_files[i]
      )
    ),
    row.names = FALSE
  )
}

# And make predictions with the model as well.
new_data <- data.frame(
  x = seq(-2,2, length.out = 200)
)

pb <- txtProgressBar(max= length(my_files))
for(i in 1:length(my_files)){
  setTxtProgressBar(pb, i)
  fit <- readRDS(
    paste0(
      "./data/",
      my_files[i]
    )
  )
  model_pred <- lapply(
    fit,
    function(x){
      predict(
        x,
        "psi",
        newdata = new_data
      )
    }
  )
  model_pred <- dplyr::bind_rows(
    model_pred
  )
  model_pred$simulation <- rep(
    1:length(fit),
    each = nrow(new_data)
  )
  model_pred$x <- new_data$x
  
  write.csv(
    model_pred,
    paste0(
      "./data/preds_",
      gsub(
        "RDS$",
        "csv",
        my_files[i]
      )
    ),
    row.names = FALSE
  )
}

