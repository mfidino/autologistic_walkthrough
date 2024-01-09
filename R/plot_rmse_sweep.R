library(bbplot)
library(pals)

source("./R/simulation_sweep.R")
source("./R/plot_utilities.R")
# sdf = SWEEP DATA FRAME
sdf <- read.csv(
  "./data/sim_sweep_rmse.csv"
)

sdf <- sdf[-which(sdf$parameter %in% c("psi(Int)", "psi(x)")),]
# make colors
(rmse_range <- range(sdf$rmse))
rmse_range <- seq(0, 25, length.out = 100)

sdf <- split(
  sdf,
  factor(sdf$scenario)
)

for(i in 1:length(sdf)){
  sdf[[i]] <- split(
    sdf[[i]],
    factor(sdf[[i]]$model)
  )
}


# get subsetting vectors together
model_vec <- rep(
  c("auto", "auto", "dynamic","dynamic"),
  4
)
scenario_vec <- rep(1:4, each = 4)
parm_vec <- rep(
  c("psi - (Intercept)", "psi - theta",
     "col(Int)", "ext(Int)"),
  4
)

subtitle <- LETTERS[1:16]
subtitle <- paste0(subtitle,") ")
subtitle_val <- sim_list$targets$latent
subtitle_val <- subtitle_val[,c("psi","theta", "gamma", "eps")]
subtitle_val <- as.matrix(subtitle_val)
subtitle_val <- round(subtitle_val,2)
subtitle_val <- t(subtitle_val)
windows(6.4,5)
m <- matrix(
  1:16,
  ncol = 4,
  nrow = 4,
  byrow = TRUE
)

layout(m)
par(mar = c(0.75,0.75,1.2,.75), oma = c(8,6,2,8))
mean_rmse <- rep(NA, 16)
for(i in 1:16){
  tmp <- sdf[[scenario_vec[[i]]]][[model_vec[i]]]
  my_yax <- ifelse(
    i %in% m[,1],
    TRUE,
    FALSE
  )
  my_xax <- ifelse(
    i %in% m[4,],
    TRUE,
    FALSE
  )
  mean_rmse[i] <- one_plot(
    x = tmp,
    yax = my_yax,
    xax = my_xax,
    param = parm_vec[i],
    zrange = c(0,5),
    return_mean = TRUE
  )
  if(i == m[1,4]){
    mtext(
      expression(
        paste("E(",Psi,") = 0.2")
      ),
      side = 4,
      las = 1,
      at = 0.87,
      outer = TRUE,
      line = 0.7
    )
  }
  if(i == m[2,4]){
    mtext(
      expression(
        paste("E(",Psi,") = 0.3")
      ),
      side = 4,
      las = 1,
      at = 0.62,
      outer = TRUE,
      line = 0.7
    )
  }
  if(i == m[3,4]){
    mtext(
      expression(
        paste("E(",Psi,") = 0.4")
      ),
      side = 4,
      las = 1,
      at = 0.38,
      outer = TRUE,
      line = 0.7
    )
  }
  if(i == m[3,4]){
    mtext(
      expression(
        paste("E(",Psi,") = 0.5")
      ),
      side = 4,
      las = 1,
      at = 0.13,
      outer = TRUE,
      line = 0.7
    )
  }
}



mm <- matrix(
  round(mean_rmse,2),
  ncol =4 , 
  nrow = 4,
  byrow = TRUE
)


hey <- "this_is__a_0_string"

# double to 1
h1 <- gsub("__", "_", hey)
# _0_ to _
h2 <- gsub("_0_", "_", h1)

strsplit(h2, "_")




