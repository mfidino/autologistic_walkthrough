
# sdf = SWEEP DATA FRAME

sdf <- read.csv(
  "./data/sim_sweep_rmse.csv"
)

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


tmp <- sdf[[1]]$auto

tmp <- tmp[order(tmp$nsite, tmp$nseason),]





