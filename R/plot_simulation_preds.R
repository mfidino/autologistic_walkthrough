library(bbplot)

# simulate the data again to get true the parameter estimates
source("./R/run_simulations.R")
source("./R/simulation_utils.R")

# read in preds for autologistic model
my_files <- list.files(
  "./data/",
  pattern = "preds"
)

preds <- lapply(
  paste0(
    "./data/",
    my_files
  ),
  read.csv
)
names(preds) <-  my_files

# start with the dynamic model first
ac <- preds[grep("dynamic", my_files)]

ac <- list(
  diff = ac[grep("different", names(ac))],
  same = ac[grep("same", names(ac))]
)


pred_results <- list(
  diff = list(
    thirty_large = pred_rmse(
      one_coef = ac$diff$preds_dynamic_different_signs_large_effect__30sites.csv,
      truth = all_sims$dynamic_different_signs_large_effect$par_list
    ),
    fifty_large = pred_rmse(
      one_coef = ac$diff$preds_dynamic_different_signs_large_effect__50sites.csv,
      truth = all_sims$dynamic_different_signs_large_effect$par_list
    ),
    thirty_small = pred_rmse(
      one_coef = ac$diff$preds_dynamic_different_signs_small_effect__30sites.csv,
      truth = all_sims$dynamic_different_signs_small_effect$par_list
    ),
    fifty_small = pred_rmse(
      one_coef = ac$diff$preds_dynamic_different_signs_small_effect__50sites.csv,
      truth = all_sims$dynamic_different_signs_small_effect$par_list
    )
  ),
  same = list(
    thirty_large = pred_rmse(
      one_coef = ac$same$preds_dynamic_same_signs_large_effect__30sites.csv,
      truth = all_sims$dynamic_same_signs_large_effect$par_list
    ),
    fifty_large = pred_rmse(
      one_coef = ac$same$preds_dynamic_same_signs_large_effect__50sites.csv,
      truth = all_sims$dynamic_same_signs_large_effect$par_list
    ),
    thirty_small = pred_rmse(
      one_coef = ac$same$preds_dynamic_same_signs_small_effect__30sites.csv,
      truth = all_sims$dynamic_same_signs_small_effect$par_list
    ),
    fifty_small = pred_rmse(
      one_coef = ac$same$preds_dynamic_same_signs_small_effect__50sites.csv,
      truth = all_sims$dynamic_same_signs_small_effect$par_list
    )
  )
)


windows(7,3.5)
#tiff(
#  "./plots/urban_openspace.tiff",
#  height = 3.5,
#  width = 7,
#  units = "in",
#  res = 600,
#  compression = "lzw"
#)


direction_cols <- c("#3423A6", "#E94F37")
type_label <- c(
  "30 sites, large effect",
  "50 sites, large effect",
  "30 sites, small effect",
  "50 sites, small effect"
)
direction_label <- c("Negative", "Positive")
lab_stuff <- data.frame(
  i = rep(1:4,each = 2),
  j = 1:2,
  y = FALSE,
  x = FALSE,
  type = FALSE,
  sl = FALSE
)
lab_stuff$y[c(1,5)] <- TRUE
lab_stuff$x[5:8] <- TRUE
lab_stuff$type[1:4] <- TRUE
lab_stuff$sl[c(4,8)] <- TRUE
par(mar = c(1,1,1,1), oma = c(4,4,2,10.5), lend = 1, xpd = NA)
m <- matrix(
  1:8,
  ncol = 4,
  nrow = 2,
  byrow = TRUE
)
layout(m)
rloc <- data.frame(
  f = rep(c(1,2), each = 4),
  s = rep(1:4, 2)
  )


par(mar = c(1,1,1,1), oma = c(4,4,2,10.5), lend = 1, xpd = NA)
m <- matrix(
  1:6,
  ncol = 3,
  nrow = 2,
  byrow = TRUE
)
layout(m)
# plot preds first
bbplot::blank(xlim = c(-2,2), ylim = c(0,1), bty = "l")
bbplot::axis_blank(1)
bbplot::axis_blank(2)

for(i in 1:)


layout(m)
for(i in 1:length(pred_df)){
  for(j in 1:length(pred_df[[i]])){
    tmp_lab <- lab_stuff[
      lab_stuff$i == i &
        lab_stuff$j == j,
    ]
    if()
    bbplot::blank(xlim = c(-2,2), ylim = c(0,1), bty = "l")
    bbplot::axis_blank(1)
    bbplot::axis_blank(2)
    if(tmp_lab$y){
      bbplot::axis_text(
        text = seq(0,0.6,0.2),
        at = seq(0,0.6,0.2),
        side = 2, las = 1, line = 0.5)
    }
    if(tmp_lab$x){
      bbplot::axis_text(
        text = c("0", "0.2", "0.4"),
        side = 1,
        line = 0.5,
        at = c(0,0.2,0.4)
      )
    }
    if(tmp_lab$season){
      u <- par("usr")
      text(
        x = mean(u[1:2]),
        y = u[4] + 0.1,
        labels =
          underlined(
            x = mean(u[1:2]),
            y = u[4] + 0.1,
            season_label[as.numeric(row.names(tmp_lab))],
            cex = 1.2
          )
      )
    }
    if(tmp_lab$sl){
      u <- par("usr")
      lloc <- sum(lab_stuff$sl[1:as.numeric(row.names(tmp_lab))])
      text(
        x = u[2] + 0.1,
        y = mean(u[3:4]),
        labels = lane_label[lloc],
        cex = 1.2,
        srt = 270
      )
      lines(
        x = rep(u[2]+0.065,2),
        y = c(0,0.6)
      )
    }
    if(m[j,i] == 6){
      bbplot::axis_text(
        "Urban openspace (proportion)",
        side = 1,
        outer = TRUE,
        at = 0.5,
        line = 1.5
      )
    }
    if(m[j,i] == 1){
      
      bbplot::axis_text(
        "Pr(Deer vehicle collision)",
        side = 2,
        outer = TRUE,
        at = 0.5,
        line = 2.5
      )
    }
    bbplot::ribbon(
      x = tmp55$covar,
      y = tmp55[,c("lower", "upper")],
      col = speed_cols[1],
      alpha = 0.5
    )
    bbplot::ribbon(
      x = tmp70$covar,
      y = tmp70[,c("lower", "upper")],
      col = speed_cols[2],
      alpha = 0.5
    )
    lines(
      x = tmp55$covar,
      y = tmp55$mu,
      col = speed_cols[1],
      lwd = 2
    )
    lines(
      x = tmp70$covar,
      y = tmp70$mu,
      col = speed_cols[2],
      lwd = 2,
      lty = 2
    )
  }
  
}
legend(
  x = 0.6,
  y = 1,
  legend = c("55", "70"),
  lty = c(1:2),
  col = speed_cols,
  lwd =2,
  bty = "n",
  title = "Speed limit\n(mph)",
  seg.len = 2,
  cex = 1.3
  
)
dev.off()
