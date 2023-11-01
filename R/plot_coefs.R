library(bbplot)

# simulate the data again to get true the parameter estimates
source("./R/run_simulations.R")
source("./R/simulation_utils.R")

# read in coefs for autologistic model
my_files <- list.files(
  "./data/",
  pattern = "coefs"
)

coefs <- lapply(
  paste0(
    "./data/",
    my_files
  ),
  read.csv
)
names(coefs) <-  my_files
# get just the autologistic ones
ac <- coefs[grep("autologistic", my_files)]


coef_results <- list(
  thirty_large = calc_rmse(
    ac$coefs_autologistic_large_effect__30sites.csv,
    all_sims$autologistic_large_effect$par_list
  ),
  fifty_large = calc_rmse(
    ac$coefs_autologistic_large_effect__50sites.csv,
    all_sims$autologistic_large_effect$par_list
  ),
  thirty_small = calc_rmse(
    ac$coefs_autologistic_small_effect__30sites.csv,
    all_sims$autologistic_small_effect$par_list
  ),
  fifty_small = calc_rmse(
    ac$coefs_autologistic_small_effect__50sites.csv,
    all_sims$autologistic_small_effect$par_list
  )
)


windows(width = 7, height = 4)
m <- matrix(c(1,1,1,2,2,2,3), ncol = 7)
layout(m)
par(mar = c(4,1,1,1))
{
bbplot::blank(xlim = c(0,4), ylim = c(0.9,3.5))
bbplot::axis_blank(1)
bbplot::axis_text(side = 1, line = 1)
bbplot::axis_text("RMSE", side = 1, line = 2.5, cex = 1.2)
for(i in 1:3){
  lines(
    x = c(
      coef_results$thirty_small$rmse[i],
      coef_results$fifty_small$rmse[i]
    ),
    y = rep(4 - i, 2),
    col = "gray",
    lwd = 3.5
  )
  points(
    x = coef_results$thirty_small$rmse[i],
    y = 4-i,
    pch = 21,
    bg = "#FE9000",
    cex = 2
  )
  points(
    x = coef_results$fifty_small$rmse[i],
    y = 4-i,
    pch = 24,
    bg = "#FE9000",
    cex = 2
  )
  lines(
    x = c(
      coef_results$thirty_large$rmse[i],
      coef_results$fifty_large$rmse[i]
    ),
    y = rep(4.33 - i, 2),
    col = "gray",
    lwd = 3.5
  )
  points(
    x = coef_results$thirty_large$rmse[i],
    y = 4.33-i,
    pch = 21,
    bg = "#19647E",
    cex = 2
  )
  points(
    x = coef_results$fifty_large$rmse[i],
    y = 4.33-i,
    pch = 24,
    bg = "#19647E",
    cex = 2
  )
}
  par(xpd = NA)
  text(
    y = 3.6,
    x = 0.25,
    labels = expression(beta[0]),
    cex = 2,
    pos = 4
  )
  text(
    y = 2.6,
    x = 0.25,
    labels = expression(beta[1]),
    cex = 2,
    pos = 4
  )
  text(
    y = 1.6,
    x = 0.25,
    labels = expression(theta),
    cex = 2,
    pos = 4
  )

  bbplot::blank(xlim = c(0.95,1), ylim = c(0.9,3.5))
  bbplot::axis_blank(1)
  bbplot::axis_text(side = 1, line = 1)
  bbplot::axis_text("Coverage", side = 1, line = 2.5, cex = 1.2)

  for(i in 1:3){
    lines(
      x = c(
        coef_results$thirty_small$coverage[i],
        coef_results$fifty_small$coverage[i]
      ),
      y = rep(4 - i, 2),
      col = "gray",
      lwd = 3.5
    )
    points(
      x = coef_results$thirty_small$coverage[i],
      y = 4-i,
      pch = 21,
      bg = "#FE9000",
      cex = 2
    )
    points(
      x = coef_results$fifty_small$coverage[i],
      y = 4-i,
      pch = 24,
      bg = "#FE9000",
      cex = 2
    )
    lines(
      x = c(
        coef_results$thirty_large$coverage[i],
        coef_results$fifty_large$coverage[i]
      ),
      y = rep(4.33 - i, 2),
      col = "gray",
      lwd = 3.5
    )
    points(
      x = coef_results$thirty_large$coverage[i],
      y = 4.33-i,
      pch = 21,
      bg = "#19647E",
      cex = 2
    )
    points(
      x = coef_results$fifty_large$coverage[i],
      y = 4.33-i,
      pch = 24,
      bg = "#19647E",
      cex = 2
    )
  }
  par(xpd = NA)
  text(
    y = 3.6,
    x = 0.953,
    labels = expression(beta[0]),
    cex = 2,
    pos = 4
  )
  text(
    y = 2.6,
    x = 0.953,
    labels = expression(beta[1]),
    cex = 2,
    pos = 4
  )
  text(
    y = 1.6,
    x = 0.953,
    labels = expression(theta),
    cex = 2,
    pos = 4
  )
}
bbplot::blank(xlim = c(0,1), ylim = c(0,1))
legend(
  x = -0.4, 
  y = 0.9,
  c(30, 50),
  pch = c(21, 24),
  bty = "n",
  title = "Sites",
  cex = 1.7,
  pt.cex = 2
)
legend(
  x = -0.5, 
  y = 0.5,
  c("Large", "Small"),
  fill = c("#19647E", "#FE9000"),
  bty = "n",
  title = "Effect size",
  cex = 1.7
)


legend(
  x = 0.1,
  y = 0,
  legend = c(
    "30 sites, large effect size",
    "50 sites, large effect size",
    "30 sites, small effect size",
    "50 sites, large effect size"
  ),
  pch = c(21, 24, 21, 24),
  pt.bg = c("#19647E", "#19647E", "#FE9000", "#FE9000"),
  border = NULL
  )
