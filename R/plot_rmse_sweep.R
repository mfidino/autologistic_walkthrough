library(bbplot)
library(pals)

### Read in data ####

source("./R/simulation_sweep.R")
source("./R/plot_utilities.R")
# sdf = SWEEP DATA FRAME
sdf <- read.csv(
  "./data/sim_sweep_rmse.csv"
)

sdf <- sdf[-which(sdf$parameter %in% c("psi(Int)", "psi(x)")),]

sdf$rel_bias[sdf$rel_bias > 5] <- 5
sdf$ci_width[sdf$ci_width > 5] <- 5
# make colors
(bias_range <- range(sdf$rel_bias))
bias_range <- seq(-5, 5, length.out = 100)

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

### Relative bias ####

#### Figure 1 ####

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
tiff(
  "./plots/relative_bias_intercepts.tiff",
  width = 6.4,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)
{
m <- matrix(
  1:16,
  ncol = 4,
  nrow = 4,
  byrow = TRUE
)

layout(m)
par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
mean_bias <- rep(NA, 16)
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
  mean_bias[i] <- one_plot(
    x = tmp,
    yax = my_yax,
    xax = my_xax,
    param = parm_vec[i],
    zrange = c(-5,5),
    type = "rel_bias",
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
  if(i == m[1,1]){
    mtext(
      expression(underline("Autologistic")),
      side = 3,
      at = 0.25,
      outer = TRUE,
      line = 0.6
    )
  }
  if(i == m[1,4]){
    mtext(
      expression(underline("Dynamic")),
      side = 3,
      at = 0.75,
      outer = TRUE,
      line = 0.6
    )
  }
  if(i %in% m[,1]){
    mtext(
      substitute(
          paste(f1, Psi[0], " = ", f2)
        ,
        list(
          f1 = subtitle[i],
          f2 = subtitle_val[i]
        )
      ),
      side = 3,
      at = 30,
      line = 0.15,
      cex = 0.8,
      adj = c(0)
    )
  }
  if(i %in% m[,2]){
    mtext(
      substitute(
        paste(f1, theta[0], " = ", f2)
        ,
        list(
          f1 = subtitle[i],
          f2 = subtitle_val[i]
        )
      ),
      side = 3,
      at = 30,
      line = 0.15,
      cex = 0.8,
      adj = c(0)
    )
  }
  if(i %in% m[,3]){
    mtext(
      substitute(
        paste(f1, gamma[0], " = ", f2)
        ,
        list(
          f1 = subtitle[i],
          f2 = subtitle_val[i]
        )
      ),
      side = 3,
      at = 30,
      line = 0.15,
      cex = 0.8,
      adj = c(0)
    )
  }
  if(i %in% m[,4]){
    mtext(
      substitute(
        paste(f1, epsilon[0], " = ", f2)
        ,
        list(
          f1 = subtitle[i],
          f2 = subtitle_val[i]
        )
      ),
      side = 3,
      at = 30,
      line = 0.15,
      cex = 0.8,
      adj = c(0)
    )
  }
}
mtext(
  "Number of sites",
  side = 1,
  outer = TRUE,
  at = 0.5,
  cex = 1.2,
  line = 2.25
)
mtext(
  "Number of sampling seasons",
  side = 2,
  outer = TRUE,
  at = 0.5,
  cex = 1.2,
  line = 3
)
my_legend <- as.raster(matrix(pals::ocean.thermal(100), ncol = 100))
par(xpd = NA)
rasterImage(
  my_legend,
  xleft = -170,
  ybottom = -8.5,
  xright = 30,
  ytop = -5.5
)


rect(
  xleft = -170,
  ybottom = -8.5,
  xright = 30,
  ytop = -5.5,
  border = "black"
)
true_x <- seq(-170, 30, length.out = 100)
x_val <- round(seq(-5, 5, length.out = 100),1)
x_val[50] <- 0
to_plot <- which(x_val%%1 == 0)
for(i in 1:11){
  lines(
    x = rep(true_x[to_plot[i]],2),
    y = c(-8.5, -9.1)
  )
}
text(
  labels = c("< -5", "0", "> 5"),
  x = true_x[to_plot[c(1,6, 11)]],
  y = rep(-10.7, 3),
  cex = 1.6
)
mtext(
  "Relative bias (%)",
  side = 1,
  outer = TRUE,
  at = 0.5,
  line = 7.7,
  cex = 1
)
}
dev.off()

# Takeaway

# Averaged across scenarios and sample sizes, the relative bias
#  in the autologistic intercept term was about 6.5 times
#  less than the dynamic colonization intercept but about 1.45 times
#  greater than the dynamic extinction intercept. The autologistic
#  theta term had consistently less bias though, which was respectively
#  51 and 5.4 times smaller than the dynamic colonization and extinction
#  intercepts. T

mm <- matrix(
  round(mean_bias,2),
  ncol =4 , 
  nrow = 4,
  byrow = TRUE
)

#### Figure 2 ####


# get subsetting vectors together
model_vec <- rep(
  c("auto", "dynamic","dynamic"),
  4
)
scenario_vec <- rep(1:4, each = 3)
parm_vec <- rep(
  c("psi - x",
    "col(x)", "ext(x)"),
  4
)

subtitle <- LETTERS[1:12]
subtitle <- paste0(subtitle,") ")
#subtitle_val <- sim_list$targets$latent
#subtitle_val <- subtitle_val[,c("psi","theta", "gamma", "eps")]
#subtitle_val <- as.matrix(subtitle_val)
#subtitle_val <- round(subtitle_val,2)
#subtitle_val <- t(subtitle_val)
tiff(
  "./plots/relative_bias_slopes.tiff",
  width = 5,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)

{
  m <- matrix(
    1:12,
    ncol = 3,
    nrow = 4,
    byrow = TRUE
  )
  
  layout(m)
  par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
  mean_bias <- rep(NA, 12)
  for(i in 1:12){
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
    mean_bias[i] <- one_plot(
      x = tmp,
      yax = my_yax,
      xax = my_xax,
      param = parm_vec[i],
      zrange = c(-5,5),
      type = "rel_bias",
      return_mean = TRUE
    )
    if(i == m[1,3]){
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
    if(i == m[2,3]){
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
    if(i == m[3,3]){
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
    if(i == m[4,3]){
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
    if(i == m[1,1]){
      mtext(
        expression(underline("Autologistic")),
        side = 3,
        at = 0.17,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i == m[1,3]){
      mtext(
        expression(underline("Dynamic")),
        side = 3,
        at = 0.66,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i %in% m[,1]){
      mtext(
        substitute(
          paste(f1, Psi[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,2]){
      mtext(
        substitute(
          paste(f1, gamma[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,3]){
      mtext(
        substitute(
          paste(f1, epsilon[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
  }
  mtext(
    "Number of sites",
    side = 1,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 2.25
  )
  mtext(
    "Number of sampling seasons",
    side = 2,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 3
  )
  my_legend <- as.raster(matrix(pals::coolwarm(100), ncol = 100))
  par(xpd = NA)
  rasterImage(
    my_legend,
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5
  )
  
  
  rect(
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5,
    border = "black"
  )
  true_x <- seq(-125, 65, length.out = 100)
  x_val <- round(seq(-5, 5, length.out = 100),1)
  x_val[50] <- 0
  to_plot <- which(x_val%%1 == 0)
  for(i in 1:11){
    lines(
      x = rep(true_x[to_plot[i]],2),
      y = c(-8.5, -9.1)
    )
  }
  text(
    labels = c("< -5", "0", "> 5"),
    x = true_x[to_plot[c(1,6, 11)]],
    y = rep(-10.7, 3),
    cex = 1.6
  )
  mtext(
    "Relative bias (%)",
    side = 1,
    outer = TRUE,
    at = 0.5,
    line = 7.7,
    cex = 1
  )
}
dev.off()

# Averaged across scenarios and sample sizes, the relative bias
#  in the autologistic slope term was 4.2 times smaller than
#  the colonization slope term and 1.6 times smaller than
#  the extinction slope term. The relative bias in colonization
#  slope terms increased with the expected occupancy of the
#  species and was highest when the number of sites and sampling
#  seasons was low.

mm <- matrix(
  round(mean_bias,2),
  ncol =3 , 
  nrow = 4,
  byrow = TRUE
)

colMeans(mm)
### CI width ####

#### Figure 3 ####


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
tiff(
  "./plots/ci_width_intercepts.tiff",
  width = 6.4,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)

{
  m <- matrix(
    1:16,
    ncol = 4,
    nrow = 4,
    byrow = TRUE
  )
  
  layout(m)
  par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
  mean_bias <- rep(NA, 16)
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
    mean_bias[i] <- one_plot(
      x = tmp,
      yax = my_yax,
      xax = my_xax,
      param = parm_vec[i],
      type = "ci_width",
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
    if(i == m[1,1]){
      mtext(
        expression(underline("Autologistic")),
        side = 3,
        at = 0.25,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i == m[1,4]){
      mtext(
        expression(underline("Dynamic")),
        side = 3,
        at = 0.75,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i %in% m[,1]){
      mtext(
        substitute(
          paste(f1, Psi[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,2]){
      mtext(
        substitute(
          paste(f1, theta[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,3]){
      mtext(
        substitute(
          paste(f1, gamma[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,4]){
      mtext(
        substitute(
          paste(f1, epsilon[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
  }
  mtext(
    "Number of sites",
    side = 1,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 2.25
  )
  mtext(
    "Number of sampling seasons",
    side = 2,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 3
  )
  my_legend <- as.raster(matrix(pals::ocean.thermal(100), ncol = 100))
  par(xpd = NA)
  rasterImage(
    my_legend,
    xleft = -170,
    ybottom = -8.5,
    xright = 30,
    ytop = -5.5
  )
  
  
  rect(
    xleft = -170,
    ybottom = -8.5,
    xright = 30,
    ytop = -5.5,
    border = "black"
  )
  true_x <- seq(-170, 30, length.out = 100)
  x_val <- round(seq(0, 5, length.out = 100),1)
  to_plot <- which(x_val%%1 == 0)
  to_plot <- to_plot[c(1,2,4,6,8,10)]
  for(i in 1:6){
    lines(
      x = rep(true_x[to_plot[i]],2),
      y = c(-8.5, -9.1)
    )
  }
  text(
    labels = c("0", "1", "2", "3", "4", "> 5"),
    x = true_x[to_plot],
    y = rep(-10.7, 3),
    cex = 1.6
  )
  mtext(
    "95% Confidence interval width",
    side = 1,
    outer = TRUE,
    at = 0.5,
    line = 7.7,
    cex = 1
  )
}
dev.off()

# Average ci widths were slightly smaller for
#  autologistic model, but overall similar between
# the various models.

# Averaged across scenarios and sample sizes, average CI width for
#  the autologistic intercept was 1.86 times narrower than the 
#  dynamic colonization intercept and 1.3 times narrower than the
#  dynamic extinction intercept. The autologistic theta term, 
#  however, had greater uncertainty. The average CI width for
#  this term was roughly 1.09 times larger than the dynamic
#  colonization intercept and 1.6 times larger than the dynamic
#  extinction intercept. This difference was largely driven
#  by the 3rd and 4th scenario, which had relatively wide
#  CI widths for theta and gamma_0.

mm <- matrix(
  round(mean_bias,2),
  ncol =4 , 
  nrow = 4,
  byrow = TRUE
)

colMeans(mm)
#### Figure 4 ####




# get subsetting vectors together
model_vec <- rep(
  c("auto", "dynamic","dynamic"),
  4
)
scenario_vec <- rep(1:4, each = 3)
parm_vec <- rep(
  c("psi - x",
    "col(x)", "ext(x)"),
  4
)

subtitle <- LETTERS[1:12]
subtitle <- paste0(subtitle,") ")
#subtitle_val <- sim_list$targets$latent
#subtitle_val <- subtitle_val[,c("psi","theta", "gamma", "eps")]
#subtitle_val <- as.matrix(subtitle_val)
#subtitle_val <- round(subtitle_val,2)
#subtitle_val <- t(subtitle_val)
tiff(
  "./plots/ci_width_slopes.tiff",
  width = 5,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)

{
  m <- matrix(
    1:12,
    ncol = 3,
    nrow = 4,
    byrow = TRUE
  )
  
  layout(m)
  par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
  mean_bias <- rep(NA, 12)
  for(i in 1:12){
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
    mean_bias[i] <- one_plot(
      x = tmp,
      yax = my_yax,
      xax = my_xax,
      param = parm_vec[i],
      zrange = c(0,5),
      type = "ci_width",
      return_mean = TRUE
    )
    if(i == m[1,3]){
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
    if(i == m[2,3]){
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
    if(i == m[3,3]){
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
    if(i == m[4,3]){
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
    if(i == m[1,1]){
      mtext(
        expression(underline("Autologistic")),
        side = 3,
        at = 0.17,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i == m[1,3]){
      mtext(
        expression(underline("Dynamic")),
        side = 3,
        at = 0.66,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i %in% m[,1]){
      mtext(
        substitute(
          paste(f1, Psi[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,2]){
      mtext(
        substitute(
          paste(f1, gamma[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,3]){
      mtext(
        substitute(
          paste(f1, epsilon[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
  }
  mtext(
    "Number of sites",
    side = 1,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 2.25
  )
  mtext(
    "Number of sampling seasons",
    side = 2,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 3
  )
  my_legend <- as.raster(matrix(pals::ocean.thermal(100), ncol = 100))
  par(xpd = NA)
  rasterImage(
    my_legend,
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5
  )
  
  
  rect(
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5,
    border = "black"
  )
  true_x <- seq(-125, 65, length.out = 100)
  x_val <- round(seq(0, 5, length.out = 100),1)
  to_plot <- which(x_val%%1 == 0)
  to_plot <- to_plot[c(1,2,4,6,8,10)]
  for(i in 1:6){
    lines(
      x = rep(true_x[to_plot[i]],2),
      y = c(-8.5, -9.1)
    )
  }
  text(
    labels = c("0", "1", "2", "3", "4", "> 5"),
    x = true_x[to_plot],
    y = rep(-10.7, 3),
    cex = 1.6
  )
  mtext(
    "95% Confidence interval width",
    side = 1,
    outer = TRUE,
    at = 0.5,
    line = 7.7,
    cex = 1
  )
}
dev.off()

# Averaged across all scenarios and sample sizes, autologistic
#  slope term confidence interavls were 2.67 times narrower than
#  colonization slope term confidence intervals and 1.56 times
#  narrower than extinction slope term confidence intervals,
#  which the largest difference being between slope term
#  confidence intervals in the 4th scenario.


mm <- matrix(
  round(mean_bias,2),
  ncol =3 , 
  nrow = 4,
  byrow = TRUE
)

colMeans(mm)

### Coverage ####

#### Figure 5 ####



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
tiff(
  "./plots/coverage_intercepts.tiff",
  width = 6.4,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)

{
  m <- matrix(
    1:16,
    ncol = 4,
    nrow = 4,
    byrow = TRUE
  )
  
  layout(m)
  par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
  mean_bias <- rep(NA, 16)
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
    mean_bias[i] <- one_plot(
      x = tmp,
      yax = my_yax,
      xax = my_xax,
      param = parm_vec[i],
      type = "coverage",
      zrange = c(0.85,1),
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
    if(i == m[1,1]){
      mtext(
        expression(underline("Autologistic")),
        side = 3,
        at = 0.25,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i == m[1,4]){
      mtext(
        expression(underline("Dynamic")),
        side = 3,
        at = 0.75,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i %in% m[,1]){
      mtext(
        substitute(
          paste(f1, Psi[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,2]){
      mtext(
        substitute(
          paste(f1, theta[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,3]){
      mtext(
        substitute(
          paste(f1, gamma[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,4]){
      mtext(
        substitute(
          paste(f1, epsilon[0], " = ", f2)
          ,
          list(
            f1 = subtitle[i],
            f2 = subtitle_val[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
  }
  mtext(
    "Number of sites",
    side = 1,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 2.25
  )
  mtext(
    "Number of sampling seasons",
    side = 2,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 3
  )
  my_legend <- as.raster(matrix(pals::ocean.thermal(100), ncol = 100))
  par(xpd = NA)
  rasterImage(
    my_legend,
    xleft = -170,
    ybottom = -8.5,
    xright = 30,
    ytop = -5.5
  )
  
  
  rect(
    xleft = -170,
    ybottom = -8.5,
    xright = 30,
    ytop = -5.5,
    border = "black"
  )
  true_x <- seq(-170, 30, length.out = 100)
  #x_val <- round(seq(85, 100, length.out = 100),1)
  to_plot <- seq(-170, 30, length.out = 15)
 # to_plot <- which(x_val%%1 == 0)
  for(i in 1:length(to_plot)){
    lines(
      x = rep(to_plot[i],2),
      y = c(-8.5, -9.1)
    )
  }
  text(
    labels = c("0.85", "0.90", "0.95", "1"),
    x = to_plot[c(1, 6, 11, 15)],
    y = rep(-10.7, 3),
    cex = 1.6
  )
  mtext(
    "Coverage",
    side = 1,
    outer = TRUE,
    at = 0.5,
    line = 7.7,
    cex = 1
  )
}
dev.off()

# Average coverage (the proportion of simulations where the model
#  fit included the data generating parameter within it's 
#  95% CI) was high across all models and scenarios (mean = 0.95.5).

mm <- matrix(
  round(mean_bias,2),
  ncol =4 , 
  nrow = 4,
  byrow = TRUE
)
colMeans(mm)

#### Figure 6 ####





# get subsetting vectors together
model_vec <- rep(
  c("auto", "dynamic","dynamic"),
  4
)
scenario_vec <- rep(1:4, each = 3)
parm_vec <- rep(
  c("psi - x",
    "col(x)", "ext(x)"),
  4
)

subtitle <- LETTERS[1:12]
subtitle <- paste0(subtitle,") ")

tiff(
  "./plots/coverage_slopes.tiff",
  width = 5,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)

{
  m <- matrix(
    1:12,
    ncol = 3,
    nrow = 4,
    byrow = TRUE
  )
  
  layout(m)
  par(mar = c(0.75,0.75,1.2,.75), oma = c(9,6,2.5,8))
  mean_bias <- rep(NA, 12)
  for(i in 1:12){
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
    mean_bias[i] <- one_plot(
      x = tmp,
      yax = my_yax,
      xax = my_xax,
      param = parm_vec[i],
      zrange = c(0.85,1),
      type = "coverage",
      return_mean = TRUE
    )
    if(i == m[1,3]){
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
    if(i == m[2,3]){
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
    if(i == m[3,3]){
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
    if(i == m[4,3]){
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
    if(i == m[1,1]){
      mtext(
        expression(underline("Autologistic")),
        side = 3,
        at = 0.17,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i == m[1,3]){
      mtext(
        expression(underline("Dynamic")),
        side = 3,
        at = 0.66,
        outer = TRUE,
        line = 0.6
      )
    }
    if(i %in% m[,1]){
      mtext(
        substitute(
          paste(f1, Psi[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,2]){
      mtext(
        substitute(
          paste(f1, gamma[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
    if(i %in% m[,3]){
      mtext(
        substitute(
          paste(f1, epsilon[1])
          ,
          list(
            f1 = subtitle[i]
          )
        ),
        side = 3,
        at = 30,
        line = 0.15,
        cex = 0.8,
        adj = c(0)
      )
    }
  }
  mtext(
    "Number of sites",
    side = 1,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 2.25
  )
  mtext(
    "Number of sampling seasons",
    side = 2,
    outer = TRUE,
    at = 0.5,
    cex = 1.2,
    line = 3
  )
  my_legend <- as.raster(matrix(pals::ocean.thermal(100), ncol = 100))
  par(xpd = NA)
  rasterImage(
    my_legend,
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5
  )
  
  
  rect(
    xleft = -125,
    ybottom = -8.5,
    xright = 65,
    ytop = -5.5,
    border = "black"
  )
  to_plot <- seq(-125, 65, length.out = 15)
  # to_plot <- which(x_val%%1 == 0)
  for(i in 1:length(to_plot)){
    lines(
      x = rep(to_plot[i],2),
      y = c(-8.5, -9.1)
    )
  }
  text(
    labels = c("0.85", "0.90", "0.95", "1"),
    x = to_plot[c(1, 6, 11, 15)],
    y = rep(-10.7, 3),
    cex = 1.6
  )
  mtext(
    "Coverage",
    side = 1,
    outer = TRUE,
    at = 0.5,
    line = 7.7,
    cex = 1
  )
  
  
}
dev.off()

# Average coverage (the proportion of simulations where the model
#  fit included the data generating parameter within it's 
#  95% CI) was high across all models and scenarios (mean = 0.95.5).


mm <- matrix(
  round(mean_bias,2),
  ncol =3 , 
  nrow = 4,
  byrow = TRUE
)

colMeans(mm)