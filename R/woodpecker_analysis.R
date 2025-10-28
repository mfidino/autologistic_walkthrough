# load packages ####

library(autoOcc)
library(bbplot)
library(pals)

# Woodpecker analysis

# Code written by M. Fidino, but this analysis
#  would not have been possible without all of
#  the great documentation provided in the 
#  Stillman et al. (2023) paper (not to mention the 
#  hard work of everyone who collected data over 
#  10 years across a huge number of locations).


# Stillman, A. N., Wilkerson, R. L., Kaschube, 
#  D. R., Siegel, R. B., Sawyer, S. C., 
#  & Tingley, M. W. (2023). 
#  Incorporating pyrodiversity into wildlife habitat 
#  assessments for rapid post-fire management:
#  a woodpecker case study. Ecological Applications, e2853.


# Create detection array ####

# Get file paths of all detection histories

det_paths <- list.files(
  "./data/woodpecker/",
  pattern = "X.array.yr",
  full.names = TRUE
)

# get the numeric part of the file paths
numeric_part <- as.numeric(
  gsub(
    "\\D",
    "",
    det_paths
  )
)

# Sort file_paths based on numeric part
det_paths <- det_paths[order(numeric_part)]

my_files <- lapply(
  det_paths,
  read.csv
)

# get number of sites, seasons, and surveys
nsite <- nrow(my_files[[1]])
nseason <- length(my_files)
nsurveys <- ncol(my_files[[1]])

# y array for modeling
y <- array(
  NA,
  dim = c(nsite, nseason, nsurveys)
)
# get counts of NA's just to get an idea of
#  data density
na_count <- matrix(
  NA,
  ncol = nseason,
  nrow = nsite
)

# fill in y array
for(i in 1:nseason){
  y[,i,] <- as.matrix(my_files[[i]])
  na_count[,i] <- rowSums(is.na(y[,i,]))
}

# Create detection design matrix ####

# Create detection covariate matrix. Both models use
# 1. Survey interval duration (binary: 2 min = 0, 3 min = 1)
# 2. Ordinal day of year (integer)
# 3. Survey type (binary: passive = 0, broadcast = 1)

# The first and third covariates are present within
#  ./data/woodpecker/detection_covars.csv
#  But it needs to get replicated across sites and surveys.

ddat <- read.csv("./data/woodpecker/detection_covars.csv")

# The second covariate is present within
#  ./data/woodpecker/S.jday.csv
#  and has already been standardized

jday <- read.csv("./data/woodpecker/S.jday.csv")

# Put all of these in a list
det_cov_list <- list(
  effort = matrix(
    ddat$ef,
    ncol = nseason,
    nrow = nsite,
    byrow = TRUE
  ),
  ord_day = as.matrix(
    jday
  ),
  survey_type = matrix(
    ddat$itype,
    ncol = nseason,
    nrow = nsite,
    byrow = TRUE
  )
)

# Fit models ####

# We are going to use three different autologistic models mostly following
#  what Stillman et al. (2023) did. The first will be a null model,
#  the second is a temporal model, and the third is a landscape temporal
#  model. However, we will keep the same detection level covariates
#  for all models. We will also use the 100m buffer as that appeared
#  to have the largest influence in their analysis.

model_paths <- c(
  null = "./data/woodpecker/null_model.RDS",
  temporal = "./data/woodpecker/temporal_model.RDS",
  landscape_temporal = "./data/woodpecker/landscape_temporal_model.RDS"
)

#--------------------#
## Fit null model ####
#--------------------#

if(!file.exists(model_paths["null"])){
  null_model <- auto_occ(
    ~effort + ord_day + survey_type ~ 1,
    y = y,
    det_covs = det_cov_list,
    level = 0.85
  )
  saveRDS(
    null_model,
    model_paths["null"]
  )
} else {
  null_model <- readRDS(model_paths["null"])
}


#-------------------------#
## Fit temporal model  ####
#-------------------------#

# The covariates for this model include

# 1. Years since fire (Integer)
# 2. Burn Severity (Proportion)
# 3. Pre-fire canopy cover (Proportion)
# 4. Elevation (continuous)
# 5. Latitude (continuous)
# 6. Elevation^2
# 7. Years since fire^2
# 8. Elevation * latitude interaction

# Let's construct the occupancy covariate list for this.

# Years since fire is located in
#  "./data/woodpecker/S.fire.age.csv

fire_age <-read.csv(
  "./data/woodpecker/S.fire.age.csv"
)

# many of the other covariates are in 
#  "./data/woodpecker/site_covars.csv

site_covars <- read.csv(
  "./data/woodpecker/site_covars.csv"
)

# T for temporal
occ_cov_list_T <- list(
  fire_age = fire_age,
  burn_sev = site_covars$S.bs100,
  pre_fire_cc = site_covars$S.precc100,
  elevation = site_covars$S.elev,
  latitude = site_covars$S.lat
)


if(!file.exists(model_paths["temporal"])){
  temporal_model <- auto_occ(
    ~effort + ord_day + survey_type 
    ~fire_age + burn_sev + pre_fire_cc + elevation * latitude +
      I(elevation^2) + I(fire_age^2),
    y = y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list_T,
    level = 0.85
  )
  saveRDS(
    temporal_model,
    model_paths["temporal"]
  )
} else {
  temporal_model <- readRDS(
    model_paths["temporal"]
  )
}



#----------------------------------#
## Fit landscape temporal model ####
#----------------------------------#

# This model includes the same covariates as the temporal model
#  plus a few additional covariates.

# The covariates in this model include:

# 1. Years since fire (Integer)
# 2. Burn Severity (Proportion)
# 3. Pre-fire canopy cover (Proportion)
# 4. Elevation (continuous)
# 5. Latitude (continuous)
# 6. Habitat classification (Categorical)
# 7. Elevation^2
# 8. Years since fire^2
# 9. Elevation * latitude interaction
# 10. Diversity in burn severity (continuous)
# 11. burn severity * years since fire interaction
# 12. Distance to low-severity edge (continuous)
# 13. Basal area of fir trees (continuous)
# 14. Basal area of fire trees * years since fire interaction

occ_cov_list_LT <- occ_cov_list_T

# fill in the new covariates
occ_cov_list_LT$burn_diversity <- site_covars$S.pyro500
occ_cov_list_LT$low_sev_dist <- site_covars$S.d.patch
occ_cov_list_LT$fir <- site_covars$S.fir100


if(!file.exists(model_paths["landscape_temporal"])){
  landscape_temporal_model <- auto_occ(
    ~effort + ord_day + survey_type 
    ~ burn_sev * fire_age + pre_fire_cc + elevation * latitude +
      I(elevation^2) + I(fire_age^2) +
      burn_diversity  + low_sev_dist +
      fir*fire_age,
    y = y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list_LT,
    level = 0.85
  )
  saveRDS(
    landscape_temporal_model,
    model_paths["landscape_temporal"]
  )
} else {
  landscape_temporal_model <- readRDS(
    model_paths["landscape_temporal"]
  )
}

# Clean up global environment a bit
rm(
  list = c(
    "ddat", "fire_age", "jday", "my_files",
    "na_count", "site_covars", 
    "det_paths", "i", "model_paths",
    "nseason", "nsite", "nsurveys",
    "numeric_part", "y"
  )
)

# Compare models with AIC ####

# As with the Stillman et al. paper,
#  the landscape temporal model has
#  the best relative fit.

model_list <- list(
  null = null_model,
  temporal = temporal_model,
  landscape_temporal = landscape_temporal_model
)

aic_results <- autoOcc::compare_models(
  model_list,
  digits = 2
)

# remove the other models as we will not need them
rm(
  list = c(
    "temporal_model", "null_model"
  )
)
# Plot some results from the best fit model ####

## Collect coefs ####
msum <- summary(
  landscape_temporal_model
)

# compare the regression coefficients to what was reported in 
#  Stillman et al. (2023)

stillman <- data.frame(
  coef = msum@psi$parameter,
  Est = NA,
  lower = NA,
  upper = NA
)

# dropping the habitat coefficients and the autologistic term
# as these were not reported.
stillman <- stillman[-grep("theta", stillman$coef),]

# Taken from supplemental of the paper

# Intercept -2.32 (-2.64, -2.04)
# Burn severity 0.29 (0.19, 0.39)
# Years since fire -0.74 (-0.84, -0.64)
# Pre-fire canopy cover 0.05 (-0.11, 0.21) 
# Elevation 1.23 (0.92, 1.55)
# Latitude 1.13 (0.81, 1.45)
# Elevation2 -0.26 (-0.52, 0.00)
# Years since fire2 0.10 (0, 0.19
# Diversity in burn severity 0.15 (0.04, 0.25)
# Distance to Edge -0.11 (-0.28, 0.05)
# Fir basal area  0.22 (0.09, 0.36)
# Burn severity * Years since fire -0.18 (-0.26, -0.1) 
# Latitude * Elevation -0.05 (-0.41, 0.32)
# Fire basal area * Years since fire 0.09 (0.00, 0.17)

stillman$Est <- c(
  -2.32, 0.29, -0.74, 0.05, 1.23, 1.13, -0.26, 0.10, 0.15, 
  -0.11, 0.22, -0.18, -0.05, 0.09
)
stillman$lower <- c(
  -2.64, 0.19, -0.84, -0.11, 0.92, 0.81, -0.52, 0, 0.04, -0.28,
  0.09, -0.26, -0.41, 0
)
stillman$upper <- c(
  -2.04, 0.39, -0.64, 0.21, 1.55, 1.45, 0.00, 0.19, 0.25, 0.05, 
  0.36, -0.1, 0.32, 0.17
)

# get my model coefs, put calculate 95% CI as I did 85% while modeling
mine <- data.frame(
  coef = msum@psi$parameter,
  Est = msum@psi$Est,
  lower = qnorm(
    0.025, msum@psi$Est, msum@psi$SE
  ),
  upper = qnorm(
    0.975, msum@psi$Est, msum@psi$SE
  )
)
mine <- mine[mine$coef %in% stillman$coef,]


# order them in a way that makes sense
my_ord <- c(1, 5, 7, 6, 13, 2, 3,8, 12, 11, 14, 
            4, 9, 10)
mine <- mine[my_ord,]
stillman <- stillman[my_ord,]

# generate some model predictions for:
# 1. Years since fire
# interaction between burn sev and years since fire

## years since fire preds ####

# To do that we need the original values of the covariates,
#  which I recieved from Andrew Stillman.
raw_site_covs <- read.csv(
  "./data/woodpecker/raw_covars/site_covars.csv"
)

raw_fire_covs <- read.csv(
  "./data/woodpecker/raw_covars/fire.age.sampled.csv"
)
# drop row numbering column
raw_fire_covs <- raw_fire_covs[,-which(colnames(raw_fire_covs) == "X")]

# get range of years since fire (one to 10)
range(raw_fire_covs, na.rm = TRUE)

# create prediction matrix
fire_vec <- seq(1, 10, length.out = 300)
fire_age_dm <- data.frame(
  matrix(
    0,
    ncol = length(landscape_temporal_model@occcovs),
    nrow = length(fire_vec)
  )
)

# add column names
colnames(fire_age_dm) <- names(landscape_temporal_model@occcovs)

fire_mu <- mean(
  unlist(raw_fire_covs),
  na.rm = TRUE
)
fire_sd <- sd(
  unlist(raw_fire_covs),
  na.rm = TRUE
)
# fill in fire_vec
fire_age_dm$fire_age <- fire_vec
fire_age_dm$fire_age <- (fire_age_dm$fire_age -  fire_mu) / fire_sd

fire_age_pred <- predict(
  landscape_temporal_model,
  type = "psi",
  newdata = fire_age_dm,
  seed = 156
)

## burn sev * years since fire preds####

range(raw_site_covs$bs100)

burn_sev_vec <- seq(0, 100, length.out = 300)

burn_sev_dm <- data.frame(
  matrix(
    0,
    ncol = length(landscape_temporal_model@occcovs),
    nrow = length(burn_sev_vec) * 2
  )
)
colnames(burn_sev_dm) <- names(landscape_temporal_model@occcovs)

# fill in burn severity and scale it
burn_sev_dm$burn_sev <- rep(
  burn_sev_vec,
  2
)

burn_sev_dm$burn_sev <- (
  burn_sev_dm$burn_sev - mean(raw_site_covs$bs100)
) / sd(raw_site_covs$bs100) 

# fill in min and max of fire age from scaled years since fire dm
fa <- c(1, 10)
fa <- (fa - fire_mu) / fire_sd
burn_sev_dm$fire_age <- rep(
  fa,
  each = length(burn_sev_vec)
)

burn_sev_pred <- predict(
  landscape_temporal_model,
  type = "psi",
  newdata = burn_sev_dm,
  seed = 154
)

# add the covariate values
burn_sev_pred$burn_sev <- burn_sev_vec
burn_sev_pred$fire_age <- rep(c(1,10), each = length(burn_sev_vec))

# split by fire age
burn_sev_pred <- split(
  burn_sev_pred,
  factor(burn_sev_pred$fire_age)
)

## make plot ####
tiff(
  "./plots/woodpecker_figure.tiff",
  width = 7,
  height = 5,
  units = "in",
  res = 600,
  compression = "lzw"
)
{
m <- matrix(
  c(1, 1, 2, 3),
  ncol = 2,
  nrow = 2
)
layout(m)
par(mar = c(2,1,1,2), oma = c(2,13,1.5,0.5), lend = 2)

bbplot::blank(
  ylim = c(0.5,14.5),
  xlim = c(-3, 3),
  xaxs = "i",
  yaxs = "i",
  bty = "l"
)
abline(v = 0, lty = 2, col = "gray", lwd = 2)
plot_cols <- pals::ocean.thermal(100)
plot_cols <- plot_cols[c(1,50)]
for(i in 1:nrow(stillman)){
  lines(
    x = c(stillman$lower[i], stillman$upper[i]),
    y = rep(14.85-i,2),
    col = plot_cols[1],
    lwd = 3
  )

  lines(
    x = c(mine$lower[i], mine$upper[i]),
    y = rep(15.15-i,2),
    col =  plot_cols[2],
    lwd = 3
  )
}
points(
  x = stillman$Est,
  y = rev(c(1:nrow(stillman))) - 0.143,
  pch = 21, 
  bg = plot_cols[1],
  cex = 1.2
)
points(
  x = mine$Est,
  y = rev(c(1:nrow(stillman))) + 0.143,
  pch = 21, 
  bg = plot_cols[2],
  cex = 1.2
)

pnames <- c(
  "Intercept", "elevation", NA,
  "latitude", "elevation x latitude",
  "burn severity", "years since fire",
  NA, "burn severity x years since fire",
  "fir basal area", "fir basal area x years since fire",
  "pre-fire canopy cover", "burn severity diversity",
  "distance to edge")

bbplot::axis_text(
  text = pnames,
  side = 2,
  line = 0.25,
  las = 1,
  at = rev(1:14)
)
mtext(
  expression("elevation"^2),
  side =2,
  line = 0.25,
  las = 1,
  at = 12
)
mtext(
  expression("years since fire"^2),
  side =2,
  line = 0.25,
  las = 1,
  at = 7
)
bbplot::axis_blank(side = 1)
bbplot::axis_text(side = 1, line = 1)
bbplot::axis_text(
  "Logit-scale estimates",
  side = 1,
  line = 2.5
)
par(xpd = NA)
legend(
  x = -1.5, y = 16.1,
  legend = c("autoOcc", "Stillman et al."),
  fill = rev(plot_cols),
  bty = "n"
)
par(xpd = FALSE)


bbplot::blank(
  xlim = c(1,10),
  ylim = c(0,1),
  bty = "l"
)
bbplot::axis_blank(1)
bbplot::axis_blank(2)
bbplot::axis_text(side =1, line = 0.4)
bbplot::axis_text(side = 2, line = 0.4, las = 1)
bbplot::axis_text("Occupancy", side = 2, line = 2.1)
bbplot::axis_text("Years since fire", side = 1, line = 1.6)

bbplot::ribbon(
  x = fire_vec,
  y = fire_age_pred[,c("lower", "upper")],
  col = plot_cols[1],
  alpha = 0.5
)
lines(
  x = fire_vec,
  y = fire_age_pred$estimate,
  col = plot_cols[1],
  lwd = 2
)


bbplot::blank(
  xlim = c(0,100),
  ylim = c(0,1),
  bty = "l"
)
bbplot::axis_blank(1)
bbplot::axis_blank(2)
bbplot::axis_text(side =1, line = 1)
bbplot::axis_text(side = 2, line = 0.4, las = 1)
bbplot::axis_text("Occupancy", side = 2, line = 2.1)
bbplot::axis_text("Burn severity", side = 1, line = 2.5)

bbplot::ribbon(
  x = burn_sev_vec,
  y = burn_sev_pred$`1`[,c("lower", "upper")],
  col = plot_cols[2],
  alpha = 0.5
)
lines(
  x = burn_sev_vec,
  y = burn_sev_pred$`1`$estimate,
  col = plot_cols[2],
  lwd = 2
)

bbplot::ribbon(
  x = burn_sev_vec,
  y = burn_sev_pred$`10`[,c("lower", "upper")],
  col = plot_cols[1],
  alpha = 0.5
)
lines(
  x = burn_sev_vec,
  y = burn_sev_pred$`10`$estimate,
  col = plot_cols[1],
  lwd = 2
)

legend(
  "topleft",
  legend = c("1 year post-fire", "10 years post-fire"),
  fill = rev(plot_cols),
  bty = "n"
)
}
dev.off()
