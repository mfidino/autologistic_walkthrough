library(autoOcc)

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

if(!file.exits(model_paths["null"])){
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
# 6. Habitat classification (Categorical)
# 7. Elevation^2
# 8. Years since fire^2
# 9. Elevation * latitude interaction

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
  latitude = site_covars$S.lat,
  habitat_class = factor(site_covars$whr)
)

if(!file.exists(model_paths["temporal"])){
  temporal_model <- auto_occ(
    ~effort + ord_day + survey_type 
    ~fire_age + burn_sev + pre_fire_cc + elevation * latitude +
      habitat_class + I(elevation^2) + I(fire_age^2),
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
    ~ burn_sev + pre_fire_cc + elevation * latitude +
      habitat_class + I(elevation^2) + I(fire_age^2) +
      burn_diversity * fire_age + low_sev_dist +
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

# Plot some results from the best fit model ####







