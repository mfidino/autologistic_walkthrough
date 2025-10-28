library(autoOcc)
library(bbplot)
library(lubridate)
library(dplyr)
library(pals)

# load opossum detection / non-detection data ####
data("opossum_det_hist")

head(opossum_det_hist)

# format the detection data as it is currently
#  in long format.
opossum_y <- autoOcc::format_y(
  x = opossum_det_hist,
  site_column = "Site",
  time_column = "Season",
  history_columns = "^Week"
)

# TEMPORAL ORDERING
# -----------------
#   
#   Primary sampling period column is a character vector, 
#   using their order of appearance from top of x to order temporally.
#   Ordering: JA19, AP19, JU19, OC19
# 
# DETECTION HISTORIES
# -------------------
#   
#   4 detection history columns found.
# Column names: Week_1, Week_2, Week_3, Week_4

# Covariates ####
data("opossum_covariates")

# query only impervious and income
opossum_covariates <- opossum_covariates[,
  grep("Impervious|Income", colnames(opossum_covariates))
]

# and scale them
occ_cov_list <- as.data.frame(
  lapply(
    opossum_covariates,
    function(x){
      if(is.numeric(x)){
        scale(x)
      }else{
        x
      }
    }
  )
)


# Read in weather data and summarize it

weather <- read.csv(
  "./data/opossum/ohare_weather.csv"
)

weather$DATE <- lubridate::ymd(
  weather$DATE
)

# drop days > 28
to_go <- lubridate::day(
  weather$DATE
) > 28

weather <- weather[-which(to_go),]

weather$DAY <- lubridate::day(
  weather$DATE
)

tmp <- data.frame(
  DAY = 1:28,
  WEEK = rep(1:4, each = 7)
)
# tack on week
weather <- dplyr::inner_join(
  weather,
  tmp,
  by = "DAY"
)

# get month
weather$MONTH <- lubridate::month(
  weather$DATE
)

weather <- weather %>% 
  dplyr::group_by(MONTH, WEEK) %>% 
  dplyr::summarise(
    temp = mean(TAVG)
)

# subset to sampling months
weather <- weather[
  weather$MONTH %in% c(1, 4, 7, 10),
]

# sort by month then week

# For a covariate that varies by primary and secondary sampling season
#  we need a matrix that is dim(opossum_y)[1] by prod(dim(opossum_y)[2:3])
#  within a list

det_cov_list <- list(
  Temperature = matrix(
    rep(
      as.numeric(
        scale(
          weather$temp
        )
      ),
      each = dim(opossum_y)[1]
    ),
    nrow = dim(opossum_y)[1],
    ncol = prod(
      dim(opossum_y)[2:3]
    )
  ),
  Impervious = occ_cov_list$Impervious,
  Income = occ_cov_list$Income
)

# Fit models ####

model_paths <- c(
   global_quadratic = "./data/opossum/global_quadratic.RDS",
  global = "./data/opossum/global.RDS",
  income_quadratic = "./data/opossum/income_quadratic.RDS",
  income_quad_imperv = "./data/opossum/income_quadratic_imperv.RDS",
  income = "./data/opossum/income.RDS",
  imperv_quadratic = "./data/opossum/imperv_quadratic.RDS",
  imperv_quad_income = "./data/opossum/imperv_quadratic_income.RDS",
  imperv = "./data/opossum/imperv.RDS",
  temperature = "./data/opossum/temperature.RDS",
  null = "./data/opossum/null.RDS"
)

## Global quadratic ####

if(!file.exists(
  model_paths["global_quadratic"]
)){
  global_quadratic <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
     Impervious + I(Impervious^2) +
     Income + I(Income^2)
    ~Impervious + I(Impervious^2) +
     Income + I(Income^2),
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    global_quadratic,
    model_paths["global_quadratic"]
  )
} else {
  global_quadratic <- readRDS(
    model_paths["global_quadratic"]
  )
}

## Global ####

if(
  !file.exists(
    model_paths["global"]
  )
){
  global <- autoOcc::auto_occ(
    ~Temperature  +  I(Temperature^2) + Impervious + Income 
    ~Impervious  + Income,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    global,
    model_paths["global"]
  )
} else {
  global <- readRDS(
    model_paths["global"]
  )
}

## Income quadratic ####


if(!file.exists(
  model_paths["income_quadratic"]
)){
  income_quadratic <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Income + I(Income^2)
    ~ Income + I(Income^2),
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    income_quadratic,
    model_paths["income_quadratic"]
  )
} else {
  income_quadratic <- readRDS(
    model_paths["income_quadratic"]
  )
}

## Income quadratic imperv ####

if(!file.exists(
  model_paths["income_quad_imperv"]
)){
  income_quad_imperv <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Income + I(Income^2) + Impervious
    ~ Income + I(Income^2) + Impervious,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    income_quad_imperv,
    model_paths["income_quad_imperv"]
  )
} else {
  income_quad_imperv <- readRDS(
    model_paths["income_quad_imperv"]
  )
}

## Income ####

if(!file.exists(
  model_paths["income"]
)){
  income <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Income 
    ~ Income,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    income,
    model_paths["income"]
  )
} else {
  income <- readRDS(
    model_paths["income"]
  )
}

## Imperv quadratic ####

if(!file.exists(
  model_paths["imperv_quadratic"]
)){
  imperv_quadratic <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Impervious + I(Impervious^2)
    ~ Impervious + I(Impervious^2),
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    imperv_quadratic,
    model_paths["imperv_quadratic"]
  )
} else {
  imperv_quadratic <- readRDS(
    model_paths["imperv_quadratic"]
  )
}


## Imperv ####
if(!file.exists(
  model_paths["imperv"]
)){
  imperv <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Impervious  
    ~ Impervious,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    imperv,
    model_paths["imperv"]
  )
} else {
  imperv <- readRDS(
    model_paths["imperv"]
  )
}

## Imperv quadratic income

if(!file.exists(
  model_paths["imperv_quad_income"]
)){
  imperv_quad_income <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) +
      Impervious + I(Impervious^2) + Income
    ~ Impervious + I(Impervious^2) + Income,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  saveRDS(
    imperv_quad_income,
    model_paths["imperv_quad_income"]
  )
} else {
  imperv_quad_income <- readRDS(
    model_paths["imperv_quad_income"]
  )
}

## Temperature ####
if(
  !file.exists(
    model_paths["temperature"]
  )
){
  temp <- autoOcc::auto_occ(
    ~Temperature + I(Temperature^2) 
    ~1 ,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  
  saveRDS(
    temp,
    model_paths["temperature"]
  )
} else {
  temp <- readRDS(
    model_paths["temperature"]
  )
}

## null ####
if(
  !file.exists(
    model_paths["null"]
  )
){
  null <- autoOcc::auto_occ(
    ~1
    ~1 ,
    y = opossum_y,
    det_covs = det_cov_list,
    occ_covs = occ_cov_list
  )
  
  saveRDS(
    null,
    model_paths["null"]
  )
} else {
  null <- readRDS(
    model_paths["null"]
  )
}


# Compare models ####

model_list <- list(
  global_quadratic = global_quadratic,
  global = global,
  income_quadratic = income_quadratic,
  income_quad_imperv = income_quad_imperv,
  income = income,
  imperv_quadratic = imperv_quadratic,
  imperv_quad_income = imperv_quad_income,
  imperv = imperv,
  temperature = temp,
  null = null
)

aic_results <- autoOcc::compare_models(
  model_list,
  digits = 2
)


# Plot some results ####

msum <- summary(
  imperv_quad_income
)

## Occupancy ####

### Impervious prediction ####

range(opossum_covariates$Impervious)
imperv_vec <- seq(20, 80, length.out = 300)

imperv_dm <- data.frame(
  matrix(
    0,
    ncol = length(imperv_quad_income@occcovs),
    nrow = length(imperv_vec)
  )
)

# add column names
colnames(imperv_dm) <- names(imperv_quad_income@occcovs)

# add in imperv and scale it in the same way as we 
#  did in the model
imperv_dm$Impervious <- (
  imperv_vec - mean(opossum_covariates$Impervious)
) / sd(opossum_covariates$Impervious)


imperv_pred <- predict(
  object = imperv_quad_income,
  type = "psi",
  newdata = imperv_dm,
  seed = 453
)

### Income prediction ####

range(opossum_covariates$Income)
income_vec <- seq(30000, 150000, length.out = 300)

income_dm <- data.frame(
  matrix(
    0,
    ncol = length(imperv_quad_income@occcovs),
    nrow = length(income_vec)
  )
)

# add column names
colnames(income_dm) <- names(imperv_quad_income@occcovs)

# add in income and scale it in the same way as we 
#  did in the model
income_dm$Income <- (
  income_vec - mean(opossum_covariates$Income)
) / sd(opossum_covariates$Income)


income_pred <- predict(
  object = imperv_quad_income,
  type = "psi",
  newdata = income_dm,
  seed = 453
)

## Detection ####

### Temperature ####
range(weather$temp)

temp_vec <- seq(-10, 25, length.out = 300)

temp_dm <- data.frame(
  matrix(
    0,
    ncol = length(imperv_quad_income@detcovs),
    nrow = length(temp_vec)
  )
)

# add column names
colnames(temp_dm) <- names(imperv_quad_income@detcovs)

# add in temp and scale it in the same way as we 
#  did in the model
temp_dm$Temperature <- (
  temp_vec - mean(weather$temp)
) / sd(weather$temp)


temp_pred <- predict(
  object = imperv_quad_income,
  type = "rho",
  newdata = temp_dm,
  seed = 453
)

## Make plot ####

# get a color
plot_cols <- pals::ocean.thermal(100)
plot_cols <- plot_cols[c(30)]


tiff(
  "./plots/opossum_figure.tiff",
  width = 3.5,
  height = 7,
  units = "in",
  res = 600,
  compression = "lzw"
)
{
m <- cbind(1:3)
layout(m)
par(mar = c(5,7,1,1), lend = 2)

### Impervious - occupancy ####
bbplot::blank(
  ylim = c(0,1),
  xlim = range(imperv_vec),
  bty = "l"
)
bbplot::axis_blank(1)
bbplot::axis_blank(2)
bbplot::axis_text(
  side = 1,
  line = 0.75
)
bbplot::axis_text(
  side = 2,
  line = 1,
  las = 1
)
bbplot::axis_text(
  "Impervious cover (%)",
  side = 1,
  line = 3.25,
  cex = 1.2
)
bbplot::axis_text(
  "Occupancy",
  side = 2,
  line = 4,
  cex = 1.2
)
bbplot::ribbon(
  x = imperv_vec,
  y = imperv_pred[,c("lower","upper")],
  col = plot_cols,
  alpha = 0.3
)
lines(
  x = imperv_vec,
  y = imperv_pred$estimate,
  col = plot_cols,
  lwd = 3
)

## Income - occupancy ####
bbplot::blank(
  ylim = c(0,1),
  xlim = range(income_vec) / 10000,
  bty = "l"
)
bbplot::axis_blank(1)
bbplot::axis_blank(2)
bbplot::axis_text(
  text = seq(3,15,3),
  side = 1,
  line = 0.75,
  at = seq(3,15,3)
)
bbplot::axis_text(
  side = 2,
  line = 1,
  las = 1
)
bbplot::axis_text(
  "Per capita income ($10K)",
  side = 1,
  line = 3.25,
  cex = 1.2
)
bbplot::axis_text(
  "Occupancy",
  side = 2,
  line = 4,
  cex = 1.2
)
bbplot::ribbon(
  x = income_vec / 10000,
  y = income_pred[,c("lower","upper")],
  col = plot_cols,
  alpha = 0.3
)
lines(
  x = income_vec / 10000,
  y = income_pred$estimate,
  col = plot_cols,
  lwd = 3
)

## Temperature - Detection ####
bbplot::blank(
  ylim = c(0,1),
  xlim = range(temp_vec),
  bty = "l"
)
bbplot::axis_blank(1)
bbplot::axis_blank(2)
bbplot::axis_text(
  side = 1,
  line = 0.75
)
bbplot::axis_text(
  side = 2,
  line = 1,
  las = 1
)
mtext(
  expression("Mean weekly temp. ("*~degree*C*")"),
  side = 1,
  line = 3.25,
  cex = 1.2,
  at = mean(temp_vec)
)


bbplot::axis_text(
  "Detection",
  side = 2,
  line = 4,
  cex = 1.2
)
bbplot::ribbon(
  x = temp_vec,
  y = temp_pred[,c("lower","upper")],
  col = plot_cols,
  alpha = 0.3
)
lines(
  x = temp_vec,
  y = temp_pred$estimate,
  col = plot_cols,
  lwd = 3
)
}
dev.off()


