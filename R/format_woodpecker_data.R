
data("opossum_det_hist")
test <- format_y(
  opossum_det_hist,
  site_column = "Site",
  time_column = "Season",
  history_columns = "Week")
# read in the detection array

det_paths <- list.files(
  "./data/woodpecker/",
  pattern = "X.array.yr",
  full.names = TRUE
)

# get the numeric part of the file paths
numeric_part <- as.numeric(gsub("\\D", "", det_paths))

# Sort file_paths based on numeric part
det_paths <- det_paths[order(numeric_part)]

my_files <- lapply(
  det_paths,
  read.csv
)

nsite <- nrow(my_files[[1]])
nseason <- length(my_files)
nsurveys <- ncol(my_files[[1]])

y <- array(
  NA,
  dim = c(nsite, nseason, nsurveys)
)
na_count <- matrix(
  NA,
  ncol = nseason,
  nrow = nsite
)
for(i in 1:nseason){
  y[,i,] <- as.matrix(my_files[[i]])
  na_count[,i] <- rowSums(is.na(y[,i,]))
}

