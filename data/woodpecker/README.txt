This file contains information and explanation for the data that accompany the following project:

Stillman, A.N., R.L. Wilkerson, D.R. Kaschube, R.L. Wilkerson, R.B. Siegel, S.C. Sawyer, and M.W. Tingley. Incorporating pyrodiversity into 
wildlife habitat assessments for rapid and robust post-fire management.

This .README file accompanies the archived data for this project. Text files provide R code for three occupancy models described in 
the main text: static, temporal, and landscape-temporal. Individual .csv files are read into R scripts and reformatted for the model.
Continuous variables were standardized prior to modeling by substracting the mean and dividing by the standard deviation. Standardized 
variables are denoted by "S" before the variable name.  
__________________________________


Dataset: detection_covars.csv
ef = binary effort variable. 0 = 2 minute survey, 1 = 3 minute survey. 
itype = survey type. 0 = passive survey, 1 = broadcast survey. 

Dataset: fire_covars.csv
fireID = Unique ID assigned to each fire where surveys took place.
fire.name = Full name of the fire as assigned by USFS and management personell. 
S.fire.size = the size of the fire area, in hectares. The natural log was applied before standardization.
fire.season = binary value for ignition date. 0 = fire started before or on August 15th, 1 = fire started after August 15th. 

Dataset: S.fire.age.csv
Matrix with nrow = the number of survey points, and columns = 1 - 10 years post fire. Each row represents 1-10 years post-fire. 

Dataset: S.jday.csv
The standardized ordinal date of the woodpecker survey at each site. nrow = the number of survey points, and columns = 1 - 10 
years post fire. Years with no woodpecker surveys are given the average value after standardization (~0). 

Dataset: site_covars.csv
Site-level covariate data at each survey point.
S.elev = Elevation at the survey location (m).
S.lat = Latitude at the survey location (decimal degrees).
bs30 = Burn severity of the pixel that overlaps the survey location, measured as the % change in canopy cover
	from before to immediately after fire.
S.bs100 = Mean burn severity within 100m of the survey point. 
S.d.patch = Distance from the survey point to the nearest pixel that burned at low severity (<25% change in canopy cover).
S.pyro500 = Inverse Simpsons diversity of burn severity pixels (divided into 11 classes) within 500 m of the survey point. 
S.precc100 = Pre-fire canopy cover within 100 m of the survey point. 
S.fir100 = Combined basal area of red and white fir within 100 m of the survey point. 
whr = California Wildlife Habitat Relationships classification at the survey point.
site.code = Unique ID assigned to each survey location.
fireID = Unique ID assigned to each fire where surveys took place. Unique IDs are matched to fire names in the file "fire_covars.csv". 

Dataset: static_S.jday
The standardized ordinal date of the woodpecker survey at each site used in the static model. nrow = the number of survey points. 

Dataset: static_site_covars.csv
Site-level covariate data at each survey point used in the static model.
S.elev = Elevation at the survey location (m), included as the residuals of the linear model Elevation ~ Latitude.
S.lat = Latitude at the survey location (decimal degrees).
S.bs100 = Mean burn severity within 100m of the survey point. 
S.precc100 = Pre-fire canopy cover within 100 m of the survey point. 
whr = California Wildlife Habitat Relationships classification at the survey point.
whrsize = California Wildlife Habitat Relationships classification of tree size class, included as a binary indicator for the 
presence of large trees using the threshold WHR_size > 3. 
site.code = Unique ID assigned to each survey location.
fireID = Unique ID assigned to each fire where surveys took place. Unique IDs are matched to fire names in the file "fire_covars.csv".

Dataset: static_X.array
Response variable for the single-season static occupancy model. This dataset represents the detection history for each point.
Each row is a different survey point and each column is a different period of a single survey (see Methods).
NA = No survey conducted
0 = Black-backed Woodpecker not detected
1 = Black-backed Woodpecker detected

Datasets: X.array.yr*
These datasets provide the response variable for the model and represent the detection history for each point. Each row is a different survey point and 
each column is a different period of a single survey (see Methods). There are 10 .csv files, each representing a different year 1 - 10 years post-fire. 
NA = No survey conducted
0 = Black-backed Woodpecker not detected
1 = Black-backed Woodpecker detected
