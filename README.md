# A repository for:

---

Fidino, M. Estimating species occupancy across multiple sampling seasons with autologistic occupancy models via the `autoOcc` R package. *Journal of Animal Ecology*.

## Links to different parts of the readme file

1. [What's in this repository?](#whats-in-this-repository)
2. [The working directory](#the-working-directory)
3. [The data folder (`./data`)](#the-data-folder-data)
4. [The plots folder (`./plots`)](#-the-plots-folder-plots)
5. [The R folder (`./R`)](#the-r-folder-r)
6. [R session information](#r-session-information)

## What's in this repository?

This working directory houses all the data and code for the simulation study and two case studies in this manuscript. The code for `autoOcc` is housed in it's own repository, which can be [found here](https://github.com/mfidino/autoocc). 

You should look through this repository if you are interested in seeing how I set simulations up, summarised the results from said simulations, or how I carried out the analyses for the case studies.

[Back to table of contents ⤒](#a-repository-for)

## The working directory

Aside from the aforementioned folders, the working directory here stores the `.gitignore` file for this repository, this README file (`README.md`), and the `.Rproj` file (for if you are using RStudio, `autologistic_walkthrough.Rproj`).

[Back to table of contents ⤒](#a-repository-for)

## The data folder

The data folder has two sub-folders (one for each case study). Aside from this, there is a results file for the simulation study titled `./data/sim_sweep_rmse.csv`. This file has 2080 rows and 11 columns.

`./data/sim_sweep_rmse.csv` metadata

| Column | Data-type | Description |
|---|---|---|
| parameter | categorical | The associated parameter across the simulation scenarios. For the autologistic model, parameters are split between the latent state model with psi and the detection model with rho. For the dynamic model, parameters are split between psi, col, and ext for the latent state (initial occupancy, colonizations, and extinction) and p for the detection model. Within each, thee are intercepts (Int) and the slope term for the simulated covariate (x). Each simulation scenario will have multiple rows, one per parameter. |
| rmse | numeric | The root mean square error of the parameter estimate. |
| coverage | numeric | The proportion of simulations where the true simulated parameter value was within the 95% CI of the parameter estimate. |
| ci_width | numeric | The absolute difference in the lower and upper 95% confidence interval of the associated parameter estimate. |
| signif | numeric | The proportion of simulations where the p value of the estimated parameter was statistically significant at the a = 0.5 level. |
| rel_bias | numeric | The relative bias of the parameter estimate. |
| nsite | integer | The number of sites for the simulation. |
| nseason | integer | The number of seasons for the simulation. |
| model | categorical | The type of model fit. Either auto or dynamic. |
| scenario | categorical | The simulation scenario. There are four, which essentially modify how common the simulated species was. The expected equilibrium occupancy for the species ranged from 0.2 to 0.5 across simulations, increasing by 0.1 per simulation. Scenario 1 = expected equilibrium occupancy of 0.2. |
| nconverged | integer | The number of simulated datasets that converged when analyzed. A total of 550 simulations were done for each simulation scenario. |

### The `./data/opossum` sub-folder

This sub-folder only contains one dataset: `./data/opossum/ohare_weather.csv`, which contains 304 rows and 6 columns. 

`./data/opossum/ohare_weather.csv` metadata

| Column | Data-type | Description |
|---|---|---|
| STATION | categorical | The code for the weather station used, which was at O'Hare international airport. |
| NAME | categorical | The name of the weather station. |
| LATITUDE | numeric | The latitude of the weather station in decimal degrees. |
| LONGITUDE | numeric | The longitude of the weather station in decimal degrees. |
| DATE | date (m/d/yyyy) | The date the weather measurement was taken. |
| TAVG | Celcius | The average temperature for the day in degrees Celcius. |

Weather data came from:

```
National Climatic Data Center [NCDC]. 2023. National Oceanic and Atmospheric Administration National Climatic Data Center. <https://www.ncdc.noaa.gov/cdo-
```

### The `./data/woodpecker` sub-folder

Data for the woodpecker analysis came from 

```
 Stillman, A. N., Wilkerson, R. L., Kaschube, D. R., Siegel, R. B., Sawyer, S. C., 
 & Tingley, M. W. (2023). Incorporating pyrodiversity into wildlife habitat assessments for rapid post-fire management: a woodpecker case study. Ecological Applications, e2853.
```

This sub-folder has many data files, but all of them are already documented within the `./data/woodpecker/README.txt` file, which I refer the interested reader to. In general though, this contains all the observational data that they collected as well as environmental covariates used in the analysis.

I greatly thank Andrew Stillman and Morgan Tingley for their conversations while I was analyzing their data and recreating the analysis (and also have this data be publicly available). I did have to request the raw environmental covariates for my case-study analysis, which they provided, and are present in the `./data/woodpecker/raw_covars` sub-folder. The readme file and associated manuscript have enough relevant information for a person to understand those files.

[Back to table of contents ⤒](#a-repository-for)

## The plots folder

This folder contains the six associated figures in the manuscript. You can look to the manuscript to see their figure legends.

Figure 1: `./plots/relative_bias_intercepts.tiff`
Figure 2: `./plots/relative_bias_slopes.tiff`
Figure 3: `./plots/ci_width_intercepts.tiff`
Figure 4: `./plots/ci_width_slopes.tiff`
Figure 5: `./plots/opossum_figure.tiff`
Figure 6: `./plots/woodpecker_figure.tiff`

[Back to table of contents ⤒](#a-repository-for)

## The R folder

This folder has all the R code to:

1. generate simualtions, fit them, and plot out the results.
2. Analyze the data from the opossum case study and plot out the results.
3. Analyze the data from the woodpecker case study and plot out the results.

Instead of describing the files in this folder in alphabetical order, I will describe them based on their purpose.

[Back to table of contents ⤒](#a-repository-for)

### Simulations

To carry out the simulation study I wrote 7 scripts. To fit the simulations, only `./R/fit_simualtion_sweep.R` needs to be ran. To summarise the simulations, `./R/pull_coefs_sweep.R` needs to be ran. Finally, to plot out the results, `./R/plot_rmse_sweep.R` needs to be ran. A description of all 7 scripts are below.

| Script | Description | Sourced by |
|---|---|---|
| `./R/fit_simulation_sweep.R` | Fits models to simulated data across all scenarios and models, saving the output. | Nothing. |
| `./R/simulate.R` | Functions to simulate data for autologistic and dynamic occupancy models. | `./R/simulation_sweep.R` |
| `./R/simulation_sweep.R` | Gets the pieces together to simulate data for a given scenario and dataset, which is then passed to `./R/simulate.R` to be generated. This script will save  all the simulations in a file `./data/sim_sweep.RDS`. If it has been created, the simulations will not be carried out again and instead `./data/sim_sweep.RDS` will be  read in. As a result, this script often gets sourced to bring in the associated simulated data to be used to generate results and plot them out (in addition to fitting the models).  | `./R/fit_simulation_sweep.R`,  `./R/plot_rmse_sweep.R`,  `./R/pull_coefs_sweep.R` |
| `./R/simulation_utils.R` | Has functions to sets up the simulated datasets to be ran in parallel as well as utility functions to summarise results | `./R/fit_simulation_sweep.R`, `./R/pull_coefs_sweep.R` |
| `./R/pull_coefs_sweep.R` | Generates the results file in the data sub-folder to plot out the results from the simulation study. | Nothing. |
| `./R/plot_rmse_sweep.R` | Generates all of the simulation study figures. I use `bbplot` to generate this plots, which can be  found here: https://github.com/dapperstats/bbplot. | Nothing |
| `./R/plot_utilities.R` | Has some helpful utility functions for plotting. | `./R/plot_rmse_sweep.R` |

### Opossum case study

The data to fit this is made available within `autoOcc` and the associated help file within the R package provides sufficient metadata. The script `./R/opossum_analysis.R` does the following steps:

1. Loads data and formats it for analysis.
2. Fits a suite of autologistic occupancy models (and saves them).
3. Compares the models using AIC.
4. Makes a figure from the model with the best relative fit. 


### Woodpecker case study

The data to fit this is within the `./data/woodpecker` sub-folder (described above). The script `./R/woodpecker_analysis.R` does the following steps:

1. Loads data and formats it for analysis. 
2. Fits the three models used in the case study.
3. Compares the models using AIC.
4. Makes a figure from the model with the best relative fit. 

[Back to table of contents ⤒](#a-repository-for)

## R session information

This is the session information that I most recently used to do this analysis.
```
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8 
[2] LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/Chicago
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices
[5] utils     datasets  methods   base     

other attached packages:
 [1] unmarked_1.4.3    doParallel_1.0.17
 [3] iterators_1.0.14  foreach_1.5.2    
 [5] dplyr_1.1.4       lubridate_1.9.3  
 [7] pals_1.9          bbplot_0.0.1     
 [9] autoOcc_0.1.1     mvtnorm_1.3-3    

loaded via a namespace (and not attached):
 [1] vctrs_0.6.5       cli_3.6.5        
 [3] rlang_1.1.6       generics_0.1.4   
 [5] glue_1.8.0        colorspace_2.1-1 
 [7] mapproj_1.2.11    grid_4.4.1       
 [9] tibble_3.2.1      MASS_7.3-60.2    
[11] lifecycle_1.0.4   compiler_4.4.1   
[13] codetools_0.2-20  Rcpp_1.0.14      
[15] timechange_0.3.0  pkgconfig_2.0.3  
[17] rstudioapi_0.17.1 maps_3.4.2       
[19] lattice_0.22-6    R6_2.6.1         
[21] dichromat_2.0-0.1 tidyselect_1.2.1 
[23] pillar_1.10.2     magrittr_2.0.3   
[25] tools_4.4.1  

```

[Back to table of contents ⤒](#a-repository-for)