# This script looks at
  # 1. the compound annual return for simulations with median funded ratio.
  # 2. funded ratios of various demographic scenarios when 30-year compound annual return meets the earnings assumption. 


#*****************************************************
##  Packages and Functions  ####
#*****************************************************


library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(magrittr) # to use %<>%
library(zoo)
library(grid)
library(gridExtra)
library(stringr)
library(xlsx)

source("Functions.R")
source("Functions_Measures.R")


#*****************************************************
##  Defining paths for inputs and outputs ####
#*****************************************************
IO_folder       <- "IO_M2.1_new"
outputs.folder  <- "/M2.1a_outputs/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Loading and pre-processing simulation outputs
# This section will produce two .RData file under the folder "Analysis_Demo"
# 1. Demo_results_all.RData. Data frame "results_all" that contains results for pension finances. 
# 2. DemoSum_all.RData".     Various data frames that contains demographic data. 

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}
# results_all <- get_results(IO_folder, "^Outputs_D") # This will take a while because of the large size of output files that contain demographic data
# save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))

## Outputs of demographics
#source("IO_M2.1_new/Report_M2.1a_LoadingDemoData.R")


## Loading existing data. 
load(paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))




#***********************************************************************
##  Compound annual returns of simulations with median funded ratio ####
#***********************************************************************

runs_demo.all <- c("D1F075-average_gn2",
                   #"D1F075-average_gn1",
                   "D1F075-average",
                   #"D1F075-average_g1",
                   "D1F075-average_g2",
                   "D1F075-mature1_gn1",
                   "D1F075-mature2_gn1",
                   "D1F075-immature_g1")

results_all %>% filter(sim > 0, runname %in% runs_demo.all) %>% 
  group_by(runname, sim) %>% 
  mutate(cumGeoReturn = cumprod(1 + i.r)^(1/seq_along(i.r)) - 1) %>% 
  ungroup() %>% 
  group_by(runname, year) %>% 
  arrange(runname, year, FR_MA) %>% 
  mutate(FR_order = seq_along(FR_MA)) %>% 
  select(runname, year, sim, i.r, cumGeoReturn, FR_MA, FR_order) %>% 
  filter(FR_order == 501) %>% 
  ggplot(aes(x = year, y = cumGeoReturn, color = runname)) + 
  geom_line() + geom_point() +
  geom_hline(yintercept = 0.075)+
  coord_cartesian(ylim = c(0.0, 0.15)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.01))

# The compound annual returns of simulations with median funded ratios mostly 
# concentrates in the range of 7% to 8% (assumption +-0.5%)


#*************************************************************************************************
##  Funded ratio of various demographic scenarios when earnings assumption is met in 30 years ####
#*************************************************************************************************

results_all %>% filter(sim %in% c(56, 228), year <=30, runname %in% runs_demo.all) %>% 
  ggplot(aes(x = year, y = FR_MA, color = runname)) + facet_grid(.~sim) + 
  geom_point() + geom_line()

# The order of FR is largely the same as that of the risk of FR falling below 40%. 


















