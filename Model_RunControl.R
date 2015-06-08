## 5/28/2015


# Preamble ###############################################################################################

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(data.table)
# library(xlsx)
# library(XLConnect) # slow but convenient because it reads ranges
library(readxl)
library(stringr)
#library(corrplot)
#devtools::install_github("donboyd5/decrements")
#devtools::install_github("donboyd5/pp.prototypes")


source("Functions.R")

devMode <- FALSE # Enter development mode if true. Parameters and initial population will be imported from Dev_Params.R instead of the RunControl file. 

#*********************************************************************************************************


folder_run          <- "IO_Initial_Runs_1"
filename_RunControl <- "RunControl_initRuns.xlsx"


#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

# Import global parameters
Global_params <- read_excel(paste0(folder_run, "/" ,filename_RunControl), sheet="GlobalParams", skip=1) 


# Import parameters for all plans
plan_params  <- read_excel(filename_RunControl, sheet="RunControl", skip=4) %>% filter(!is.na(runname))
plan_returns <- read_excel(filename_RunControl, sheet="Returns",    skip=0) %>% filter(!is.na(runname))
plan_contributions <- read_excel(filename_RunControl, sheet="Contributions", skip=0) %>% filter(!is.na(runname))




#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

## select plans
runlist <- plan_params %>% filter(include == TRUE) %>% select(runname) %>% unlist
# runlist <- runlist[runlist == "R4F1"|runlist == "R4F1"|runlist == "R4F1"]
# runlist <- runlist[runlist == "average1"|runlist == "average3"]
# runlist <- runlist[runlist == "average3"]
runlist




## Run selected plans 

for (runName in runlist){

## Extract plan parameters 
paramlist    <- get_parmsList(plan_params, runName)
paramlist$plan_returns <- plan_returns %>% filter(runname == runName)
if(paramlist$exCon) paramlist$plan_contributions <- trans_cont(plan_contributions, runName) else 
                    paramlist$plan_contributions <- list(0) 


## Extract global parameters and coerce the number of simulation to 1 when using deterministic investment reuturns.
Global_paramlist <- Global_params %>% as.list
if ((paramlist$return_type == "simple" & paramlist$ir.sd == 0) |
    (paramlist$return_type == "internal" &  all(paramlist$plan_returns$ir.sd == 0))){
  
  Global_paramlist$nsim <- 1
  
}

## Run the model
source("Model_Master.R", echo = TRUE)
}


  












## Note on the size of output files
# load("Outputs/Outputs_average1_06-01-2015.RData")
# outputs_list$results %>% nrow
# object.size(outputs_list$results)/(1024*1024)
# 
# load("Outputs/Outputs_R6F1_06-04-2015.RData")
# outputs_list$results %>% nrow
# object.size(outputs_list$results)/(1024*1024)

# Object size in memory is roughly proportional to the number of sims while file size on disk is not.
# File size is significantly smaller for runs with fixed returns where all runs have identical values. (only 140+KB for 1000 runs). So the variation  
# So the .RData file is in a compressed format which compresses all repetitive values. 



