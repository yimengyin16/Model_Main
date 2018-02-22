## Run control file for pension simulation model
## 8/2017


#*********************************************************************************************************
#                                               Preamble ####
#*********************************************************************************************************

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
library(readxl)
library(stringr)
library(xlsx)
# library(XLConnect) # slow but convenient because it reads ranges
# devtools::install_github("donboyd5/decrements")
# devtools::install_github("donboyd5/pp.prototypes")

library(pp.prototypes)
library(decrements)               # mortality and termination for now


# Initial actives and retirees 
  # Load data for new prototypes before they are in the pp.prototypes package
load("Data/2015-10-07/actives.rda")
load("Data/2015-10-07/retirees.rda") 


# Decrements
load("Data/2015-10-07/retrates.rda");  retrates %<>% dplyr::rename(qxr = retrate)
# load("Data/retrates_AZ.RData"); retrates <- retrate_AZ
load("Data/2015-10-07/termrates.rda"); termrates %<>% dplyr::rename(qxt = termrate) # %>% mutate(qxt = 0.5*qxt)
load("Data/2015-10-07/mortality.rda")
load("Data/winklevossdata.rdata") # disability, disability mortaity and early retirement

# Salary scale
load("Data/2015-10-07/salgrowth.rda"); salgrowth %<>% mutate(age = NULL)


source("Functions.R")

devMode <- FALSE # Enter development mode if true. Parameters and initial population will be imported from Dev_Params.R instead of the RunControl file. 

#retirees %<>% mutate(nretirees = 0)
#actives %<>% mutate(nactives = 0) 
#actives
#retrates %<>% mutate(qxr = ifelse(age == 65, 1, 0)) 




#*********************************************************************************************************
#                      ## Calibration of decrements  ####
#*********************************************************************************************************

# Calibrate term rates, mortality rates and retirement rates to approximately match workforce flows of AZ-PERS
# in 2013.  

termrates %<>% mutate(qxt = 1.2 * qxt)

mortality %<>% mutate(qxm = 0.6 * qxm) %>% 
               mutate(qxm.r = qxm)

retrates %<>% mutate(qxr = qxr * 0.7) 



#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************


folder_run <- "IO_M2.1_new" 
# folder_run <- "IO_M1_new"
# folder_run <- "IO_M2.1history_new" 
 
filename_RunControl <- dir(folder_run, pattern = "^RunControl")

path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)


# Import global parameters
Global_params <- read_excel(path_RunControl, sheet="GlobalParams", skip = 1) 


# Import parameters for all plans
plan_params        <- read_excel(path_RunControl, sheet="RunControl",    skip=4) %>% filter(!is.na(runname))
plan_returns       <- read_excel(path_RunControl, sheet="Returns",       skip=0) %>% filter(!is.na(runname))
plan_contributions <- read_excel(path_RunControl, sheet="Contributions", skip=0) %>% filter(!is.na(runname))


#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

## select plans
runlist <- plan_params %>% filter(include == TRUE) %>% select(runname) %>% unlist
# runlist <- runlist[runlist == "R4F3"]
# runlist <- runlist[runlist == "average1"|runlist == "average3"]
# runlist <- runlist[runlist == "average3"]
runlist


## Run selected plans 

for (runName in runlist){

suppressWarnings(rm(paramlist, Global_paramlist))
  
## Extract plan parameters 
paramlist    <- get_parmsList(plan_params, runName)
paramlist$plan_returns <- plan_returns %>% filter(runname == runName)
if(paramlist$exCon) paramlist$plan_contributions <- trans_cont(plan_contributions, runName) else 
                    paramlist$plan_contributions <- list(0) 


## Extract global parameters and coerce the number of simulation to 1 when using deterministic investment reuturns.
Global_paramlist <- Global_params %>% as.list
if ((paramlist$return_type == "simple" & paramlist$ir.sd == 0) |
    (paramlist$return_type == "internal" &  all(paramlist$plan_returns$ir.sd == 0))|
    (paramlist$return_type == "external")){

  Global_paramlist$nsim <- 1
}

## Run the model
# source("Model_Master.R", echo = TRUE)
}




