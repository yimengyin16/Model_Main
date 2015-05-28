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
library(xlsx)
library(XLConnect) # slow but convenient because it reads ranges
library(readxl)
library(stringr)
#library(corrplot)
#devtools::install_github("donboyd5/decrements")
#devtools::install_github("donboyd5/pp.prototypes")

# source the following file when running the program for the first time. 
# source("Inputs_Import_Winklevoss.R")

source("Functions.R")

dev_mode <- FALSE # Enter development mode if true. Parameters and initial population will be imported from Dev_Params.R instead of the RunControl file. 

#*********************************************************************************************************

if(!file.exists("Outputs")) dir.create("Outputs")

filename_RunControl <- "RunControl(6).xlsx"



# Import global parameters
Global_paramlist <- read_excel(filename_RunControl, sheet="GlobalParams", skip=1) %>% as.list


# Import plan parameters
plan_params <- read_excel("RunControl(6).xlsx", sheet="RunControl", skip=4) %>% filter(!is.na(runname))
paramlist   <- get_parmsList(plan_params, "average1")


# Rum the model
source("Model_Master.R", echo = TRUE)











