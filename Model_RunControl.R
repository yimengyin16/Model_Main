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

if(!file.exists("Outputs")) dir.create("Outputs")

filename_RunControl <- "RunControl(6).xlsx"
runName <- "average2"


# Import global parameters
Global_paramlist <- read_excel(filename_RunControl, sheet="GlobalParams", skip=1) %>% as.list


# Import plan parameters
plan_params  <- read_excel(filename_RunControl, sheet="RunControl", skip=4) %>% filter(!is.na(runname))
plan_returns <- read_excel(filename_RunControl, sheet="Returns",    skip=0) %>% filter(!is.na(runname))
plan_contributions <- read_excel(filename_RunControl, sheet="Contributions", skip=0) %>% filter(!is.na(runname))

paramlist    <- get_parmsList(plan_params, runName)
paramlist$plan_returns <- plan_returns %>% filter(runname == runName)


if(paramlist$exCon){
df_cont <- function(start, duration, pct) data.frame(year = start + 0:(duration - 1), pct_ADC = pct)
paramlist$plan_contributions <- with(plan_contributions %>% filter(runname == runName), 
                                     mapply(df_cont,
                                            start = start, duration = duration, pct = pct_ADC, SIMPLIFY = FALSE)) %>% 
                                bind_rows
} else paramlist$plan_contributions <- list(0)


# Rum the model
source("Model_Master.R", echo = TRUE)











