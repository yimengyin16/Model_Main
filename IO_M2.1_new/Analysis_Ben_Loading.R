rm(list = ls())

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
library("readr")
library("readxl")
library(xlsx)

source("Functions.R") 

IO_folder <- "IO_M2.1_new/"


#****************************************************************************************************
#               ## 1. Loading simulation results ####
#****************************************************************************************************

load_run <- function(runname, folder){
  load(paste0(folder, runname))
  outputs_list$results
}

run_select <- dir(IO_folder, pattern = "^Outputs_B")

results_all <- ldply(run_select, load_run, IO_folder)

results_all %<>% mutate(MA_B   = 100 * MA / B)

save(results_all, file = paste0(IO_folder, "/Analysis_Ben/Ben_results.RData"))



#****************************************************************************************************
#               ## 2. Measures of FR risk and contribution volatility   ####
#****************************************************************************************************
# Needs results_all

runs_benefit    <- c(paste0("BF075-", c(0:3, "3a") ))

df_metrics_Ben_y30 <- get_metrics(runs_benefit, 30,  "BF075-1") 
df_maxChg_Ben_y30  <- get_metrics_maxChg(runs_benefit, 30) 

save(df_metrics_Ben_y30,
     df_maxChg_Ben_y30,
     
     file = paste0(IO_folder, "/Analysis_Ben/Ben_metrics.RData"))



df_metrics_Ben_y30 %>% kable




