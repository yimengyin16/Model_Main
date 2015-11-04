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

run_select <- dir(IO_folder, pattern = "^Outputs_I")

results_all <- ldply(run_select, load_run, IO_folder)

results_all %<>% mutate(ExF_MA = 100 * ExF / MA)
save(results_all, file = paste0(IO_folder, "/Analysis_Investment/Investment_results.RData"))















