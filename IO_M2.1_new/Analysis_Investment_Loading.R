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

run_select <- dir(IO_folder, pattern = "^Outputs_I|^Outputs_B")

results_all <- ldply(run_select, load_run, IO_folder)

results_all %<>% mutate(MA_B   = 100 * MA / B)

save(results_all, file = paste0(IO_folder, "/Analysis_Investment/InvBen_results.RData"))



#****************************************************************************************************
#               ## 2. Measures of FR risk and contribution volatility   ####
#****************************************************************************************************
# Needs results_all

runs_investment <- c(paste0("I1F075-", c(1:8, "6a")),
                     paste0("I2F075-", 1:8),
                     paste0("I3F075-", 1:8),
                     paste0("I4F075-", 1:5),
                     paste0("I5F075-", 1:5),
                     paste0("I6F075-", c(1:4, 6:8, "6a")),
                     paste0("I7F075-", 1:8))

# runs_investment <- c(paste0("I2F075-", c(1:5,6:8)))
# df_metrics_I6_y30 <- get_metrics(runs_investment, 30)
# df_maxChg_I6_y30  <- get_metrics_maxChg(runs_investment, 30) 
# load(file = paste0(IO_folder, "/Analysis_Investment/InvBen_metrics.RData"))
# df_metrics_Inv_y30 <- rbind(df_metrics_Inv_y30, df_metrics_I6_y30)
# df_maxChg_Inv_y30  <- rbind(df_maxChg_Inv_y30, df_maxChg_I6_y30)


runs_benefit    <- c(paste0("BF075-", 1:2))

df_metrics_Inv_y30 <- get_metrics(runs_investment, 30, "I7F075-1")
df_maxChg_Inv_y30  <- get_metrics_maxChg(runs_investment, 30)  


df_metrics_Ben_y30 <- get_metrics(runs_benefit, 30,  "I7F075-1") 
df_maxChg_Ben_y30  <- get_metrics_maxChg(runs_benefit, 30) 

save(df_metrics_Inv_y30,
     df_maxChg_Inv_y30,
     
     df_metrics_Ben_y30,
     df_maxChg_Ben_y30,
     
     file = paste0(IO_folder, "/Analysis_Investment/InvBen_metrics.RData"))

df_metrics_Inv_y30 %>% kable
df_metrics_Ben_y30 %>% kable




