# Analysis of Plan Demographics
# Yimeng Yin
# 10/27/2015

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


#****************************************************************************************************
#               ## Combine selected files into a single list. ####
#****************************************************************************************************

IO_folder <- "IO_M2.1_new"

get_results <- function(IO_folder){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, pattern = "^Outputs_D1")
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder)

# save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))

# load(paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))





#****************************************************************************************************
#               ## Select runs  ####
#****************************************************************************************************

runs_all <- c(
 "average_gn2",
 "average_gn1",
 "average",
 "average_g1",
 "average_g2",
 

 "mature1_gn1",
 "mature1",
 
 "mature1_gn1_lowB",
 "mature1_lowB",
  
 "mature2_gn1",
 "mature2",
 
 "immature",
 "immature_g1"
 )
  




#****************************************************************************************************
#               ## Measures of FR risk and contribution volatility   ####
#****************************************************************************************************

get_metrics <- function(runs, year.max, prefix = "D1F075-", include.maxChg = FALSE){
  
#   runs = runs_all
#   year.max = 30
#   include.maxChg = FALSE
#   prefix = "D1F075-"
  
  df_TO <- results_all %>% filter(runname %in% paste0(prefix, runs), year <= year.max, sim > 0) %>% 
    select(runname, year, sim, FR_MA, ERC_PR, C_PR, C, ERC, PR)  

  ## Measures of funded status *********************************************
  
  # 1. Probability of funded ratio falling below X% in 5, 15, 30 years.  
  # 2. VaR-like measure:  5th percentile of funded ratio in year 5, 15, and 30.
  # 3. CVaR-like measure: weighted average of funded ratio below 5th percentile in year 5, 15, and 30. 
  #                       question: how to calculate the weight?  
  
  
  
  df_ruin <- 
    df_TO %>% group_by(runname, sim) %>% 
    mutate(FR50 = cumany(FR_MA  <= 50),
           FR40 = cumany(FR_MA  <= 40)) %>% 
    filter(year %in% c(15,30,40)) %>% 
    ungroup %>% group_by(runname, year) %>% 
    summarise(FR50 = 100 * sum(FR50)/n(),
              FR40 = 100 * sum(FR40)/n()) %>% 
    gather(variable, value, -runname, -year) %>% 
    mutate(variable = paste0(variable, "_y", year),
           year = NULL) %>% 
    spread(variable, value)
  
  df_ruin
  
  
  ## Measures of contribution volatility *************************************
  
  df_sd <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>% 
    group_by(sim, runname) %>% 
    summarise(C_PR.sd = sd(C_PR, na.rm = TRUE), ERC_PR.sd = sd(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)
  df_sd
  
  
  df_dsd <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>% 
    group_by(sim, runname) %>% 
    summarise(C_PR.dsd = sd(diff(C_PR), na.rm = TRUE), ERC_PR.dsd = sd(diff(ERC_PR), na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)
  df_dsd
  
  
  df_worst <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%  
    group_by(sim, runname) %>% 
    summarise(C_PR.max = max(C_PR), ERC_PR.max = max(ERC_PR)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)
  df_worst
  
  
  df_final <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%   
    group_by(sim, runname)    %>% 
    filter(year == max(year)) %>% 
    group_by(runname) %>% 
    summarise(C_PR.final   = median(C_PR), 
              ERC_PR.final = median(ERC_PR))
  df_final
  
  
  df_5yearChg <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%   
    group_by(sim, runname)  %>% 
    mutate_each(funs(. - lag(., 5)), one_of(c("C_PR", "ERC_PR")) ) %>% 
    summarise(C_PR.5yChg   = max(C_PR,   na.rm = TRUE),
              ERC_PR.5yChg = max(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)

  df_5yearChg
    
  # with(df_5yearChg, df_5yearChg[runname == "A1F075_O30pA5","C_PR.5yChg"] %>% unlist) %>% hist(breaks = 50)
  
  if(include.maxChg){
    ## Create functions to calculate max changes in 5-year intervals. 
    maxChgWithin <- function(y, fn, ...){
      # max/min change within a single interval.
      zoo::rollapply(y, rev(seq_along(y)), function(x) fn(x - x[1], ...), fill = NA, align = "left") %>% fn(., ...)
      #y <- outer(x, x, "-")
      #y[lower.tri(y)] %>% fn(., ...)  
    }
    
    roll_maxChg <- function(x, fun, width,  ... ){
      # For a given vector x, calculate the max/min change WITHIN each interval of the width "width" 
      zoo::rollapply(x, width, maxChgWithin, fn = fun, ...,  fill = NA, align = "right")
    }
    
    
    system.time(
      df_5yearMaxChg <- df_TO %>%
        select(runname, year, sim, C_PR, ERC_PR) %>%
        group_by(sim, runname)  %>% 
        mutate(C_PR  = roll_maxChg(C_PR,  max, 5),
               ERC_PR= roll_maxChg(ERC_PR,max, 5)) %>% 
        summarise(C_PR.5yMaxChg  = max(C_PR,   na.rm = TRUE),
                  ERC_PR.5yMaxChg= max(ERC_PR, na.rm = TRUE)) %>% 
        group_by(runname) %>% 
        summarise_each(funs(median)) %>% select(-sim)
    )
  }
  
  
  
  ## Present Value of contribution *************************************
  
  df_PVC <- df_TO %>% 
    select(runname, year, sim, C, ERC, PR) %>% 
    group_by(sim, runname) %>% 
    mutate(discount = 1/(1 + 0.075)^(year - 1),
           discount_L10 = ifelse(max(year) - year >= 10, 0, discount)) %>% 
    summarise(PV.C   = sum(C * discount),
              PV.ERC = sum(ERC * discount),
              PV.PR  = sum(PR * discount),
              PV.C_L10   = sum(C * discount_L10),
              PV.ERC_L10 = sum(ERC * discount_L10),
              PV.PR_L10  = sum(PR * discount_L10)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median), -sim) %>% 
    mutate(PV.C_PR   = 100 * PV.C / PV.PR,
           PV.ERC_PR = 100 * PV.ERC / PV.PR,
           PV.C_PR_L10   = 100 * PV.C_L10 / PV.PR_L10,
           PV.ERC_PR_L10 = 100 * PV.ERC_L10 / PV.PR_L10 )
  
  df_PVC
  
  df_metrics <- join_all(list(df_ruin, 
                              df_PVC,
                              df_sd,
                              df_dsd,
                              df_worst,
                              df_final,
                              df_5yearChg))
  
  if(include.maxChg) df_metrics %<>% left_join(df_5yearMaxChg)
  
  return(df_metrics)
  
}


df_metrics_y30 <- get_metrics(runs_all, 30) 


df_metrics_y30 %<>% select(runname, FR40_y30, ERC_PR.dsd, ERC_PR.5yChg, PV.ERC_PR, PV.ERC_PR_L10) 

df_metrics_y30 %>% filter(grepl("average", runname)) %>% kable(digits = 2)

df_metrics_y30 %>% filter(grepl("-mature", runname)) %>% kable(digits = 2)

df_metrics_y30 %>% filter(grepl("immature", runname)) %>% kable(digits = 2)


#****************************************************************************************************
#               ## Measures of FR risk and contribution volatility   ####
#****************************************************************************************************

# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.
var.display <- c("runname",
                 "year",  
                 "AL",    "AA",   "FR", "NC",    "SC", # "UAAL",
                 "AL_PR", "AL.act_PR", "AL.ret_PR",# "AL.term_PR", "AL.Ben_PR",
                 "NC_PR", #" NC.act_PR", "NC.term_PR", "MA_PR", 
                 #"AL_PR", "NC_PR", "SC_PR", "C_PR", "ERC_PR", 
                 "ExF_PR",   
                 "C", "B", "B.v", "B_PR","PR"    
)




runName_list <- c("D1F075-average", "D1F075-mature1", 
                  "D1F075-mature2", "D1F075-immature")

results_all %>% filter(runname %in% runName_list, sim == 0, year %in% c(1, 15, 30,60)) %>% 
                select(one_of(var.display))


results_all %>% filter(runname == "D1F075-immature", sim == 0, year %in% c(1, 15, 30,60)) %>% 
  select(one_of(var.display))






retirees  %>% filter(grepl(".fill", planname)) %>% 
              ggplot(aes(x = age, y = benefit, color = planname)) +
              geom_line() + 
              geom_point()

retirees %>% filter(planname == "WA-PERS2-119.fillin")







