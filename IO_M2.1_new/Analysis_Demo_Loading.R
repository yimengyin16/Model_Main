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

run_select <- dir(IO_folder, pattern = "^Outputs_D1")

results_all <- ldply(run_select, load_run, IO_folder)

results_all %<>% mutate(ExF_MA = 100 * ExF / MA)
save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))
 


 
#****************************************************************************************************
#               ## 2. Measures of FR risk and contribution volatility   ####
#****************************************************************************************************
# Needs results_all

 runs_all <- c(
   "average_gn2",
   "average_gn1",
   "average",
   "average_g1",
   "average_g2",
   
   
   "mature1_gn1",
   "mature1",
   
   #"mature1_gn1_lowB",
   #"mature1_lowB",
   
   "mature2_gn1",
   "mature2",
   
   "immature",
   "immature_g1"
 )
 
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
 
  df_metrics_y30 <- get_metrics(runs_all, 30, include.maxChg = TRUE) 
 
  save(df_metrics_y30, file = paste0(IO_folder, "/Analysis_Demo/Demo_metrics.RData"))
 
 
#****************************************************************************************************
#               ## 3. Function for loading a single run ####
#****************************************************************************************************

  get_demoSum <- function(fileName, folder = IO_folder){
  

  #               ## Loading Data  ####
  #****************************************************************************************************

  load(paste0(folder, "/", fileName))
  
    
  #               ## Computing remaining working life and remaining lifetime.   ####
  #****************************************************************************************************
  
  get_expLife <- function(p, age){
    # get expected remaining work life/life time.
    # p: probability of survival at age x
    len <- length(age)
    exp.life <- numeric(len)
    
    for(i in seq_along(age)){
      p.i   <- p[i:len]
      age.i <- age[i:len]  
      
      exp.life[i] <- sum(cumprod(p.i) * c( 1 - p.i[-1], 0) * seq_along(age.i)) # prob of quitting the workforce at age x * age x
    }
    
    return(exp.life)
  }
  
  
  df_workLife <- expand.grid(ea = 20:74, age = 20:74) %>% 
    filter(age >= ea) %>% 
    left_join(outputs_list$decrement) %>% 
    select( ea, age, pxT) %>% 
    group_by(ea) %>% 
    mutate(workLife = get_expLife(pxT, age))
  
  df_workLife.sum <- outputs_list$ind_active %>% 
    left_join(df_workLife) %>% group_by(year) %>% 
    summarise(workLife.avg = weighted.mean(workLife, number.a, na.rm = TRUE))
  
  #df_workLife.sum %>% print(n = 60)                   
  
  
  df_lifeTime <- expand.grid(ea = 20, age = 50:120) %>% 
    left_join(outputs_list$decrement) %>% 
    select(age, pxm) %>% 
    mutate(lifeTime = get_expLife(pxm, age))
  
  df_lifeTime.sum <- outputs_list$ind_retiree %>% 
    left_join(df_lifeTime) %>% group_by(year) %>% 
    summarise(lifeTime.avg = weighted.mean(lifeTime, number.r, na.rm = TRUE))
  
  #df_lifeTime.sum %>% print(n = 60)                   
  
  
  
  #               ## Summary measures   ####
  #****************************************************************************************************
  # To be done:
  # - more flow measures for terms. 
  
  df_demo_summary <- outputs_list$demo_summary %>% 
    left_join(df_workLife.sum) %>% 
    left_join(df_lifeTime.sum)
  

  
  #               ## AL by age and ea ####
  #****************************************************************************************************
  
  df_AL_sx <- outputs_list$ind_active %>% 
    select(runname, year, ea, age, number.a, sx, ALx, ALx.v) %>% 
    mutate(AL_sx = 100 * (ALx + ALx.v) / sx) %>% 
    filter(year == 30, age < 75)
  
  
  df_NC_sx <- outputs_list$ind_active %>% 
    select(runname, year, ea, age, number.a, sx, NCx, NCx.v) %>% 
    mutate(NC.av_sx = 100 * (NCx + NCx.v) / sx,
           NC.a_sx  = 100 * NCx / sx,
           NC.v_sx  = 100 * NCx.v / sx) %>% 
    filter(year == 30, year == 30, age == ea, ea %in% 20:74 )
  
  #df_AL_sx
  #df_NC_sx
  
  
  #               ## AL-payroll ratio ####
  #****************************************************************************************************
  
  df_AL.ratios <- outputs_list$results %>% filter(sim == 1) %>% 
    select(runname, year, AL_PR, AL.act_PR, AL.ret_PR, AL.Ben_PR,  AL.term_PR)
  
  # df_AL.ratios %>% kable(digits = 3)
  
  
  #               ## Descriptive statistics of the demo runs. ####
  #****************************************************************************************************
  
  # runName <- "average"
  # load(paste0(IO_folder,"/Outputs_D1F075-",runName,".RData"))
  
  # Distribution of Age
  
  pop.actives <- outputs_list$ind_active %>% select(runname, year, ea, age, number.a)
  pop.retirees <- outputs_list$ind_retiree %>% select(runname, year, ea, age, number.r)
  
  df_ageDist <- pop.actives %>% group_by(year, age) %>% 
    summarize(nactives_age = sum(number.a)) %>% 
    mutate(pct_age =100* nactives_age / sum(nactives_age),
           runname = outputs_list$paramlist$runname )
  
  df_eaDist <- pop.actives %>% group_by(year, ea) %>% 
    summarize(nactives_age = sum(number.a)) %>% 
    mutate(pct_age =100* nactives_age / sum(nactives_age),
           runname = outputs_list$paramlist$runname ) %>% rename(age = ea)
  
  df_yosDist <- pop.actives %>% mutate(yos = age - ea) %>% group_by(year, yos) %>% 
    summarize(nactives_age = sum(number.a)) %>% 
    mutate(pct_age =100 * nactives_age / sum(nactives_age),
           runname = outputs_list$paramlist$runname )
  
  
  df_entDist <- data.frame(runname = outputs_list$paramlist$runname, 
                           age =  outputs_list$paramlist$range_ea,
                           pct_age = outputs_list$entrant_dist * 100)
  
  
  df_ageDist.r <- pop.retirees %>% group_by(year, age) %>% 
    filter(age >= outputs_list$paramlist$r.min) %>% 
    summarize(nretirees_age = sum(number.r)) %>% 
    mutate(pct_age =100* nretirees_age / sum(nretirees_age),
           runname = outputs_list$paramlist$runname) 
  
  
  save(df_demo_summary, 
       df_AL_sx,
       df_NC_sx,
       df_AL.ratios,
       pop.actives,
       pop.retirees,
       df_ageDist,
       df_eaDist,
       df_yosDist,
       df_entDist,
       df_ageDist.r,
       file = paste0(IO_folder,"/Analysis_Demo/DemoSum_", outputs_list$paramlist$runname, ".RData"))
  
  invisible()
}

  # get_demoSum("Outputs_D1F075-average.RData")


#****************************************************************************************************
#               ## Calculate summary results for demographics runs ####
#****************************************************************************************************

file_select <- dir(IO_folder, pattern = "^Outputs_D1")
# file_select <- file_select[1:2]
file_select
for (i in seq_along(file_select)){
  get_demoSum(file_select[i])
}

#****************************************************************************************************
#               ## loading all demographics runs ####
#****************************************************************************************************

folder <- paste0(IO_folder,"/Analysis_Demo/")
file_select <- dir(folder, pattern = "DemoSum_D1")

df_demo_summary_all <- list(0) 
df_AL_sx_all        <- list(0) 
df_NC_sx_all        <- list(0) 
df_AL.ratios_all    <- list(0)

df_ageDist_all <- list(0)
df_eaDist_all  <- list(0)
df_yosDist_all <- list(0)
df_entDist_all <- list(0)
df_ageDist.r_all <- list(0)

# load(paste0(folder,"DemoSum_D1F075-average.RData"))



for (i in seq_along(file_select)){
  load(paste0(folder, file_select[i]))
  
  df_demo_summary_all[[i]] <- df_demo_summary 
  df_AL_sx_all[[i]]        <- df_AL_sx
  df_NC_sx_all[[i]]        <- df_NC_sx
  df_AL.ratios_all[[i]]    <- df_AL.ratios
  
  df_ageDist_all[[i]]   <- df_ageDist
  df_eaDist_all[[i]]    <- df_eaDist
  df_yosDist_all[[i]]   <- df_yosDist  
  df_entDist_all[[i]]   <- df_entDist
  df_ageDist.r_all[[i]] <- df_ageDist.r
}

df_demo_summary_all %<>% rbind_all
df_AL_sx_all        %<>% rbind_all
df_NC_sx_all        %<>% rbind_all
df_AL.ratios_all    %<>% rbind_all

df_ageDist_all    %<>% rbind_all 
df_eaDist_all     %<>% rbind_all 
df_yosDist_all    %<>% rbind_all 
df_entDist_all    %<>% rbind_all 
df_ageDist.r_all  %<>% rbind_all 


save(df_demo_summary_all,
     df_AL_sx_all,      
     df_NC_sx_all,      
     df_AL.ratios_all,  
     
     df_ageDist_all,    
     df_eaDist_all,     
     df_yosDist_all,    
     df_entDist_all,    
     df_ageDist.r_all, file = paste0(IO_folder, "/Analysis_Demo/DemoSum_all.RData"))

