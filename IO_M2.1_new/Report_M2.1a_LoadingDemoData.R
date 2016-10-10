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



#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************
runs_demo <- c("D1F075-average_gn2",
               "D1F075-average",
               "D1F075-average_g2",
               "D1F075-mature1_gn1",
               "D1F075-mature2_gn1",
               "D1F075-immature_g1")

runs_demo_labels <- c("Average, 2% decline",
                      "Average, constant workforce",
                      "Average, 2% growth",
                      "Mature plan 1 (high asset-payroll ratio)",
                      "Mature plan 2 (high normal cost)",
                      "Immature plan")

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

