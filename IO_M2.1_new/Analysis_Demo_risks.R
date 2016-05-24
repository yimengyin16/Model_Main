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
#               ## Loading data ####
#****************************************************************************************************

IO_folder <- "IO_M2.1_new"
load(paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))
load(paste0(IO_folder, "/Analysis_Demo/Demo_metrics.RData"))

#****************************************************************************************************
#               ## Measures of FR risk and contribution volatility   ####
#****************************************************************************************************

df_metrics_y30


df_metrics_y30 %<>% select(runname, FR40_y30, ERC_PR.5yMaxChg, PV.ERC_PR) 

df_metrics_y30 %>% filter(grepl("average", runname)) %>% kable(digits = 2)

df_metrics_y30 %>% filter(grepl("-mature", runname)) %>% kable(digits = 2)

df_metrics_y30 %>% filter(grepl("immature", runname)) %>% kable(digits = 2)




#****************************************************************************************************
#               ## Simulation results  ####
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

runName_list <- c("D1F075-average", "D1F075-mature1", # "D1F075-mature1_lowB", 
                  "D1F075-mature2", "D1F075-immature")

runName_list <- c("D1F075-average", "D1F075-mature1_gn1", # "D1F075-mature1_lowB", 
                  "D1F075-mature2", "D1F075-immature_g1")

runName_list <- c("D1F075-average", "D1F075-average_g2", "D1F075-average_gn2")

results_all %>% filter(runname %in% runName_list, sim == 0, year %in% c(1,15,30,60)) %>% 
                select(one_of(var.display))


results_all %>% filter(runname %in% runName_list, sim == 0, year %in% c(1, 15, 30,60)) %>% 
  select(one_of(var.display))



#****************************************************************************************************
#               ## Detective work on LA-CERA: Initial Benefit.  ####
#****************************************************************************************************
load("Data/2015-10-07/retirees.rda")

retirees  %>% filter(grepl(".fill", planname)) %>% 
              ggplot(aes(x = age, y = benefit, color = planname)) +
              geom_line() + 
              geom_point()

retirees %>% filter(planname == "WA-PERS2-119.fillin")


#****************************************************************************************************
#               ## Detective work on WA-PERS2: new entrants  ####
#****************************************************************************************************

load("Data/2015-10-07/actives.rda")

actives %>% filter(planname == "OH-PERS-85", age - ea <= 4)

actives %>% filter(planname == "OH-PERS-85.fillin.yos", age - ea <= 0, ea >=20) %>% arrange(ea) %>% 
            group_by(ea) %>% 
            summarise(nactives = sum(nactives)) %>% 
            qplot(x = ea, y = nactives, data =., geom = c("line","point"))

actives %>% mutate(year.start = 1 - (age - ea)) %>% filter(planname == "OH-PERS-85.fillin.yos") %>%  
            filter(ea >=20, age - ea <= 4 ) %>% arrange(ea) %>% 
  group_by(ea) %>% 
  summarise(nactives = sum(nactives)) %>% 
  qplot(x = ea, y = nactives, data =., geom = c("line","point"))







#****************************************************************************************************
#               ## Detective work Salary scale  ####
#****************************************************************************************************

load("Data/2015-10-07/salgrowth.rda"); salgrowth %<>% mutate(age = NULL)

salgrowth %>% filter(grepl("yos", planname)) %>% 
  qplot(x = yos, y = salgrowth, color = planname, data = ., geom = c("line","point"))

# Verified: high salary growth lead to high normal cost 






#****************************************************************************************************
#              ## Tables of Measures of FR risk and contribution volatility for the report   ####
#****************************************************************************************************

df_metrics_y30 %<>% select(runname, FR40_y30, ERC_PR.5yMaxChg, PV.ERC_PR) 


runname_average <- c("D1F075-average_gn1", "D1F075-average", "D1F075-average_g1",
                     "D1F075-mature1_gn1", "D1F075-mature2_gn1",
                     "D1F075-immature_g1")


tab.risk <- df_metrics_y30 %>% filter(runname %in% runname_average) 

tab.risk %<>% left_join(results_all %>% filter(runname %in% runname_average, sim == 0, year %in% c(1,30)) %>%
                          select(runname, year, MA_PR, ExF_PR) %>% 
                          gather(variable, value, - runname, -year) %>% 
                          mutate(variable = paste0(variable,"_y",year), year = NULL) %>%
                          spread(variable, value))




tab.risk %>% mutate(runname = factor(runname, levels = runname_average)) %>% 
             arrange(runname) %>%
             select(runname, ERC_PR.5yMaxChg, MA_PR_y1, MA_PR_y30, FR40_y30, ExF_PR_y1, ExF_PR_y30, PV.ERC_PR) %>% 
             kable(digits = 1)


