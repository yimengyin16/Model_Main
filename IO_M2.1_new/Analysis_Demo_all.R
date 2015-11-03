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


IO_folder <- "IO_M2.1_new"
load(paste0(IO_folder, "/Analysis_Demo/DemoSum_all.RData"))
load(paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))

#****************************************************************************************************
#               ## Summary statistics ####
#****************************************************************************************************

df_demo_summary_all %>% select(runname, year, ends_with("avg"))%>% filter(year %in% c(1, 30)) %>% print(n = 1e10)
    
df_AL.ratios_all %>% filter(year %in% c( 30)) %>% print(n = 1e10) 

df_demo_summary_all


#****************************************************************************************************
#               ## Individual AL ####
#****************************************************************************************************

df_AL_sx_all %>% filter(runname == "D1F075-average", ea %in% seq(0,65, 10)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea))) + 
  geom_line() + geom_point()

df_AL_sx_all %>% filter(runname == "D1F075-mature1_gn1", ea %in% seq(0,70,5)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea))) + 
  geom_line() + geom_point()

df_AL_sx_all %>% filter(runname == "D1F075-mature2_gn1", ea %in% seq(0,70,5)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea))) + 
  geom_line() + geom_point()

df_AL_sx_all %>% filter(runname == "D1F075-immature_g1", ea %in% seq(0,70,5)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea))) + 
  geom_line() + geom_point()



#****************************************************************************************************
#               ## Individual NC ####
#****************************************************************************************************

runName_list <- c("D1F075-average", "D1F075-mature1", # "D1F075-mature1_lowB", 
                  "D1F075-mature2", "D1F075-immature")

df_NC_sx_all %>% filter(runname %in% runName_list) %>% 
  ggplot(aes(x = ea, y = NC.av_sx, color = runname)) + geom_line() + geom_point()

df_NC_sx_all 
df_NC_sx_all %>% filter(runname == "D1F075-average") %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + geom_bar(stat = "identity")


df_NC_sx_all %>% filter(runname == "D1F075-mature1_gn1", ea <= 65) %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + geom_bar(stat = "identity")


df_NC_sx_all %>% filter(runname == "D1F075-mature2_gn1") %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + geom_bar(stat = "identity")

df_NC_sx_all %>% filter(runname == "D1F075-immature_g1") %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + geom_bar(stat = "identity")



#****************************************************************************************************
#               ## Descriptive statistics of plans ####
#****************************************************************************************************

runName <- "D1F075-mature2"
runName_list <- c("D1F075-average",
                  "D1F075-mature1_gn1", 
                  "D1F075-mature2_gn1", 
                  "D1F075-immature_g1")


df_ageDist_all %>% filter(runname %in% runName_list, year %in% c(1,30) , age <= 74) %>% 
                   ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
                   labs(title = "Age Distribution of Actives", y = "%") + facet_grid(runname~year)

df_eaDist_all %>% filter(runname %in% runName_list, year %in% c(1,30) , age <= 74) %>% 
                  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
                  labs(title = "Entry Age Distribution of Actives", y = "%") + facet_grid( runname ~ year)

df_yosDist_all %>% filter(runname %in% runName_list, year %in% c(1,30), yos <= 55) %>% 
                   ggplot(aes(x = yos,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
                   labs(title = "Year of Service Distribution of Actives", y = "%") + facet_grid( runname ~ year)


df_ageDist.r_all %>% filter(runname %in% runName_list, year %in% c(1,30)) %>% 
                     ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
                     labs(title = "Age Distribution of Retirees", y = "%") + facet_grid( runname ~ year)


names(df_entDist_all)[3] <-  "pct_age"
df_entDist_all %>% filter(runname %in% runName_list) %>% 
                   ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
                   labs(title = "Age Distribution of Entrants", y = "%") + facet_grid(.~runname)
 


df_entDist_all %>% filter(runname %in% c("D1F075-average", "D1F075-mature1", 
                                         "D1F075-mature2", "D1F075-immature")) %>% 
                   ggplot(aes(x = age, y = pct_age, color = runname)) +  
                   geom_line() + 
                   geom_point()



#****************************************************************************************************
#               ## Tables for the report ####
#****************************************************************************************************

tab.vars <- c("runname", "year", 
              "actives_age.avg",
              "actives_yos.avg",
              "actives_ea.avg",
              "retirees_age.avg",
              #"workLife.avg",
              #"lifeTime.avg",
              "ab.ratio")  

tab.runname <- c("average",
                 "average_g1",
                 "average_gn1",
                 "mature1_gn1",
                 "mature2_gn1",
                 "immature_g1")
tab.runname <- paste0("D1F075-", tab.runname)


tab_demoSum_y1 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
                        filter(runname %in% tab.runname, year %in% c(1))

tab_demoSum_y30 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
                        filter(runname %in% tab.runname, year %in% c(30))

tab_demoSum_y60 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
                        filter(runname %in% tab.runname, year %in% c(60))



  
  
  

tab_demoSum_y1 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_PR) %>% 
                                filter(runname %in% tab.runname, year == 1) %>% 
                                group_by(runname) %>% 
                                summarise(NC_PR = median(NC_PR),
                                          MA_PR = median(MA_PR),
                                          ExF_PR = median(ExF_PR)))
tab_demoSum_y1 %<>% select(-sim) 
tab_demoSum_y1 %>% kable(digits = 1)

tab_demoSum_y30 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_PR) %>% filter(runname %in% tab.runname, year == 30, sim == 0))
tab_demoSum_y30 %<>% select(-sim) 
tab_demoSum_y30 %>% kable(digits = 1)

tab_demoSum_y60 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_PR) %>% filter(runname %in% tab.runname, year == 60, sim == 0))
tab_demoSum_y60 %<>% select(-sim) 
tab_demoSum_y60 %>% kable(digits = 1)




#****************************************************************************************************
#               ## Descriptive statistics of plans for the report ####
#****************************************************************************************************

runName <- "D1F075-mature2"
runName_list <- c("D1F075-average",
                  "D1F075-mature1_gn1", 
                  "D1F075-mature2_gn1", 
                  "D1F075-immature_g1")


df_ageDist_all %>% filter(runname %in% runName_list, year %in% c(1,30) , age <= 74) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Age Distribution of Actives", y = "%") + facet_grid(year~runname)

df_eaDist_all %>% filter(runname %in% runName_list, year %in% c(1,30) , age <= 74) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Entry Age Distribution of Actives", y = "%") + facet_grid(year~runname)

df_yosDist_all %>% filter(runname %in% runName_list, year %in% c(1,30), yos <= 55) %>% 
  ggplot(aes(x = yos,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Year of Service Distribution of Actives", y = "%") + facet_grid(year~runname)
df_yosDist_all

df_ageDist.r_all %>% filter(runname %in% runName_list, year %in% c(1,30)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Age Distribution of Retirees", y = "%") + facet_grid( runname ~ year)


names(df_entDist_all)[3] <-  "pct_age"
df_entDist_all %>% filter(runname %in% runName_list) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Age Distribution of Entrants", y = "%") + facet_grid(.~runname)







