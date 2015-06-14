# This script compares mortality tables in decrement packages with that in TPAF and PSERS. 

library(gdata) # read.xls
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(decrements)


rm(list = ls())
source("Functions.R")


#*********************************************************************************************************
#  Mortality in decrements package  ####
#*********************************************************************************************************

data("mortality")
data("termination")


mort <- mortality %>% select(tablename, age, qxm) %>% group_by(tablename) %>%  
  mutate(pxm = ifelse(age == min(age), 1, lag(cumprod(1 - qxm))))



#*********************************************************************************************************
#  Mortality and Termination rates in TPAF  ####
#*********************************************************************************************************
## Notes
# Mortality under age 20 will be dropped. 
# Problem: No accidental mortality rate is provided. Omitting it may cause overestimation of survival probabilities. 

data.path <- "E:/GitHub/PenSim-Projects/Model_Main/IO_Initial_Runs_3/"
data.file <- "TPAF ES2012 Assumptions.xlsx"

## Mortality 
mortMale <- read.xls(paste0(data.path, data.file), sheet = "MortalityMale", skip = 3, na.strings = "NA", header = TRUE) %>% 
  right_join(data.frame(age = 20:110)) %>% 
  mutate(qxm.m = ifelse(age < 65, qxm_act_o, qxm_post_h)) %>% 
  select(age, qxm.m)

mortFemale <- read.xls(paste0(data.path, data.file), sheet = "MortalityFemale", skip = 3, na.strings = "NA", header = TRUE) %>% 
  right_join(data.frame(age = 20:110)) %>% 
  mutate(qxm.f = ifelse(age < 65, qxm_act_o, qxm_post_h)) %>% 
  select(age, qxm.f)

mort.TPAF <- left_join(mortMale, mortFemale) %>%
             mutate(qxm = ifelse(age == max(age), 1, 0.75 * qxm.f + 0.25 * qxm.m),
                    pxm = ifelse(age == min(age), 1, lag(cumprod(1 - qxm))),
                    tablename = "TPAF13.f75") %>% 
             select(tablename, age, qxm, pxm)

## Mortality 

load("Data/decrement_term.RData")

termFemale_AB_vest %>% select(-yos) %>% spread(ea, qxt.vest)


#*********************************************************************************************************
#  Plot  ####
#*********************************************************************************************************

mort <- rbind(mort, mort.TPAF)

table_select <- c("gam1971.hybrid", "rp2000.hybrid.f75", "rp2014.hybrid", "TPAF13.f75")

mort %>% filter(tablename %in% table_select) %>% filter(age <=50) %>%  
ggplot(aes(x = age, y = qxm, color = tablename)) + theme_bw() + geom_line() + geom_point() 

mort %>% filter(tablename %in% table_select) %>%  
  ggplot(aes(x = age, y = pxm, color = tablename)) + theme_bw() + geom_line() + geom_point() 









