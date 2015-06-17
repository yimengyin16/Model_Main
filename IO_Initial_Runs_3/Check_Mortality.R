# This script compares mortality tables in decrement packages with that in TPAF and PSERS. 

library(gdata) # read.xls
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(decrements)


# rm(list = ls())
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



#*********************************************************************************************************
#  Check NC rate  ####
#*********************************************************************************************************

# Run the following code after running the model with uniform population distribution

NC_rate <- liab$active %>% mutate(NC.rate = NCx / sx) %>%
           filter(year== 1, age <= 64) %>% # NC rate by ea (NC rates are the same given ea under EAN.CP)
           left_join(actives %>% filter(planname == "average")) %>% 
           group_by(ea) %>% 
           summarise(sal.tot = sum(nactives * salary), NC.rate = max(NC.rate)) %>% 
           mutate(weight = sal.tot / sum(sal.tot),
                  growth = "growth7pct")

NC_rate %>% ggplot(aes(x = ea, y = NC.rate )) + geom_point()+ geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.2))
NC_rate %>% select(ea, weight) %>% 
            ggplot(aes(ea, weight)) + geom_bar(stat = "identity") + theme_bw() + 
            coord_cartesian(ylim = c(0, 0.22))

# Check if the weighted average NC rate is the same as the model output
with(NC_rate, sum(NC.rate * weight)) # matched! 

save(NC_rate, file  = "IO_Initial_Runs_3/SalGrowth/salGrowth700.RData")
# 0.40%: NC rate 8.30%
# 0.45%: NC rate 8.81%
# 0.50%: NC rate 9.34%
# 0.568%: NC rate 10.11%
# 0.60%: NC rate 10.49%
# 0.65%: NC rate 11.11%
# 0.7%: NC rate 11.67%

salgrowth.df <- lapply(dir("IO_Initial_Runs_3/SalGrowth/"), 
                function(x) {load(paste0("IO_Initial_Runs_3/SalGrowth/",x)); return(NC_rate)}) %>% 
                bind_rows
salgrowth.df$growth %<>% factor(levels = c("growth4pct", "growth4.5pct", "growth5pct", "growth5.68pct", "growth6pct"
                                          , "growth6.5pct", "growth7pct"))

ycoord <- salgrowth.df %>% filter(ea == 20) %>% select(NC.rate) %>% unlist
ylabel <- c("8.30%", "8.81%", "9.34%", "10.11%", "10.49%", "11.11%", "11.67%")

salgrowth.df %>% ggplot(aes(ea, NC.rate, color = growth)) + geom_point() + geom_line() + theme_bw() + 
                 scale_color_brewer(palette = "Reds") + coord_cartesian(x = 15:66) +
                 annotate(geom = "text", x = 20, y = ycoord,
                 label = ylabel,
                 hjust = 1.2, vjust = 0, size = 3.5)
  
                 

# Run the following code after running the model with the "average" population distribution
NC_rate <- liab$active %>% mutate(NC.rate = NCx / sx) %>%
  filter(year== 1, age <= 64, ea == age) 
 
weight <- liab$active %>% 
          left_join(actives %>% filter(planname == "average")) %>% 
          group_by(ea) %>% 
          summarise(sal.tot = sum(nactives * salary, na.rm = TRUE)) %>% 
          mutate(weight = sal.tot / sum(sal.tot))

NC_rate %<>% left_join(weight)  

NC_rate %>% ggplot(aes(x = ea, y = NC.rate )) + geom_point()+ geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.2))
NC_rate %>% select(ea, weight) %>% 
            ggplot(aes(ea, weight)) + geom_bar(stat = "identity") + theme_bw() + # NC rate at low entry ages are very low
            coord_cartesian(ylim = c(0, 0.22))

         






