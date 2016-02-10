# This script constructs mortality tables for the UCRP model. 


# Mortality tables:
# - Pre-retirement mortality: RP2014 White Collar Employee Mortality Table projected with the two dimensional MP2014 
#     projection scale to 2029 (RP2014 p60)
# - Post-retirement mortality: RP-2014 White Collar Healthy Mortality Table projected with the Two dimensional MP2014 
#     projection scale to 2029. Ages are then set forward on year for males(from the male table) and females 
#     (from the female table) (RP2014 p60)
# - Disabled mortality: RP-2014 Disabled Retiree Mortality Table projected with the Two dimensional MP2014 projection 
#     scale to 2029. Ages are then set forward on year for males(from the male table) and females (from the female table) (RP2014, p54)

# The construction of projected mortaltiy follows the procedure described in Mortality Improvement Scale MP-2014 Report(SOA, 2014, p22).



rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)

source("Functions.R")

#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

# Import morality data
data_raw_tot <- read_excel("Data/RP2014/research-2014-rp-mort-tab-rates-exposure.xlsx", sheet = "Total Dataset", skip = 3)[, c(1, 4, 8)] 
data_raw_wc  <- read_excel("Data/RP2014/research-2014-rp-mort-tab-rates-exposure.xlsx", sheet = "White Collar", skip = 3)[, -4] # exclude an empty column

names(data_raw_tot) <- c("age", "qxm.d.M", "qxm.d.F")
names(data_raw_wc)  <- c("age", "qxm.pre.M", "qxm.post.M", "qxm.pre.F", "qxm.post.F")

data_raw <- left_join(data_raw_wc, data_raw_tot)  


# Import projection scale (scale BB-2D)

data_scale_M <- read_excel("Data/RP2014/research-2014-mort-imp-scale-rates.xlsx", sheet = "Males", skip = 1) %>% 
                filter(!is.na(Age)) %>% 
                mutate(Age = 20:120, 
                       gender = "M")
names(data_scale_M) <- c("age",1951:2030,"gender")


data_scale_F <- read_excel("Data/RP2014/research-2014-mort-imp-scale-rates.xlsx", sheet = "Females", skip = 1) %>%
                filter(!is.na(Age)) %>% 
                mutate(Age = 20:120, 
                       gender = "F")
names(data_scale_F) <- c("age",1951:2030, "gender")


# Expand the scales to 1915-2164
 # 1915: the year when a 120-year old retiree in 2015 was at age 20. 
 # 2164: the year when a 20-year old new entrant in 2015 will be at age 120.
 # The scale BB-2D covers year 1951-2030. Years before 1951 use the scale for 1951, and years after 2030 use the scale for 2030. 




#*********************************************************************************************************
#                      ## Construct Projected Mortality table ####
#*********************************************************************************************************

# Transform data to long format
data_raw %<>% gather(type, qxm, -age) %>% mutate(year = 2014, gender = str_sub(type, -1))
data_scale_M %<>% gather(year.match, scale.M, -age, -gender) %>% mutate(year.match = as.numeric(f2n(year.match)))
data_scale_F %<>% gather(year.match, scale.F, -age, -gender) %>% mutate(year.match = as.numeric(f2n(year.match)))


# Creat data table: age x year x type
# mortality <- expand.grid(year = 1951:2030, age = 20:120, type = levels(data_raw$type)) %>% 
#   mutate(gender = str_sub(type, -1))

mortality <- expand.grid(year = 1915:2164, age = 20:120, type = levels(data_raw$type)) %>% 
  mutate(gender     = str_sub(type, -1),
         year.match = ifelse(year < 1951, 1951, ifelse(year>2030, 2030, year)))




# Calculate projected mortality
mortality %<>% left_join(data_raw) %>% 
  left_join(data_scale_M) %>% 
  left_join(data_scale_F) %>% 
  mutate(scale = ifelse(gender == "M", scale.M, scale.F)) %>% 
  group_by(type, age) %>% 
  mutate(
    qxm_proj = ifelse(year >= 2014, qxm[year == 2014] *  cumprod(ifelse(year <= 2014, 1, 1 - scale)), NA),
    qxm_proj = ifelse(year < 2014,  qxm[year == 2014] * lead(order_by(-year, cumprod(ifelse(year > 2014, 1, 1/(1 - scale))))), qxm_proj)
    ) %>% 
  select(year, age, type, qxm_proj)


# Spot check the results
df1 <- mortality %>% filter(type == "qxm.pre.M") %>% ungroup %>% 
              select(year, age, qxm_proj) %>% filter(age == 20)



#*********************************************************************************************************
#                      ## Produce separate tables for each type ####
#*********************************************************************************************************

# pre-retirement table 
mortality.pre <- mortality %>% filter(type %in% c("qxm.pre.M", "qxm.pre.F")) %>% ungroup %>% 
                 spread(type, qxm_proj)

# post-retirement table 
mortality.post <- mortality %>% filter(type %in% c("qxm.post.M", "qxm.post.F")) %>% ungroup %>% 
                  spread(type, qxm_proj)

# disabled retirees table 
mortality.disb <- mortality %>% filter(type %in% c("qxm.d.M", "qxm.d.F")) %>% ungroup %>% 
  spread(type, qxm_proj)


mortality.pre %>% filter(year == 2029) %>% print(n = 1000)
mortality.disb %>% filter(year == 2029) %>% print(n = 1000)


save(mortality.pre, mortality.post, mortality.disb, file = "Data/UCRP.inputs1.RData")






