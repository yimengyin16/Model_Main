# This script import example salary and retirement benefit data.

## Outputs:
 # SS: salary scale
 # avgpay: average salary by ea and age in year 1.  (ea range: 20-70, age range: 20-120)
 # avgben: average benefit by ea and age in year 1. (ea range: 20-70, age range: 48-120) 



library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(XLConnect) # slow but convenient because it reads ranges

source("Functions.R")

file_path <- paste0("Data/")

#*************************************************************************************************************
#                                      Import Salary scale from AV                                       #####                  
#*************************************************************************************************************

# Notes:
# 1. Given a salary scale and a salary table in year 1, we can infer salary for workers who enter the workforce between 
#    year 1 - (r.max - 1 - 20) (-43 when r.max = 65), and year 0. However, for workers entering the workforce later than year 1, 
#    we cannot infer their salary based on the salary scale. One possible way to do the inference is described below. 
#    1) Make assmption about how starting salary (ea = age) grows after year 1, one option is to use the inflation rate + real wage growth rate, or just inflation rate. 
#       as the growth rates of starting salary.
#    2) With the assumed growth rates of starting salary, calculate the starting salary in year 2 to year nyear based on the starting salary in year 1.
#    3) Based on the starting calculated in 2), calculate complete salary table (ea by age) for year 2 to year nyear using the salary scale. 


## Salary scale type 1: Growth only depends on age
# Example: PA-PSERS

SS <- read.xlsx2(paste0(file_path, "PA-PSERS.xlsx"), sheetName = "SalaryGrowth", colClasses = "numeric", startRow = 3, stringsAsFactors = FALSE)
SS %<>%  rename(age.match = age) %>% right_join(data.frame(age = 20:70) %>% mutate(age.match = floor(age/10)*10)) %>% 
  select(-age.match) %>% 
  mutate(sscale.hist.rate = cton(growth)/100)

## Salary scale type 2: Growth depends on yos and year.
# Example: NJ-TPAF

#*************************************************************************************************************
#                                      Import Initial Salary table from AV                               #####                  
#*************************************************************************************************************

## Read in an example of salary table: PA-PSERS

# actives - numbered p.34 ####
# age x yos
# age 25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 Over 64
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.mid <- c(25, seq(27, 62, 5), 66)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format 
df <- readWorksheetFromFile(paste0(file_path, "PA-PSERS.xlsx"), sheet="PA-PSERS", header=FALSE, region="A5:L24")
names(df) <- c("order", "tabletype", "agegrp", yos.mid)

avgpay <- df %>% filter(tabletype=="avgpay") %>% 
  arrange(order) %>%
  mutate(age=age.mid) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, salary, -age) %>%
  filter(!is.na(salary)) %>%
  mutate(yos = f2n(yos)) %>% 
  splong("age", method = "natural") %>%
  splong("yos", method = "natural")

avgpay <- (expand.grid(age = 20:70, yos = 2:42) %>% mutate(age.match = ifelse(age < 25, 25,ifelse(age>66, 66,age)))) %>% 
  left_join(avgpay %>% rename(age.match = age))

avgpay <- (expand.grid(age = 20:70, yos = 0:50) %>% mutate(yos.match = ifelse(yos < 2, 2, ifelse(yos>42, 42, yos)))) %>% 
  left_join(avgpay %>% rename(yos.match = yos))

avgpay %<>% select(-age.match, -yos.match) %>% 
  filter(age - yos >= 20) %>% 
  mutate(ea = age - yos)


#Display in matrix form  
# avgpay %<>% select(-yos) %>% 
#   spread(age, avgpay, fill = 0)
# rownames(avgpay) <- avgpay$ea
# avgpay




#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************

## Read in an example of retirement benefit table: PA-PSERS

## WARNING: # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)

# repeat for retirees p.35 ####
# age x yos
# age has different groupings than for actives, of course; yos groupings are the same
# age <50 50-54 55-59 60-64 65-69 70-74 75-79 80-84 85-89 Over 89
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.mid <- c(48, seq(52, 87, 5), 92)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format 
df <- readWorksheetFromFile(paste0(file_path, "PA-PSERS.xlsx"), sheet = "PA-PSERS", header = FALSE, region = "A30:L49")
names(df) <- c("order", "tabletype", "agegrp", yos.mid)

avgben <- df %>% filter(tabletype=="bens") %>% 
  arrange(order) %>%
  mutate(age=age.mid) %>%
  select(-order, -tabletype, -agegrp) %>% 
  gather(yos, benefit, -age) %>%
  mutate(yos = f2n(yos)) %>% 
  splong("age", method = "natural") %>%
  splong("yos", method = "natural")
  #%>% spread(yos, avgben) %>% print

avgben <- (expand.grid(age = 48:120, yos = 2:42) %>% mutate(age.match = ifelse(age > 92, 92, age))) %>% 
  left_join(avgben %>% rename(age.match = age))

avgben <- (expand.grid(age = 48:120, yos = 0:50) %>% mutate(yos.match = ifelse(yos < 2, 2, ifelse(yos>42, 42, yos)))) %>% 
  left_join(avgben %>% rename(yos.match = yos))

avgben %<>% 
  mutate(ea = 70 - yos) %>%    
  filter(ea >= 20) %>%  
         #age >= r.max) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  select(-age.match, -yos.match, -yos)

# Notes:
# 1. With single retirement age, entry age is assumed retirement age minus yos. Note that "assumed retirement age" is
#    not necessarily equal to r.max. But this is only a example of retirement benefit table used to develop the main model,
#    all we want to make sure is that its format is compatible with the model. When we want to model the a prototype plan,
#    we need to make the table consistent with the parameters in the main model. 
# 2. With multiple retirement ages and vested terms, we can simple assume they retire/quite at age r.max, since 
#    when they retire/quite does not affect the calculation 
# 3. Actually all we need is average salary by age group. I don't think we care about the ea and yos of retirees. 
#    When merged to the main model we can assign any proper ea to the initial retirees. 

  
# Display in matrix form  
# avgben %<>% 
#   spread(age, avgben, fill = 0)
# rownames(avgben) <- avgben$ea
# avgben

# there are negative values at the upper left corner. But it should be ok if we assume there is no retirees under age 52. 

save(SS, avgpay, avgben, file = paste0(file_path, "example_Salary_benefit.RData"))
