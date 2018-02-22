# This script checks various technical issues in M2.1a and M2.1b
  # 1. the compound annual return for simulations with median funded ratio.
  # 2. funded ratios of various demographic scenarios when 30-year compound annual return meets the earnings assumption. 
  # 3. Make sure we understand Fig 4 in M2.1a: Decline in AAL around age 60 
  # 4. Make sure we understand the decline in median ERC rate in Fig 5 M2.1b

#*****************************************************
##  Packages and Functions  ####
#*****************************************************


library(magrittr) # to use %<>%

library(knitr)

library(grid)
library(gridExtra)

library(reshape2)
library(forcats)
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# hms tidyr tibble readr stringr purrr dplyr ggplot2 grid gtable scales DBI Rcpp colorspace

library(zoo)
library(xlsx)

library(pp.prototypes)

source("Functions.R")
source("Functions_Measures.R")


#*****************************************************
##  Defining paths for inputs and outputs ####
#*****************************************************
IO_folder       <- "IO_M2.1_new"
outputs.folder  <- "/M2.1a_outputs/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Loading and pre-processing simulation outputs
# This section will produce two .RData file under the folder "Analysis_Demo"
# 1. Demo_results_all.RData. Data frame "results_all" that contains results for pension finances. 
# 2. DemoSum_all.RData".     Various data frames that contains demographic data. 

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}
# results_all <- get_results(IO_folder, "^Outputs_D") # This will take a while because of the large size of output files that contain demographic data
# save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))

## Outputs of demographics
#source("IO_M2.1_new/Report_M2.1a_LoadingDemoData.R")


## Loading existing data. 
load(paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))




#***********************************************************************
##  Compound annual returns of simulations with median funded ratio ####
#***********************************************************************

runs_demo.all <- c("D1F075-average_gn2",
                   #"D1F075-average_gn1",
                   "D1F075-average",
                   #"D1F075-average_g1",
                   "D1F075-average_g2",
                   "D1F075-mature1_gn1",
                   "D1F075-mature2_gn1",
                   "D1F075-immature_g1")

results_all %>% filter(sim > 0, runname %in% runs_demo.all) %>% 
  group_by(runname, sim) %>% 
  mutate(cumGeoReturn = cumprod(1 + i.r)^(1/seq_along(i.r)) - 1) %>% 
  ungroup() %>% 
  group_by(runname, year) %>% 
  arrange(runname, year, FR_MA) %>% 
  mutate(FR_order = seq_along(FR_MA)) %>% 
  select(runname, year, sim, i.r, cumGeoReturn, FR_MA, FR_order) %>% 
  filter(FR_order == 501) %>% 
  ggplot(aes(x = year, y = cumGeoReturn, color = runname)) + 
  geom_line() + geom_point() +
  geom_hline(yintercept = 0.075)+
  coord_cartesian(ylim = c(0.0, 0.15)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.01))

# The compound annual returns of simulations with median funded ratios mostly 
# concentrates in the range of 7% to 8% (assumption +-0.5%)


#*************************************************************************************************
##  Funded ratio of various demographic scenarios when earnings assumption is met in 30 years ####
#*************************************************************************************************

results_all %>% filter(sim %in% c(56, 228), year <=30, runname %in% runs_demo.all) %>% 
  ggplot(aes(x = year, y = FR_MA, color = runname)) + facet_grid(.~sim) + 
  geom_point() + geom_line()

# The order of FR is largely the same as that of the risk of FR falling below 40%. 



#*************************************************************************************************
##  Make sure we understand Fig 4 in M2.1a: Decline in AAL around age 60 ####
#*************************************************************************************************

load(paste0(IO_folder, "/Outputs_D1F075-average.RData"))
df_actives <- outputs_list$ind_active

df_actives %>% filter(ea == 20, year - (age - ea) == 1 ) %>% 
  mutate(ALx.av = ALx + ALx.v,
         NCx.av = NCx + NCx.v,
         AL_sx = 100 * ALx.av / sx,
         growth.AL = 100 * (ALx.av - lag(ALx.av))/lag(ALx.av),
         growth.sx = 100 * (sx - lag(sx))/lag(sx),
         growth.NCx = 100 * (NCx.av - lag(NCx.av))/lag(NCx.av),
         growth.diff = growth.AL - growth.sx,
         growth.AL_sx = (AL_sx - lag(AL_sx))) %>% 
  select(year, ea, age, ALx, ALx.av, sx, AL_sx, NCx, growth.AL, growth.sx, growth.diff, growth.NCx, growth.AL_sx)

# Growth of AL slows dramatically at age 60 and then recovers gradually. 
# One possible explanation is that the early retirement panelty on benefit only applies untile age 60, after which AL loses a source of growth.




#*************************************************************************************************
##  Make sure we understand the decline in median ERC rate in Fig 5 M2.1b ####
#*************************************************************************************************

load(paste0(IO_folder, "/Outputs_I8F075-3.RData"))

df_results <- outputs_list$results

df_results %>% filter(sim == 0) %>% 
  mutate(SC_PR = 100 * SC / PR) %>% 
  select(sim, year, NC_PR, SC_PR, C_PR)

# The declining ERC rate is caused by declining SC rate. 



#*************************************************************************************************
##  Check normal cost for various demographic scenarios ####
#*************************************************************************************************

## Check why mature plan 2 has very high NC rate. 
load(paste0(IO_folder, "/Outputs_D1F075-average.RData"))
df_actives_avg <- outputs_list$ind_active

df_actives_avg %>% 
  group_by(ea) %>% 
  filter(year - (age - ea) == 1, age == ea + 1) %>%
  mutate(NC_PR =  (NCx + NCx.v)/sx ) %>% 
  select(year, ea, age, NC_PR)
  

load(paste0(IO_folder, "/Outputs_D1F050-mature2_gn1.RData"))
df_actives_m2 <- outputs_list$ind_active

df_actives_m2 %>% 
  group_by(ea) %>% 
  filter(year - (age - ea) == 1, age == ea + 1) %>%
  mutate(NC_PR =  (NCx + NCx.v)/sx ) %>% 
  select(year, ea, age, NC_PR)


load("./Data/2015-10-07/salgrowth.rda")
salgrowth %>% filter(planname %in% c("AZ-PERS-6.yos", "LA-CERA-43.yos", "OH-PERS-85.yos")) %>% 
  spread(planname, salgrowth)

salgrowth %>% filter(planname %in% c("AZ-PERS-6.yos", "LA-CERA-43.yos", "OH-PERS-85.yos")) %>% 
  group_by(planname) %>% 
  summarise(sal60 = prod(1 + salgrowth))


load(paste0(IO_folder, "/Outputs_D1F075-mature1_gn1.RData"))
outputs_list$results %>% filter(sim  == 1) %>% select(year, sim, ERC_PR, NC_PR)

#*************************************************************************************************
##  Check normal cost and contribution rates for various investment return scenarios ####
#*************************************************************************************************

load(paste0(IO_folder, "/Outputs_I1F075-6.RData")) # Invest in riskier assets

outputs_list$results %>% filter(sim == 1, year <= 30) %>% 
  select(runname, NC_PR, ERC_PR) %>% 
  data.frame()


load(paste0(IO_folder, "/Outputs_I7F075-1.RData")) # lower discount rate

outputs_list$results %>% filter(sim == 0, year <= 30) %>% 
  select(runname, year, NC_PR, ERC_PR) %>% 
  data.frame()







