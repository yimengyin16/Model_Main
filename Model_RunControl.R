## 5/28/2015


# Preamble ###############################################################################################

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
library(data.table)
library(readxl)
library(stringr)
# library(xlsx)
# library(XLConnect) # slow but convenient because it reads ranges
#devtools::install_github("donboyd5/decrements")
#devtools::install_github("donboyd5/pp.prototypes")

library(pp.prototypes)
library(decrements)               # mortality and termination for now
load("Data/winklevossdata.rdata") # disability, disability mortaity and early retirement
# load("Data/example_Salary_benefit.RData")


source("Functions.R")

devMode <- FALSE # Enter development mode if true. Parameters and initial population will be imported from Dev_Params.R instead of the RunControl file. 



## for replicating David and Gang's paper

# 0.04, 0.045, 0.050, 0.0568, 0.06, 0.065, 0.07 
dg.rate   <- 0.0568
init.rate <- 0.0568 

act.tot <- 108
ret.tot <- 54


salgrowth.hist   <- salgrowth.hist   %>% mutate(sscale.hist.rate   = dg.rate)
salgrowth.assume <- salgrowth.assume %>% mutate(sscale.assume.rate = dg.rate)


df <- expand.grid(age = 20:64, ea = 20:64) %>% 
       mutate(planname = "average",
         nactives = act.tot/n()) %>% 
         group_by(ea) %>% 
         mutate(salary = (1 + init.rate)^(0:(length(age) - 1) )) %>% 
  filter(age >= ea, age %in% 30:64, ea %in% 30:64)

 # x <- df %>% mutate(f = 0.02 * (65-ea) * max(salary)) %>% ungroup %>% filter(age == 64)
 # x$f %>%  mean # average projected benefit
 
df$salary %>% mean


actives %<>% filter(planname != "average" ) %>% rbind(df)


df2 <- data.frame(planname = "average", age = 65:90) %>% 
       mutate(nretirees = ret.tot/n(),
              benefit   = 2) #  
df2
retirees %<>% filter(planname != "average" ) %>% rbind(df2)

retirees

# termination rate
termination$qxt <- 0

# whether to exclude retirees
retirees %<>% mutate(nretirees = 0*nretirees) 


# "Uniform for average"
# da <- actives %>% group_by(planname) %>% summarize(mean = mean(nactives))
# actives %<>% mutate(nactives = as.numeric(da[1,"mean"]))  
# # with(actives,sum(age*nactives)/sum(nactives))
# 
# dr <- retirees %>% group_by(planname) %>% summarize(mean = mean(nretirees))
# retirees %<>% mutate(nretirees = as.numeric(dr[1,"mean"]))



#*********************************************************************************************************

folder_run          <- "IO_ActMethods"
# folder_run          <- "IO_Prelim"

filename_RunControl <- dir(folder_run, pattern = "^RunControl")


#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)


# Import global parameters
Global_params <- read_excel(path_RunControl, sheet="GlobalParams", skip=1) 


# Import parameters for all plans
plan_params        <- read_excel(path_RunControl, sheet="RunControl", skip=4)    %>% filter(!is.na(runname))
plan_returns       <- read_excel(path_RunControl, sheet="Returns",    skip=0)    %>% filter(!is.na(runname))
plan_contributions <- read_excel(path_RunControl, sheet="Contributions", skip=0) %>% filter(!is.na(runname))




#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

## select plans
runlist <- plan_params %>% filter(include == TRUE) %>% select(runname) %>% unlist
# runlist <- runlist[runlist == "R4F3"]
# runlist <- runlist[runlist == "average1"|runlist == "average3"]
# runlist <- runlist[runlist == "average3"]
runlist




## Run selected plans 

for (runName in runlist){

suppressWarnings(rm(paramlist, Global_paramlist))
  
## Extract plan parameters 
paramlist    <- get_parmsList(plan_params, runName)
paramlist$plan_returns <- plan_returns %>% filter(runname == runName)
if(paramlist$exCon) paramlist$plan_contributions <- trans_cont(plan_contributions, runName) else 
                    paramlist$plan_contributions <- list(0) 


## Extract global parameters and coerce the number of simulation to 1 when using deterministic investment reuturns.
Global_paramlist <- Global_params %>% as.list
if ((paramlist$return_type == "simple" & paramlist$ir.sd == 0) |
    (paramlist$return_type == "internal" &  all(paramlist$plan_returns$ir.sd == 0))){
  
  Global_paramlist$nsim <- 1
  
}

## Run the model
source("Model_Master.R", echo = TRUE)
}



