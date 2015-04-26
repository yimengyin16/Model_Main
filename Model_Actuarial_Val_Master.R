# Actuarial Valuation in a simple setting
# Yimeng Yin
# 2/1/2015
# archived: 4/24/2015

# Note for this archived brunch ("Archived 4/24/2015")
# The model in this version does not include early retirement, ancillary benefits, asset smoothing and COLA.
# It is used to test the general modeling approach.  


# This program consists of following files:
  # Model_Actuarial_Val.R
  # Model_Actuarial_Val_wf.R
  # Model_Actuarial_Var_Import_Data.R
  # Functions.R
# Data files are contained in "your project directory"\Data\:
 # GAM-1971-Male.xls
 # Winklevoss(6).xlsx

# Goal: 
# 1. Conduct an actuarial valuation at time 1 based on plan design, actuarial assumptions, and plan data.
# 2. Conduct an actuarial valuation at time 2 based on the plan experience during the period [1, 2), and calculate
#    supplemental costs at time 2 based on the experience gain/loss or assumption changes. 
# 3. Conduct actuarial valuations each year during a time period of 100 years, allowing for stochastic investment returns.
# 4. Conduct the stochastic simulation in 3 for 10k times. 

# Assumptions
 # Plan Desgin
  # Beneficiary:  : Retirement Only; No disability, death, surivorship benefits
  # Benfit formula: 1% of FAS per YOS; FAS for last 3 years
  # Retirment age : 65, not early retirement
  # Vesting:      : 1) No vesting;
  #                 2) Vested if YOS >= 3

 # Cost Method
  # 1) EAN(Entry Age Normal)
  # 2) PUC(Projected Unit Credit)

 # Actuarial Assumptions
  # Decrements: Mortality, termination, disability
  # Salary scale
  # Assumed interest rate
  # Inflation
  # Productivity
 
 # Other Assumption
  # Contribution rule: Sponsor contributes the amount of plan cost(Normal cost + supplemental cost) in all periods.

# Outputs
  # Actuarial liability at time 1 and time 2
  # Normal cost between 1 and 2
  # Assets at 1 and 2
  # UAAL at 1 and 2
  # Funded Ratio


# Preamble ###############################################################################################

rm(list = ls())

library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
#library(corrplot)

source("Functions.R")

# source("Model_Actuarial_Val_Import_Data.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 0. Parameters ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Assumptions
nyear <- 100          # The simulation only contains 2 years.
ncore <- 1            # # of CPU cores used in parallelled loops

benfactor <- 0.01   # benefit factor, 1% per year of yos
fasyears  <- 3      # number of years in the final average salary calculation
r.max     <- 65     # maximum retirement age, at which all actives retire with probability 1. 


infl <- 0.04        # Assumed inflation
prod <- 0.01        # Assumed productivity
i <- 0.08           # Assumed interest rate
v <- 1/(1 + i)      # discount factor



# Actuarial method
actuarial_method <- "EAN.CP"  # One of "PUC", "EAN.CD", "EAN.CP"

# Amortization 
amort_method <- "cd" # "cd" for Constant dollar, "cp" for constant percent, "sl" for straitline
m = 3                # years of amortization of gain/loss


# Age and entry age combinations  
range_ea  <- seq(20, 60, 5) # For now, assume new entrants only enter the workforce with interval of 5 years. 
range_age <- 20:110 

# Initial Active
init_active <- rbind(c(20, 20, 100),
                     c(20, 40, 100),
                     c(20, 64, 100))

# Initial Retired 
init_retired <- rbind(c(20, 65, 100),
                      c(20, 85, 100))

# Set real rate of return
AA_0 <- 200
init_AA <- "AL0"  # "AA0" for preset value; "AL0" for being equal to initial liability 





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Actuarial liabilities, normal costs and benenfits ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source("Model_Actuarial_Val_Liab.R")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Workforce ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Simulation of the workforce is done in the file below: 
source("Model_Actuarial_Val_wf.R")
 # Note: Objects passed to the program above:
 #       range_age
 #       range_ea
 #       nyear
 #       decrement



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Simulation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nsim  <- 2    # No. of sims

set.seed(1234)

# setting actual investment return.
i.r <- matrix(rnorm(nyear*nsim, mean = 0.08, sd = 0.12),nrow = nyear, ncol = nsim) 
#i.r <- matrix(0.08, nrow = nyear, ncol = nsim) 

source("Model_Actuarial_Val_Sim.R")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Results ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

options(digits = 3, scipen = 3)

#View(penSim_results[[1]])
kable(penSim_results[[1]], digits = 3)



df <- bind_rows(penSim_results) %>% 
  mutate(sim=rep(1:nsim, each=nyear)) %>%
  select(sim, year, everything())



## After optimization

#           Total /  Loop
#  100 sims
#  4 cores: 16.74 / 3.29
#  8 cores: 17.59 / 3.72   
# 
#  1000 sims
#  4 cores: 34.8  / 21.56 
#  8 cores: 33.07 / 18.91

#  5000 sims
#  4 cores: 118.30 / 105.11
#  8 cores: 101.2  / 87.2

#  10000 sims
#  4 cores: 216.2  / 202.8
#  8 cores: 186.1  / 171.6

Time_liab
Time_wf
Time_loop




