# Actuarial Valuation in a simple setting
# Yimeng Yin
# 4/30/2015


# This program consists of following files:
  # Model_Actuarial_Val_master.R
  # Model_Actuarial_Val_Liab.R
  # Model_Actuarial_Val_wf.R
  # MOdel_Actuarial_Val_Sim.R
  # Model_Actuarial_Var_Import_Data.R
  # Functions.R
# Data files are contained in "your project directory"\Data\:
 # GAM-1971-Male.xls
 # Winklevoss(6).xlsx

# Goals: 
# 1. Conduct an actuarial valuation at time 1 based on plan design, actuarial assumptions, and plan data.
# 2. Conduct an actuarial valuation at time 2 based on the plan experience during the period [1, 2), and calculate
#    supplemental costs at time 2 based on the experience gain/loss or assumption changes. 
# 3. Conduct actuarial valuations each year during a time period of 100 years, allowing for stochastic investment returns.
# 4. Conduct the stochastic simulation in 3 for 10k times. 

# Assumptions
 # Plan Desgin
  # Beneficiary:  : Retirement Only; No disability, death, surivorship benefits
  # Benfit formula: % of FAS per YOS; FAS for last # years
  # Retirment age : NO early retirement
  # Vesting:      : Vested if YOS > #

 # Cost Method
  # 1) EAN(Entry Age Normal)
  # 2) PUC(Projected Unit Credit)

 # Actuarial Assumptions
  # Decrements: Mortality, termination, disability
  # Salary scale
  # Assumed interest rate
  # Inflation
  # Productivity
 
 # Contribution rules: 
    # 1. Sponsor contributes full ADC, which equals the amount of plan cost(Normal cost + supplemental cost) in all periods.
    # 2. Sponsor contributes ADC with a cap, which equals a percentage of total payroll.    
    # 3. Sponsor contributes a fixed percentage of payroll. 

# Outputs
  # Actuarial liability
  # Normal costs
  # Assets
  # UAAL
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
##########################################################################################################




#*********************************************************************************************************
# 0. Parameters ####
#*********************************************************************************************************
# Rstrictions on current version
# 1. r.max must be in the range [55, 65]
# 2. r.min must be fixed at 55
# 3. v.yos <= r.max - r.min
# 4. Only EAN.CP and EAN.CD are ready for vested terms. 


## Model parameters
nyear <- 100        # # of years in simulation 
ncore <- 4          # # of CPU cores used in parallelled loops

## Benefit structure
benfactor <- 0.015   # benefit factor, % of final average salary per year of yos
fasyears  <- 3      # number of years in the final average salary calculation
# WARNING: In curret version, please do not set a r.max greater than 65 and less than 55 
r.max     <- 65     # maximum retirement age, at which all actives retire with probability 1. 
r.min     <- 55     # minimum retirement age. (for multi-retirement ages, will not be used in current version)
                    # Current version depends on the assumption that active workers do not quit their jobs once reaching age r.min. 
cola      <- 0.01   # annual growth of retirement benefits in a simple COLA 

v.yos     <- 10      # yos required to be vested.  
r.yos     <- 10      # yos required to be eligible for early retirement

## Actuarial assumptions

# Economic assumptions
infl <- 0.03        # Assumed inflation
prod <- 0.01        # Assumed productivity
i <- 0.08           # Assumed discount rate
v <- 1/(1 + i)      # discount factor

# Actuarial method
actuarial_method <- "EAN.CP"  # One of "PUC", "EAN.CD", "EAN.CP"

# Amortization 
amort_method <- "cd" # "cd" for Constant dollar, "cp" for constant percent, "sl" for straitline
m = 3                # years of amortization of gain/loss


## Contriubtion Policy
ConPolicy    <- "ADC"
PR_pct_cap   <- 0.25
PR_pct_fixed <- 0.145
 # Values of ConPolicy:
  # ADC:     pay full ADC
  # ADC_cap: pay full ADC with a cap, which is defined as a proportion of payroll. If ADC_cap is chosen, set the parameter PR_pct_cap.
  # Fixed:   pay a fixed percent of payroll. If Fixed is chosen, set the parameter PR_pct_fixed.
EEC_rate     <- 0.005   

# For now the employer and employee contributions are calculated as follows:
# 1). Calculate ADC = NC + SC
# 2). Calculate EEC = EEC_rate * Payroll
# 3). Calculate employer's proportion of ADC: ADC_ER = ADC - EEC
# 4). Employer's contribution ERC is determined by one of the three contribution rules
# 5). Total contribution: C = EEC + ERC
# Potential problem:
  # Employee contribution as % of salary. For now this rate is fixed in all period, even 
  # employee contribution is greater than normal costs and results in negative employer contribution.
  # More rules are needed to avoid negative contributions. 



# Set inital asset values
MA_0 <- 200   # market value at 0
EAA_0 <- MA_0 # expected market value at 0
init_MA  <- "AL"  # "MA" for preset value; "AL" for being equal to initial liability 
init_EAA <- "MA"  # "MA" for the same value as MA; "EAA" for preset value of inital EAA.

# weight on market value in asset smoothing
w <- 1  # No asset smoothing when set to 1. 



## Population 
# Age and entry age combinations  
# range_ea  <- c(seq(20, r.max - 1, 5), r.max - 1 ) # For now, assume new entrants only enter the workforce with interval of 5 years. Note that max entry age must be less than max retirement age.  
range_ea <- c(20, 25, 30, 35, 40, 45:(r.max - 1))
# range_ea <- 20:(r.max - 1)

range_age <- 20:110 # please do not change this for now. The code needs to be modified if we use life table with larger max age.  

# Initial Active
# WARNING: Ages and entry ages of active members must be less than retirement age. (max retirement age when multiple retirement ages is implemented) 
init_active <- rbind(c(20, 20, 1), # (entry age,  age, number)
                     c(20, 40, 1),
                     c(20, r.max - 1, 1),
                     c(45, 45, 1),
                     c(45, r.max - 1, 1),
                     c(50, 50, 1),
                     c(55, 55, 1),
                     c(r.max - 1, r.max - 1, 1)
                     )

# Initial Retired 
# WARNING: Ages and entry ages of retirees must be no less than retirement age. (min retirement age when multiple retirement ages is implemented)
init_retired <- rbind(c(20, r.max, 1),
                      c(20, 85, 1))

# Growth rate of workforce
wf_growth   <- 0.00    # growth rate of the size of workforce. For now, new entrants are equally distributed across all entry ages. 
no_entrance <- TRUE  # No new entrants into the workforce if set "TRUE". Overrides "wf_growth"



#*********************************************************************************************************
# 1. Import Salary table and initial retirement benefit table ####
#*********************************************************************************************************

# Notes:
# 1. User must provide the following data frames
#   "salary": a complete salary table containing salary history of current actives and projected salary path for future workers,
#              both of which are calculated based on a salary table of current actives from the AV and assumptions on salary growth.  
#   "avgben": a data frame containing average retirement benefit payment(including both service retirement and deferred retirement)
#             of all valid ea and age combos. The user MUST make sure the smallest age in the retirement benefit table is smaller than the 
#             single retirement age specified in parameter section. (smaller than r.min with multiple retirement ages)
# 2. Note that avgben is only used to calculate the current and projected values of benefits and liabilities of retirees at year 1. Future retirees' 
#    benefits and liabilities are calculated based on their salary history provided in "salary". 
# 3. I don't think in "avgben" we need average benefit for all combos of ea and age. All we need is actually average benefits
#    by age. Once we have that we can assign an arbitrary ea to it an throw it into the model. Note that the AV of NJ-TPAF only contains 
#    average benefits for retirees and vested terms by age group. Also note that if service retirees and vested terms follow the same mortality table 
#    and have the same COLA rule, I don't think we need to distinguish between them.     
#

# Artificial salary table and benefit table for testing purpose are imported by sourcing the following script.
# These tables are based on PA-PSERS and some naive assumptions for imputation.  
source("Model_Actuarial_Val_Salary_Benefit.R")




#*********************************************************************************************************
# 2. Actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("Model_Actuarial_Val_Liab.R")



#*********************************************************************************************************
# 3. Workforce ####
#*********************************************************************************************************

# Simulation of the workforce is done in the file below: 
source("Model_Actuarial_Val_wf.R")
 # Note: Objects passed to the program above:
 #       range_age
 #       range_ea
 #       nyear
 #       decrement



#*********************************************************************************************************
# 4. Simulation ####
#*********************************************************************************************************

nsim  <- 10    # No. of sims
set.seed(1234)

# setting actual investment return.
#i.r <- matrix(rnorm(nyear*nsim, mean = 0.08, sd = 0.12),nrow = nyear, ncol = nsim) 
i.r <- matrix(0.08, nrow = nyear, ncol = nsim) 

source("Model_Actuarial_Val_Sim.R")


#*********************************************************************************************************
# 5. Results ####
#*********************************************************************************************************

options(digits = 2, scipen = 6)

# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.
var.display <- c("year",  "AL",    "MA",    "AA",    "EAA",   "FR",  "ExF",   
                 "UAAL",  "EUAAL", "LG",    "NC",    "SC",    
                 "ADC", "EEC", "ERC",  "C", "B",     
                 "I.r" ,   "I.e",  "i",    "i.r",
                 "PR", "ADC_PR", "C_PR", 
                 # "AM", 
                 "C_ADC")

x <- penSim_results[[1]][,var.display]
kable(x, digits = 2)


# Conbine results into a data frame. 
df <- bind_rows(penSim_results) %>% 
      mutate(sim=rep(1:nsim, each=nyear)) %>%
      select(sim, year, everything())

# Running time of major modules
Time_liab # liability
Time_wf   # workforce
Time_prep_loop # Preparing for the loop
Time_loop # the big loop


(Time_liab + Time_wf + Time_prep_loop + Time_loop)[3]/60 # # of minutes
# 5.7 munites with all entry ages and 10 simulations
# 11  munites with all entry ages and 10000 simulations (8 core on home computer)

# AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
# NC: Normal Cost  
# MA: Market value of assets.
# AA: Actuarial value of assets.
# EAA:Expected actuarial value of assets.(used with asset smoothing method in TPAF)
# UAAL: Unfunded accrued actuarial liability, defined as AL - NC
# EUAAL:Expected UAAL.
# PR: payroll 
# LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
# AM: Amount to be amortized at period t, it is the sum of loss/gain and shortfall in ADC payment. 
# i is assumed interest rate. AMs of each period will be amortized seperately.  
# SC: Supplement cost 
# ADC: ADC
# C : Actual contribution
# C_ADC: shortfall in paying ADC (C - ADC). Note that negative number means shortall. 
# B : Total benefit payment(currently all to retirees)   
# Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
# Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
# Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
# I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
# Funded Ratio: AA / AL
# C_PR: contribution as % of payroll





