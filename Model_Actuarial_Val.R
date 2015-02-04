# Actuarial Valuation in a simple setting
# Yimeng Yin
# 2/1/2015
# Expected to be finished on 2/4/2015

# Goal: 
# 1. Conduct an actuarial valuation at time 1 based on plan design, actuarial assumptions, and plan data.
# 2. Conduct an actuarial valuation at time 2 based on the plan experience during the period [1, 2), and calculate
#    supplemental costs at time 2 based on the experience gain/loss or assumption changes. 

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
  # Decrements: Mortality and termination
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


# Preamble ##############################################################################################

library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
#library(corrplot)

source("Functions.R")

wvd <- "E:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"


# 0. Parameters implied by assumptions ####
benfactor <- 0.01  # benefit factor, 1% per year of yos
fasyears  <- 3      # number of years in the final average salary calculation
infl <- 0.04        # Assumed inflation
prod <- 0.01        # Assumed productivity
i <- 0.08           # Assumed interest rate
v <- 1/(1 + i)      # discount factor
nyear <- 2         # The simulation only contains 2 years.


# 1. Decrement table ####
# For now, we assume all decrement rates do not change over time. 
# Use decrement rates from winklevoss.  
load(paste0(wvd, "winklevossdata.rdata"))

# Reorganize termination table into long format
term2 <- data.frame(age = 20:110) %>% left_join(select(term, age, everything())) %>% gather(ea, qxt, -age) %>%
  mutate(ea = as.numeric(gsub("[^0-9]", "", ea)))

# Create decrement table and calculate probability of survival
decrement <- filter(gam1971, age>=20) %>% left_join(term2) %>% left_join(disb) %>% # survival rates
  select(ea, age, everything()) %>% 
  arrange(ea, age) %>% 
  filter(age >= ea) %>%
  group_by(ea) %>%
  # Calculate survival rates
  mutate( pxm = 1 - qxm,
          pxT = (1 - qxm) * (1 - qxt) * (1 - qxd),
          px65m = order_by(-age, cumprod(ifelse(age >= 65, 1, pxm))), # prob of surviving up to 65, mortality only
          px65T = order_by(-age, cumprod(ifelse(age >= 65, 1, pxT))), # prob of surviving up to 65, composite rate
          p65xm = cumprod(ifelse(age <= 65, 1, lag(pxm))))            # prob of surviving to x from 65, mortality only



# 2. Salary scale #### 
# We start out with the case where 
# (1) the starting salary at each entry age increases at the rate of productivity growth plus inflation.
# (2) The starting salary at each entry age are obtained by scaling up the the salary at entry age 20,
#     hence at any given period, the age-30 entrants at age 30 have the same salary as the age-20 entrants at age 30. 
# The first step is to produce tables for a given year's starting salary. 

# Notes:
# At time 1, in order to determine the liability for the age 20 entrants who are at age 110, we need to trace back 
# to the year when they are 20, which is -89. 

# scale for starting salary 
growth <- data.frame(start.year = -89:nyear) %>%
  mutate(growth = (1 + infl + prod)^(start.year - 1 ))

# Salary scale for all starting year
salary <- expand.grid(start.year = -89:nyear, ea = seq(20, 60, 5), age = 20:64) %>% 
  filter(age >= ea) %>%
  arrange(start.year, ea, age) %>%
  left_join(merit) %>% left_join(growth) %>%
  group_by(start.year, ea) %>%
  mutate( sx = growth*scale*(1 + infl + prod)^(age - min(age)))

salary %>% filter(start.year == -89) %>% as.data.frame

# 3. Individual AL and NC by age and entry age ####

liab <- expand.grid(start.year = -89:nyear, ea = seq(20, 60, 5), age = 20:110) %>%
  left_join(salary) %>% right_join(decrement) %>%
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate salary and benefits
  mutate(# sx = scale * (1 + infl + prod)^(age - min(age)),   # Composite salary scale
    year = start.year + age -ea,  # year index in the simulation
    vrx = v^(65-age),                                  # discount factor
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    Bx = benfactor * yos * fas,                        # accrued benefits
    bx = lead(Bx) - Bx,                                # benefit accrual at age x
    ax = ifelse(age < 65, NA, get_tla(pxm, i)),        # Since retiree die at 110 for sure, the life annuity is equivalent to temporary annuity up to age 110. 
    ayx = c(get_tla2(pxT[age <= 65], i), rep(0, 45)),                # need to make up the length of the vector up to age 110
    ayxs= c(get_tla2(pxT[age <= 65], i,  sx[age <= 65]), rep(0, 45)),  # need to make up the length of the vector up to age 110
    B   = ifelse(age>=65, Bx[age == 65], 0)            # annual benefit 
  ) %>%
  # Calculate normal costs (following Winklevoss, normal costs are calculated as a multiple of PVFB)
  mutate(
    PVFBx = Bx[age == 65] * ax[age == 65] * vrx * px65T,
    NCx.PUC = bx * ax[age == 65] * px65T * vrx,                                         # Normal cost of PUC
    NCx.EAN.CD = PVFBx[age == min(age)] / ayx[age == 65],                               # Normal cost of EAN, constant dollar
    NCx.EAN.CP = PVFBx[age == min(age)] / (sx[age == min(age)] * ayxs[age == 65]) * sx  # Normal cost of EAN, constant percent
  ) %>% 
  # Calculate accrued liablity
  mutate(
    ALx.PUC = Bx/Bx[age == 65] * PVFBx,
    ALx.EAN.CD = ayx/ayx[age == 65] * PVFBx,
    ALx.EAN.CP = ayxs/ayxs[age == 65] * PVFBx,
    ALx.r      = ax * Bx[age == 65]             # Remaining liability(PV of unpaid benefit) for retirees, identical for all methods
    ) %>% 
  select(start.year, year, ea, age, everything()) 

# 4. Workforce ####

# The workforce can be discribed by a slice of the workforce 3-D array 

range_ea  <- seq(20, 60, 5) # For now, assume new entrants only enter the workforce with interval of 5 years. 
range_age <- 20:110 
nyears    <- 2 # For time 0 and 1

# Simulation of the workforce is done in the file below: 
source("Model_Actuarial_Val_wf.R")



# 5. Calculate Total Actuarial liabilities and Normal costs 

# Extract the variables in a single time period

extract_slice <- function(Var, Year,  data = liab){
  # This function extract information for a specific year.
  # inputs:
    # Year: numeric
    # Var : character, variable name 
    # data: name of the data frame
  # outputs:
    # Slice: data frame. A data frame with the same structure as the workforce data.
  Slice <- data %>% ungroup %>% filter(year == Year) %>% 
    select_("ea", "age", Var) %>% arrange(ea, age) %>% spread_("age", Var, fill = 0)
  rownames(Slice) = Slice$ea
  Slice %<>% select(-ea) %>% as.matrix
  return(Slice)
}

extract_slice("NCx.EAN.CP",1)
extract_slice("NCx.EAN.CD",1)
extract_slice("NCx.PUC", 1)

extract_slice("ALx.EAN.CP",1)
extract_slice("ALx.EAN.CD",1)
extract_slice("ALx.PUC", 1)
extract_slice("ALx.r", 1)

extract_slice("B", 1) # note that in the absence of COLA, within a time period older retirees receive less benefit than younger retirees do.



# Total AL for Active participants
sum(wf_active[, , 1] * extract_slice("ALx.EAN.CP",1))
sum(wf_active[, , 1] * extract_slice("ALx.EAN.CP",2))

sum(wf_active[, , 1] * extract_slice("ALx.EAN.CD",1))
sum(wf_active[, , 1] * extract_slice("ALx.EAN.CD",2))

sum(wf_active[, , 1] * extract_slice("ALx.PUC",1))
sum(wf_active[, , 1] * extract_slice("ALx.PUC",2))


# Total Normal Costs
sum(wf_active[, , 1] * extract_slice("NCx.EAN.CP",1))
sum(wf_active[, , 1] * extract_slice("NCx.EAN.CP",2))

sum(wf_active[, , 1] * extract_slice("NCx.EAN.CD",1))
sum(wf_active[, , 1] * extract_slice("NCx.EAN.CD",2))

sum(wf_active[, , 1] * extract_slice("NCx.PUC",1))
sum(wf_active[, , 1] * extract_slice("NCx.PUC",2))

# Total AL for retirees
sum(wf_retired[, , 1] * extract_slice("ALx.r",1))
sum(wf_retired[, , 1] * extract_slice("ALx.r",2))

# Total benefit payment
sum(wf_retired[, , 1] * extract_slice("B",1))
sum(wf_retired[, , 1] * extract_slice("B",2))




x <- liab %>% filter(start.year == -88) %>% as.data.frame
x


























