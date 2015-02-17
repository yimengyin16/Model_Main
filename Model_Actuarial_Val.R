# Actuarial Valuation in a simple setting
# Yimeng Yin
# 2/1/2015
# Note: This program consists of 3 files:
  # Model_Actuarial_Val.R
  # Model_Actuarial_Val_wf.R
  # Functions.R

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
#library(corrplot)

source("Functions.R")


# Use the path below when Don is running the program
# wvd <- "E:\\Dropbox (Personal)\\Pensions\\Pension simulation project\\How to model pension funds\\Winklevoss\\"

# Use the path below when Yimeng is running the program
# wvd <- "C:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"


 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_start <- proc.time()

# 0. Parameters ####

# Assumptions
nyear <- 100          # The simulation only contains 2 years.
nsim  <- 100         # # of sims
ncore <- 4            # # of CPU cores used in parallelled loops

benfactor <- 0.01   # benefit factor, 1% per year of yos
fasyears  <- 3      # number of years in the final average salary calculation

infl <- 0.04        # Assumed inflation
prod <- 0.01        # Assumed productivity
i <- 0.08           # Assumed interest rate
v <- 1/(1 + i)      # discount factor

# i.r <- rep(0.06, nyear) # real rate of return
set.seed(1234)
#i.r <- matrix(rnorm(nyear*nsim, mean = 0.08, sd = 0.12),nrow = nyear, ncol = nsim) 
i.r <- matrix(0.08, nrow = nyear, ncol = nsim)

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





# 1. Decrement table ####
# For now, we assume all decrement rates do not change over time. 
# Use decrement rates from winklevoss.  
load(paste0(wvd, "winklevossdata.rdata"))

# Reorganize termination table into long format
term2 <- data.frame(age = range_age) %>% left_join(select(term, age, everything())) %>% gather(ea, qxt, -age) %>%
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
 

# Notes:
# At time 1, in order to determine the liability for the age 20 entrants who are at age 110, we need to trace back 
# to the year when they are 20, which is -89. 

# scale for starting salary 
growth <- data.frame(start.year = -89:nyear) %>%
  mutate(growth = (1 + infl + prod)^(start.year - 1 ))

# Salary scale for all starting year
salary <- expand.grid(start.year = -89:nyear, ea = range_ea, age = 20:64) %>% 
  filter(age >= ea) %>%
  arrange(start.year, ea, age) %>%
  left_join(merit) %>% left_join(growth) %>%
  group_by(start.year, ea) %>%
  mutate( sx = growth*scale*(1 + infl + prod)^(age - min(age)))


# 3. Individual AL and NC by age and entry age ####

liab <- expand.grid(start.year = -89:nyear, ea = range_ea, age = range_age) %>%
  left_join(salary) %>% right_join(decrement) %>%
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate salary and benefits
  mutate(# sx = scale * (1 + infl + prod)^(age - min(age)),   # Composite salary scale
    year = start.year + age - ea,                      # year index in the simulation
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
  # Calculate actuarial liablity
  mutate(
    ALx.PUC = Bx/Bx[age == 65] * PVFBx,
    ALx.EAN.CD = ayx/ayx[age == 65] * PVFBx,
    ALx.EAN.CP = ayxs/ayxs[age == 65] * PVFBx,
    ALx.r      = ax * Bx[age == 65]             # Remaining liability(PV of unpaid benefit) for retirees, identical for all methods
    ) %>% 
  ungroup %>% 
  select(start.year, year, ea, age, everything()) 


# 4. Workforce ####

# Simulation of the workforce is done in the file below: 
source("Model_Actuarial_Val_wf.R")



# 5. Calculate Total Actuarial liabilities and Normal costs 

# Define a function to extract the variables in a single time period
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

# Extract variables in liab that will be used in the simulation, and put them in a list.
# Storing the variables this way can significantly boost the speed of the loop. 

var.names <- liab %>% select(B:ALx.r) %>% colnames()
liab_list <- alply(var.names, 1, function(var){
               alply(1:nyear,1, function(n) extract_slice(var, n))
                 }
             )
names(liab_list) <- var.names

#eg. extract "B" for year 1, a matrix is returned
liab_list[["B"]][[1]]


a <- proc.time()
extract_slice("NCx.EAN.CP",1)
extract_slice("NCx.EAN.CD",1)
extract_slice("NCx.PUC", 1)
b <- proc.time()
b-a


extract_slice("ALx.EAN.CP",1)
extract_slice("ALx.EAN.CD",1)
extract_slice("ALx.PUC", 1)
extract_slice("ALx.r", 1)


extract_slice("B", 1) # note that in the absence of COLA, within a time period older retirees receive less benefit than younger retirees do.
b <- proc.time()
b-a


# Total AL for Active participants
sum(wf_active[, , 1] * extract_slice("ALx.EAN.CP",1))
sum(wf_active[, , 1] * extract_slice("ALx.EAN.CD",1))
sum(wf_active[, , 1] * extract_slice("ALx.PUC",1))


# Total Normal Costs
sum(wf_active[, , 1] * extract_slice("NCx.EAN.CP",1))
sum(wf_active[, , 1] * extract_slice("NCx.EAN.CD",1))
sum(wf_active[, , 1] * extract_slice("NCx.PUC",1))

# Total AL for retirees
sum(wf_retired[, , 1] * extract_slice("ALx.r",1))

# Total benefit payment
sum(wf_retired[, , 1] * extract_slice("B",1))


# x <- liab %>% filter(start.year == -88) %>% as.data.frame
# Todo:
# vesting
# retirement benefit for disabled. 



# 6. Actuarial Valuation

# Now we do the actuarial valuation at period 1 and 2. 
# In each period, following values will be caculated:
  # AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
  # NC: Normal Cost  
  # AA: Value of assets.
  # UAAL: Unfunded accrued actuarial liability, defined as AL - NC
  # EUAAL:Expected UAAL. 
  # LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
           # i is assumed interest rate. ELs of each period will be amortized seperately.  
  # SC: Supplement cost 
  # C : Actual contribution, assume that C(t) = NC(t) + SC(t)
  # B : Total beneift Payment   
  # Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
  # Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
  # Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
  # I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
  # S : Total payrol
  # Funded Ratio: AA / AL

# Formulas
  # AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
  # AA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
  # I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
  # Ia(t) = i * AA(t)
  # Ib(t) = i * B(t)
  # Ic(t) = i * C(t)
  # EI(t) = Ia(t) - Ib(t) + Ic(t) 
  # C(t) = NC(t) + SC(t)
  # UAAL(t) = AL(t) - AA(t)
  # EUAAL(t) = [UAAL(t-1) + NC(t-1)](1+i(t-1)) - C(t-1) - Ic(t-1)
  # LG(t) =   UAAL(t) - EUAAL for t>=2 ; LG(1) = -UAAL(1) (LG(1) may be incorrect, need to check)
  # More on LG(t): When LG(t) is calculated, the value will be amortized thourgh m years. This stream of amortized values(a m vector) will be 
    # placed in SC_amort[t, t + m - 1]
  # SC = sum(SC_amort[,t])
  # ExF = B(j) - C(j)

# About gains and losses
  # In this program, the only source of gain or loss is the difference between assumed interest rate i and real rate of return i.r,
  # which will make I(t) != Ia(t) + Ic(t) - Ib(t)




# Set up data frame
penSim0 <- data.frame(year = 1:nyear) %>%
  mutate(AL   = 0, #
         AA   = 0, #
         FR   = 0, #
         ExF  = 0, # 
         UAAL = 0, #
         EUAAL= 0, #
         LG   = 0, #
         NC   = 0, #
         SC   = 0, #
         C    = 0, #
         B    = 0, #                        
         I.r  = 0, #                        
         I.e  = 0, #
         Ia   = 0, #                         
         Ib   = 0, #                         
         Ic   = 0, #  
         i    = i,
         i.r  = 0)

# matrix representation of amortization: better visualization but large size, used in this excercise
SC_amort0 <- matrix(0, nyear + m, nyear + m)
#SC_amort0
# data frame representation of amortization: much smaller size, can be used in real model later.
#SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))


cl <- makeCluster(ncore) 
registerDoParallel(cl)

start_time_loop <- proc.time()

#penSim_results <- list()
#for(k in 1:nsim){

penSim_results <- foreach(k = 1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
# k <- 1
  # initialize
  penSim <- penSim0
  SC_amort <- SC_amort0 
  penSim[,"i.r"] <- i.r[, k]
  
for (j.year in 1:nyear){
  # j.year <- 1
  # AL(j)

   penSim[j.year, "AL"] <- sum(wf_active[, , j.year] * liab_list[[paste0("ALx.", actuarial_method)]][[j.year]]) + 
                                     sum(wf_retired[, , j.year] * liab_list[["ALx.r"]][[j.year]])
  # NC(j)
   penSim[j.year, "NC"] <- sum(wf_active[, , j.year] * liab_list[[paste0("NCx.", actuarial_method)]][[j.year]]) 
     
  # B(j)
   penSim[j.year, "B"] <-  sum(wf_retired[, , j.year] * liab_list[["B"]][[j.year]])

  # for testing purpose
  # penSim[penSim$year == j.year, "B"] <-  sum(extract_slice("B",j.year))
  # penSim[penSim$year == j.year, "B"] <-  sum(wf_retired[, , j.year])

  
  # AA(j)  
  if(j.year == 1) penSim[j.year, "AA"] <- switch(init_AA,
                                                      AA0 = AA_0,                           # Use preset value
                                                      AL0 = penSim[j.year, "AL"]) # Assume inital fund equals inital liability. 
  if(j.year > 1)  penSim[j.year, "AA"] <- with(penSim, AA[j.year - 1] + I.r[j.year - 1] + C[j.year - 1] - B[j.year - 1])
  
  # UAAL(j)
  penSim$UAAL[j.year] <- with(penSim, AL[j.year] - AA[j.year]) 
  
  # LG(j)
  if (j.year == 1){
    penSim$EUAAL[j.year] <- 0
    penSim$LG[j.year] <- with(penSim,  UAAL[j.year])
  }
  if (j.year > 1){
    penSim$EUAAL[j.year] <- with(penSim, (UAAL[j.year - 1] + NC[j.year - 1])*(1 + i[j.year - 1]) - C[j.year - 1] - Ic[j.year - 1])
    penSim$LG[j.year] <- with(penSim,  UAAL[j.year] - EUAAL[j.year])
  }   
  
  # Amortize LG(j)
   SC_amort[j.year, j.year:(j.year + m - 1)] <- amort_LG(penSim$LG[penSim$year == j.year], i, m, g, end = FALSE, method = amort_method)  
  
  # Supplemental cost in j.year
   penSim$SC[j.year] <- sum(SC_amort[, j.year])

  
  # C(j)
  penSim$C[j.year] <- with(penSim, NC[j.year] + SC[j.year]) 
  
  # Ia(j), Ib(j), Ic(j)
  penSim$Ia[j.year] <- with(penSim, AA[j.year] * i[j.year])
  penSim$Ib[j.year] <- with(penSim,  B[j.year] * i[j.year])
  penSim$Ic[j.year] <- with(penSim,  C[j.year] * i[j.year])
  
  # I.e(j)
  penSim$I.e[j.year] <- with(penSim, Ia[j.year] + Ic[j.year] - Ib[j.year])
  
  # I.r(j)
  penSim$I.r[j.year] <- with(penSim, i.r[j.year] *( AA[j.year] + C[j.year] - B[j.year]))

  # Funded Ratio
  penSim$FR[j.year] <- with(penSim, AA[j.year] / AL[j.year])
 
  # External fund
  penSim$ExF[j.year] <- with(penSim, B[j.year] - C[j.year])
}
  
  
#penSim_results[[k]] <- penSim
penSim
}

end_time_loop <- proc.time()

stopCluster(cl)




getOption("scipen")
options(digits = 2, scipen = 3)
#View(penSim_results[[1]])
kable(penSim_results[[2]], digits = 3)
#kable(penSim_results[[2]], digits = 3)

#SC_amort
#penSim_results


time_end <- proc.time()

Time <- time_end - time_start 
Time_loop <- end_time_loop - start_time_loop 



## Original Performance:

# 5 simulations
# sequential: 35.45 sec
# 4 cores:    19.78 sec
# 8 cores:    18.54 sec

# 10 simulations
# 4 cores:    26.75  21.19
# 8 cores:    25.84  19.27 

# 30 sims
# 4 cores:    59.66/ 53.39
# 8 cores:    47.53/ 39.36

# 100 sims
# 8 cores:     130.93/120.36


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


#save(penSim_results, Time, Time_loop, file = "penSim_results5k.Rdata")


Time
Time_loop




