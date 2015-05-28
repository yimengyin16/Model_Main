#*********************************************************************************************************
# 0.1 Global Parameters ####
#*********************************************************************************************************

Global_paramlist <- list(
## Model parameters
nyear = 100,        # # of years in simulation 
nsim  = 10,         # # of sims
ncore = 6 ,         # # of CPU cores used in parallelled loops


#   ## Restrictions on ea and age ranges
  max.age = 120,
  min.age = 20,
  
  max.ea = 70,
  min.ea = 20

# ## Range of age allowed.
# range_age = 20:120,
# 
# ## Range of entry age allowed. 
# # range_ea  = c(seq(20, 70, 5), 69 ) # Assume new entrants only enter the workforce with interval of 5 years. Note that max entry age must be less than max retirement age.  
# # range_ea = c(20, 25, 30, 35, 40:70)       # "Continuous" entry ages after 45
# range_ea = 20:70                        # Complete range of entry ages. Most time comsuming. 
)

#*********************************************************************************************************
# 0.2 Prototype Parameters ####
#*********************************************************************************************************

# Parameters can be read from "RunControl.xlsx"

# Restrictions on current version
# 1. r.max must be in the range [55, 65]
# 2. v.yos <= r.max - r.min
# Note: The restrictions above are removed in current version since we use single retirement age and set r.min = r.max.

paramlist <- list(
  
  runname = "Devlopment",
  
  
  ## 1. Benefit structure
  benfactor = 0.015,  # benefit factor, % of final average salary per year of yos
  fasyears  = 3,      # number of years in the final average salary calculation
  # WARNING: In curret version, please do not set a r.max greater than 65 
  r.max     = 65,     # maximum retirement age, at which all actives retire with probability 1. 
  r.min     = 65,     # minimum retirement age. (for multi-retirement ages, will not be used in current version)
  # Current version depends on the assumption that active workers do not quit their jobs once reaching age r.min. 
  cola      = 0.01,   # annual growth of retirement benefits in a simple COLA 
  
  v.yos     = 10,      # yos required to be vested.  
  r.yos     = 10,      # yos required to be eligible for early retirement
  
  
  ## 2. Economic assumptions
  infl = 0.03,        # Assumed inflation
  prod = 0.01,        # Assumed productivity
  i    = 0.08,           # Assumed discount rate
  
  
  
  ## 3. Actuarial method
  actuarial_method = "EAN.CP",  # One of "PUC", "EAN.CD", "EAN.CP"
  
  
  ## 4. Amortization method 
  amort_type   = "closed",  # one of "closed" and "open".
  # Notes:
  #  "closed": Loss/gain + underfunding of ADC for each year is amortized separately using closed-ended method.
  #  "open"  : UAAL of each year is amortized using open-ended method.
  amort_method = "cd", # "cd" for Constant dollar, "cp" for constant percent, "sl" for straitline
  m = 3,                # years of amortization of gain/loss
  
  
  ## 5. Contriubtion Policy
  ConPolicy    = "ADC",
  PR_pct_cap   = 0.25,
  PR_pct_fixed = 0.145,
  # Values of ConPolicy:
  # ADC:     pay full ADC
  # ADC_cap: pay full ADC with a cap, which is defined as a proportion of payroll. If ADC_cap is chosen, set the parameter PR_pct_cap.
  # Fixed:   pay a fixed percent of payroll. If Fixed is chosen, set the parameter PR_pct_fixed.
  EEC_rate     = 0.005,   
  
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
  
  
  
  ## 6. Market value and actuarial value of assets
  # Set inital asset, market value
  MA_0 = 200,   # market value at 0. If changed, EAA_0 may also need to change. 
  init_EAA = "MA",  # "MA" for the same value as MA; "EAA" for preset value of inital EAA.
  
  # Choose asset smoothing method
  smooth_method = "method1", # one of "method1" and "method2"
  
  # Parameters for asset smoothing method 1 (PSERS) 
  # Note: The difference between expected and actual investment return will be realized over s.year years. (realize 1/s.year each year) 
  s.year = 10,    # amortization period for unexpected investment gain/loss
  
  
  # Parameters for asset smoothing method 2 (TPAF)
  # Note: actuarial asset value of current year is a weighted average of expected actuarial asset value and market asset value. 
  init_MA  = "AL",  # "MA" for preset value; "AL" for being equal to initial liability 
  EAA_0    = 200,   # expected market value at 0. Set to the same value as MA_0 by default.
  w = 1,            # weight on market value in asset smoothing; no asset smoothing when set to 1. 
  
  
  ## 7.  Growth rate of workforce
  wf_growth   = 0.00,    # growth rate of the size of workforce. For now, new entrants are equally distributed across all entry ages. 
  no_entrance = TRUE,    # No new entrants into the workforce if set "TRUE". Overrides "wf_growth"
  
  
  ## 8. Parameters for 1.1-1.3
  
  
  # Choose decrement tables
      tablename_mortality   = "gam1971.hybrid",  #  "rp2000.hybrid.f75",  # "gam1971.hybrid",  # rp2000.hybrid
      tablename_termination = "Winklevoss",

  # Choose salary scale
      planname_sscale.hist   = "average",        # "average", "underfunded" 
      planname_sscale.assume = "average",        # "average", "underfunded"

  # Choose initial population
      planname_actives  = "average",             # "average", "underfunded" 
      planname_retirees = "average",             # "average", "underfunded" 

  # Choose actual investment matrix
      ir.mean = 0.08,
      ir.sd   = 0

  
)


# Define Other parameters that depend on the fundamental parameters just imported. 
paramlist$range_age <- with(Global_paramlist, min.age:max.age)

#paramlist$range.ea  <- c(seq(20, paramlist$r.max, 5), paramlist$r.max) # Assume new entrants only enter the workforce with interval of 5 years. Note that max entry age must be less than max retirement age.  
#paramlist$range.ea  <- c(20, 25, 30, 35, 45:paramlist$r.max)           # "Continuous" entry ages after 45
paramlist$range_ea  <- c(20: (paramlist$r.max-1))                           # Complete range of entry ages. Most time comsuming. 
  

paramlist$v <- with(paramlist, 1/(1 + i))  # discount factor, just for convenience

# ## Modify range_ea based on r.max
# Global_paramlist$range_ea <- with(Global_paramlist, unique(c(range_ea[range_ea <= (paramlist$r.max - 1)], paramlist$r.max - 1))) # make sure to include r.max - 1



#*********************************************************************************************************
# 1.1 Inital population  ####
#*********************************************************************************************************

## Inputs
# data frames:
#   - data frame for initial actives
#   - data frame for initial retirees
# parameters:
#   - planname: planname to be selected from the data frames above. From RunControl
#   - range_age
#   - range_ea

## Output
#   - init_active:  matrix, max ea by max age
#   - init_retiree  matrix, max ea by max age


if(dev_mode){
  ## The following code is used for convevience when developing new features. 
  # Initial Active
  # WARNING: Ages and entry ages of active members must be less than retirement age. (max retirement age when multiple retirement ages is implemented) 
range_ea  <- paramlist$range_ea
range_age <- paramlist$range_age 
r.max     <- paramlist$r.max
  
  init_actives <- rbind(c(20, 20, 1), # (entry age,  age, number)
                       c(20, 40, 1),
                       c(20, r.max - 1, 1),
                       c(45, 45, 1),
                       c(45, r.max - 1, 1),
                       c(50, 50, 1),
                       c(55, 55, 1),
                       c(r.max - 1, r.max - 1, 1)
  ) %>% as.data.frame
  colnames(init_actives) <- c("ea", "age", "nactives")
  init_actives <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives) %>% 
    spread_("age", "nactives", fill = 0) %>% select(-ea) %>% as.matrix
  
  # Initial Retired 
  # WARNING: Ages and entry ages of retirees must be no less than retirement age. (min retirement age when multiple retirement ages is implemented)
  init_retirees <- rbind(c(20, r.max, 1),
                        c(20, 85, 1)
  ) %>% as.data.frame
  colnames(init_retirees) <- c("ea", "age", "nretirees")                 
  init_retirees <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_retirees) %>% 
    spread_("age", "nretirees", fill = 0) %>% select(-ea) %>% as.matrix

  init_pop = list(actives = init_actives, retirees = init_retirees)  

  rm(range_ea, range_age, r.max, init_actives, init_retirees)
}
