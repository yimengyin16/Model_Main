# This program calculates actuarial liabilities, normal costs, benefits, and other values that will be used
# in the simulation model

start_time_liab <- proc.time()

#*************************************************************************************************************
#                                     1. Decrement table ####
#*************************************************************************************************************

# Notes
 # 1) For now, we assume all decrement rates do not change over time.  
 # 2) Use decrement rates from winklevoss.  
 # 3) Now assume the decrement tables contain multiple decrement rates(probabilities) rather than single decrement rates.
      # If the decrement tables provide single decrement rates, we need to convert them to multiple decrement rates in a consistent way.   
      # At least for TPAF, the multiple decrement rates (probabilities) are provided in AV.  

# Timing of decrements
 # Time period t is defined as the time interval [t, t+1), closed at the beginning and open at the end. 
 # Assume retirement is independent of all other risks and occurs at the beginning of time period t, with the probability qxr(t).
   # Individual's status at t becomes "retired" immediately after the risk of retirement is realized at the beginning of t.    
 # The occurence of death, disability and termination follow UUD over period t. 
 # payment of retirement benefit occurs at the beginning of t. Hence all retirees will recieve benefit at least once, at the very moment when
   # they become retirees. 
 # Given the assumptions above, it follows that (' indicates single decrement rates)
   # qe = qe'
   # qt = qt'(1 - 0.5qm')(1 - 0.5 qd')(1 - qe'), (qd, qm are similar), note that qd=qm=qt=0 at max retirement age, when qe' = 1
   # p  = 1 - qe - qt - qm - qd
 # We assume qe, qt, qd, qm are directly available from data.     


load("Data/winklevossdata.RData")
term3 %<>% mutate(qxt.p = ifelse(age >= r.min & yos >= r.yos, 0, qxt.p)) # coerce termination rates to 0 when eligible for early retirement. 


# select(term3, -yos) %>% spread(ea, qxt.p)

# Create decrement table and calculate probability of survival
decrement <- expand.grid(age = range_age, ea = range_ea) %>% 
  left_join(filter(gam1971, age>=20)) %>%    # mortality 
  left_join(term3) %>%                       # termination
  left_join(disb)  %>%                       # disability
  left_join(dbl)   %>%                       # mortality for disabled
  left_join(er)    %>%                       # early retirement
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>% 
  filter(age >= ea) %>%
  group_by(ea) 

decrement$qxe.p <- na2zero(decrement$qxe.p)
decrement$qxe.p <- ifelse(decrement$age == r.max, 1, 0) # Single retirement age. 

# Timing of decrements
  
decrement %<>% 
  # For active(".a"). 
  mutate(qxt.a   = ifelse(age >= r.max, 0, qxt.p),   # qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2),
         qxd.a   = ifelse(age >= r.max, 0, qxd.p),   # (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
         qxm.a   = ifelse(age >= r.max, 0, qxm.p),   # (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
         qxr.a   = qxe.p                             # ifelse(age == 64, (1 - qxt.p)*(1 - qxd.p)*(1 - qxm.p), 0)
  ) %>%
  
  # For terminated(".t"), target status are dead and retired.
  # Terminated workers will never enter the status of "retired". Rather, they will begin to receive pension benefits 
  # when reaching age r.max, but still with the status "terminated". So now we do not need qxr.t
  mutate(qxm.t   = qxm.p) %>%
  
  # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
  # Note the difference from the flows 3Darray.R. Disabled can not become retired here. 
  mutate(qxm.d = qxmd.p ) %>%
  
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  mutate(qxm.r   = qxm.p) %>% 
  
  # Calculate various survival probabilities
  mutate( pxm = 1 - qxm.p,
          pxT = 1 - qxt.p - qxd.p - qxm.p - qxe.p, #(1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          pxRm = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm))) # prob of surviving up to r.max, mortality only
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
          )



#*************************************************************************************************************
#                                       2. Salary  #### 
#*************************************************************************************************************

# source the the script below or import the compelte salary data frame from other source.
source("Model_Actuarial_Val_Salary_Benefit.R")

#
# We start out with the case where 
# (1) the starting salary at each entry age increases at the rate of productivity growth plus inflation.
# (2) The starting salary at each entry age are obtained by scaling up the the salary at entry age 20,
#     hence at any given period, the age-30 entrants at age 30 have the same salary as the age-20 entrants at age 30. 
# 
# Notes:
# At time 1, in order to determine the liability for the age 20 entrants who are at age 110, we need to trace back 
# to the year when they are 20, which is -89. 

# # scale for starting salary 
# growth <- data.frame(start.year = -89:nyear) %>%
#   mutate(growth = (1 + infl + prod)^(start.year - 1 ))
# 
# # Salary scale for all starting year
# salary <- expand.grid(start.year = -89:nyear, ea = range_ea, age = 20:(r.max - 1)) %>% 
#   filter(age >= ea, start.year + 110 - ea >= 1 ) %>%
#   arrange(start.year, ea, age) %>%
#   left_join(merit) %>% left_join(growth) %>%
#   group_by(start.year, ea) %>%
#   mutate( sx = growth*scale*(1 + infl + prod)^(age - min(age)))



#*************************************************************************************************************
#                     3. Individual AL and NC by age and entry age ####
#*************************************************************************************************************

# variables relevant to COLA: B, ax, ALx.r

liab <- expand.grid(start.year = -89:nyear, ea = range_ea, age = range_age) %>%
  filter(start.year + 110 - ea >= 1)   %>%  # drop redundant combinations of start.year and ea. 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  left_join(salary) %>%
  left_join(avgben) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  right_join(decrement) %>%
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate salary and benefits
  mutate(
    # vrx = v^(r.max-age),                             # discount factor
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale.
    Bx = benfactor * yos * fas,                        # accrued benefits payable at age r.max
    bx = lead(Bx) - Bx,                                # benefit accrual at age x
    B  = ifelse(age>=r.max, Bx[age == r.max] * COLA.scale/COLA.scale[age == r.max], 0), # annual benefit # NOT COMPATIBLE WITH MULTIPLE RETIREMENT AGES!!!
    B.init = ifelse(start.year < 1 & age >= r.max, avgben[which(!is.na(avgben))] * COLA.scale/COLA.scale[which(!is.na(avgben))], 0), # Calculte future benefits of initial retirees.
    # B  = rowSums(cbind(B, B.init), na.rm = TRUE),

    ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at 110 for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age 110. 
    axR = c(get_tla(pxT[age<r.max],i), rep(0, 110 - r.max + 1)),                      # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age<r.max],i, sx[age<r.max]), rep(0, 110 - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age<r.min],i), rep(0, 110 - r.min + 1))),                          
    axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age<r.min],i, sx[age<r.min]), rep(0, 110 - r.min + 1))),      
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, 110 - r.max)),                     # need to make up the length of the vector up to age 110
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, 110 - r.max))   # need to make up the length of the vector up to age 110
  )
  
# c1 <- !is.na(liab$B) & liab$B!=0
# c2 <- !is.na(liab$B.init) & liab$B.init!=0
# cbind(c1, c2)
# range(c1+c2) # Good if only 0 and 1

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab %<>%   
  mutate(gx.r  = ifelse(age %in% r.min:r.max, 1 - 12 * (r.max - age) * 0.0025 , 0), # reduction factor for early retirement benefits
         TCx.r = gx.r * Bx * qxr.a * ax,  # term cost of retirement
         PVFBx.r = c(get_PVFB(pxT[age <= r.max], v, TCx.r[age <= r.max]), rep(0, 110 - r.max)),
         
         ## NC and AL of UC
         # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
         # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         
         # NC and AL of PUC
         TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax)), # Note that this is not really term cost 
         NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, 110 - r.max)),
         ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, 110 - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD = ifelse(age < r.max, PVFBx.r[age == min(age)]/ayx[age == r.max], 0),
         ALx.EAN.CD = PVFBx.r - NCx.EAN.CD * axR,
         # NC and AL of EAN.CP
         NCx.EAN.CP = ifelse(age < r.max, sx * PVFBx.r[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
         ALx.EAN.CP = PVFBx.r - NCx.EAN.CP * axRs,
         ALx.r      = ifelse(age < r.max, 0, ax * B)  # Remaining liability(PV of unpaid benefit) for retirees, identical for all methods  # NOT COMPATIBLE WITH MULTIPLE RETIREMENT AGES!!!
  ) 


# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begins to receive deferred retirement benefit at r.max.
# Notes on deferred retirement benefits for vested terms. 
  # 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
  #    at age r.manx
  # 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. Note that
  #    at least for EAN method. 
  #    
  # WARNING: There will be a problem of actives entering after r.min can get vested, when PVFB is only amortized up to age r.min   


liab %<>% 
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0), # actives become vested after reaching v.yos years of yos
         TCx.v = gx.v * Bx * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max],  # term cost of vested termination benefits
         PVFBx.v = c(get_PVFB(pxT[age < r.max], v, TCx.v[age < r.max]), rep(0, 110 - r.max + 1)),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).         
         
         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/ayx[age == r.min], 0), # Note that NC is 0 after age r.min - 1
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.min]) * sx, 0),  # Note that NC is 0 after age r.min - 1
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
         )

# Calculate AL and benefit payment for vested terms terminating at different ages.   
liab.term <- expand.grid(start.year = (1 - (r.max - 1 - 20)):nyear, ea = range_ea[range_ea < r.min], age = range_age, age.term = range_age[range_age < r.max]) %>% # start year no longer needs to start from -89 if we import initial benefit data.
  filter(start.year + 110 - ea >= 1, age >= ea, age.term >= ea) %>% # drop redundant combinations of start.year and ea. 
  arrange(start.year, ea, age.term, age) %>%
  group_by(start.year, ea, age.term) %>% 
  left_join(liab %>% select(start.year, year, ea, age, Bx, gx.v, ax, COLA.scale, pxRm)) %>% 
  mutate(year.term = year[age == age.term],
         #year.term = year - (age - age.term),
         Bx.v  = gx.v * Bx,
         B.v   = ifelse(age >= r.max, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.max], 0),  # Benefit payment after r.max  
         ALx.v = ifelse(age < r.max, Bx.v[age == unique(age.term)] * ax[age == r.max] * pxRm * v^(r.max - age),
                                  B.v * ax)  
         )
  
liab %<>% ungroup %>% select(start.year, year, ea, age, everything()) 
liab.term %<>% ungroup %>% select(-start.year, -age.term, -Bx, -Bx.v, -gx.v, -ax, -COLA.scale, -pxRm) 



#*************************************************************************************************************
#             4. Prepare data frames that are ready to be used with workforce data ####
#*************************************************************************************************************

# Choosing AL and NC variables corresponding to the chosen acturial methed
ALx.method <- paste0("ALx.", actuarial_method)
NCx.method <- paste0("NCx.", actuarial_method)
ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")

var.names <- c("sx", "B", "ALx.r", ALx.method, NCx.method, ALx.v.method, NCx.v.method)
liab %<>% 
  filter(year %in% 1:100) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx" = ALx.method, "NCx" = NCx.method, "ALx.v" = ALx.v.method, "NCx.v" = NCx.v.method) %>% # Note that the positions of old names and new names are reversed when using dplyr::rename_
  right_join(expand.grid(year=1:100, ea=range_ea, age=range_age))
liab <- colwise(na2zero)(liab)


liab.term %<>% 
  filter(year %in% 1:100) %>% 
  right_join(expand.grid(year = 1:100, ea = range_ea, age = range_age, year.term = 1:100))
liab.term <- colwise(na2zero)(liab.term)


end_time_liab <- proc.time()
Time_liab <- end_time_liab - start_time_liab
Time_liab







# Some comparisons

# microbenchmark(
# ll2[["NCx.PUC"]]$`3` + ll2[["NCx.PUC"]]$`3`,
# ll2$NCx.PUC$`3` + ll2$NCx.PUC$`3`, times = 1000
# )      
# Result: appox. the same

# microbenchmark( lldf %>% filter(variable == "sx", year == 1),
#                 ll2[["sx"]][[1]] # this is much f
#                 , times = 100)  
# Result: latter is much faster. 

# compare year 3 for variable NCx.PUC
# ll2$NCx.PUC$`3`


# x <- data.frame(a = 1:2, b = 3:4)
# y <- "a"
# rename_(x, "A" = y )



