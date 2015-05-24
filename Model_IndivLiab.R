# This program calculates actuarial liabilities, normal costs, benefits, and other values that will be used
# in the simulation model

start_time_liab <- proc.time()




#*************************************************************************************************************
#                                       2. Salary  #### 
#*************************************************************************************************************

# This part will no longer be used. It is kept here just for reference. 
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


# Inputs:
  # Data frames:
  #  - salary table: history and prospect salary for all start.year, age and ea combos
  #  - avgben table: benefit payments for all age and ea combos at period 1
  #  - decrement tables
  # Parameters:
  #  - max.age, min.age
  #  - nyear
  #  - range_ea, range_age
  #  - fasyears
  #  - benfactor
  #  - cola
  #  - r.max, r.min
  #  - v.yos
  #  - i
  #  - actuarial_method     
# Output
  # liab     : individual liabilities of actives by year, ea, and age
  # liab.term: individual liabilities of terms   by year, year.term, ea and age 

# Notes:
  # variables relevant to COLA: B, ax, ALx.r




liab <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= 1)   %>%  # drop redundant combinations of start.year and ea. 
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
    B  = rowSums(cbind(B, B.init), na.rm = TRUE),

    ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    axR = c(get_tla(pxT[age<r.max],i), rep(0, max.age - r.max + 1)),                      # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age<r.max],i, sx[age<r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    axr = c(get_tla(pxT[age<r.min],i), rep(0, max.age - r.min + 1)),                      # Similar to axR, but based on r.min        
    axrs= c(get_tla(pxT[age<r.min],i, sx[age<r.min]), rep(0, max.age - r.min + 1)),       # Similar to axRs, but based on r.min   
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )
  
# c1 <- !is.na(liab$B) & liab$B!=0
# c2 <- !is.na(liab$B.init) & liab$B.init!=0
# cbind(c1, c2)
# range(c1+c2) # Good if only 0 and 1

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab %<>%   
  mutate(gx.r  = ifelse(age %in% r.min:r.max, 1 - 12 * (r.max - age) * 0.0025 , 0), # reduction factor for early retirement benefits
         TCx.r = gx.r * Bx * qxr.a * ax,  # term cost of retirement
         PVFBx.r = c(get_PVFB(pxT[age <= r.max], v, TCx.r[age <= r.max]), rep(0, max.age - r.max)),
         
         ## NC and AL of UC
         # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
         # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         
         # NC and AL of PUC
         TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax)), # Note that this is not really term cost 
         NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
         ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
         
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
  # 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
  #    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the 
  #    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balance. 
  #    
  # WARNING: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min   

liab %<>% 
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0), # actives become vested after reaching v.yos years of yos
         Bx.v  = gx.v * Bx,
         TCx.v = Bx.v * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max],  # term cost of vested termination benefits
         PVFBx.v = c(get_PVFB(pxT[age < r.max], v, TCx.v[age < r.max]), rep(0, max.age - r.max + 1)),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).         
         
         # NC and AL of PUC
         TCx.vPUC = TCx.v / (age - min(age)),
         NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
          
         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/ayx[age == r.min], 0), # Note that NC is 0 after age r.min - 1
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.min]) * sx, 0),  # Note that NC is 0 after age r.min - 1
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
         ) %>% 
  ungroup %>% select(start.year, year, ea, age, everything()) 



# Calculate AL and benefit payment for vested terms terminating at different ages.   
# liab.term <- expand.grid(start.year = (1 - (r.max - 1 - 20)):nyear, ea = range_ea[range_ea < r.min], age = range_age, age.term = range_age[range_age < r.max]) %>% # start year no longer needs to start from -89 if we import initial benefit data.
#   filter(start.year + 110 - ea >= 1, age >= ea, age.term >= ea) %>% # drop redundant combinations of start.year and ea. 
#   # arrange(start.year, ea, age.term, age) %>% # Very slow. Uncomment it only when we want to examine liab.term.
#   group_by(start.year, ea, age.term) %>% 
#   left_join(liab %>% select(start.year, year, ea, age, Bx.v, ax, COLA.scale, pxRm)) %>% 
#   mutate(year.term = year[age == age.term],
#          #year.term = year - (age - age.term),
#          B.v   = ifelse(age >= r.max, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.max], 0),  # Benefit payment after r.max  
#          ALx.v = ifelse(age < r.max, Bx.v[age == unique(age.term)] * ax[age == r.max] * pxRm * v^(r.max - age),
#                                   B.v * ax)  
#          )

# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(start.year = (1 - (r.max - 1 - min.age)):nyear, ea = range_ea[range_ea < r.min], age = range_age, age.term = range_age[range_age < r.max]) %>% # start year no longer needs to start from -89 if we import initial benefit data.
  filter(start.year + max.age - ea >= 1, age >= ea, age.term >= ea) %>% 
  data.table(key = "ea,age,start.year,age.term")# drop redundant combinations of start.year and ea. 
liab.term <- merge(liab.term,
                   select(liab, start.year, year, ea, age, Bx.v, ax, COLA.scale, pxRm) %>% data.table(key = "ea,age,start.year"), 
                   all.x = TRUE, by = c("ea", "age","start.year"))

liab.term %<>% as.data.frame %>% 
  # arrange(start.year, ea, age.term, age) %>% # Very slow. Uncomment it only when we want to examine liab.term.
  group_by(start.year, ea, age.term) %>% 
  mutate(year.term = year[age == age.term],
         #year.term = year - (age - age.term),
         B.v   = ifelse(age >= r.max, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.max], 0),  # Benefit payment after r.max  
         ALx.v = ifelse(age < r.max, Bx.v[age == unique(age.term)] * ax[age == r.max] * pxRm * v^(r.max - age),
                                  B.v * ax)  
         ) %>% 
  ungroup %>% 
  select(-start.year, -age.term, -Bx.v, -ax, -COLA.scale, -pxRm) %>% 
  filter(year %in% 1:nyear)
# liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])



# Choosing AL and NC variables corresponding to the chosen acturial methed
ALx.method <- paste0("ALx.", actuarial_method)
NCx.method <- paste0("NCx.", actuarial_method)
ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")

var.names <- c("sx", "B", "ALx.r", ALx.method, NCx.method, ALx.v.method, NCx.v.method)
liab %<>% 
  filter(year %in% 1:nyear) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx" = ALx.method, "NCx" = NCx.method, "ALx.v" = ALx.v.method, "NCx.v" = NCx.v.method) # Note that dplyr::rename_ is used. 
  # liab[-(1:3)] <- colwise(na2zero)(liab[-(1:3)])






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



