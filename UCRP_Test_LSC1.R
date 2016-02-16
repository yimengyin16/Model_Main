# This script test the modeling approach for lump sum cash flow in UCRP.



# Notes on LSC in UCRP

# The LSC is modeled as an new type of benefit.
# The probability of opting for LSC upon retirement depends on yos and member type (active, inactive, disability crossovers.) 
# The amount of LSC is cacluated as the discount value of future retirement benefit payments with the following assumptions:
#   1. discount rate 7.25% (starting from 2015)
#   2. COLA is assumed to be 2%. 
#   3. uni-sex mortality table, weighted 40% male and 60% female. 

# All calculations can be done in liab.active.   


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
library(readxl)
library(stringr)


load("Data/UCRP.inputs1.RData")
load("Data/UCRP.inputs2.RData")


source("Functions.R")




init.year <- 2015
nyear <- 40
max.age <-  120

range_ea <- c(20:74)
range_age <- 20:120
range_age.r <- 50:75


fasyears <- 3
cola     <- 0.03
i <- 0.0725
v <- 1/(1 + i)
infl <- 0.03

r.full <- 60 # age at which vested terms are assumed to retire. 
r.yos  <- 5
v.yos  <- 5 
r.min  <- min(range_age.r)
r.max  <- max(range_age.r) 


pct.F.actives <- 0.55
pct.M.actives <- 1 - pct.F.actives

pct.F.LSC <- 0.6
pct.M.LSC <- 1 - pct.F.LSC

pct.fac.actives.t76 <- 0.5
pct.stf.actives.t76 <- 1 - pct.fac.actives.t76 
pct.fac.actives.t13 <- 0.5
pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 
pct.fac.actives.tm13 <- 0.5
pct.stf.actives.tm13 <- 1 - pct.fac.actives.t13  


pct.ca <- 0.8 * pct.F.actives + 0.6 * pct.M.actives # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
pct.la <- 1 - pct.ca                                # For those opting for annuit rather than LSC, the % of choosing life annuity (100% for 2013 and modified 2013 tier)


source("UCRP_Decrements.R")


#*************************************************************************************************************
#                                Preparation                        #####                  
#*************************************************************************************************************
# Starts with a simple case with only 2 entry ages: 20 and 74

liab.active <- expand.grid(start.year = 2015 , 
                           ea = c(20:74), age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>%
  mutate(sx = 1.03^(age - ea)) %>% # left_join(.salary)  %>%  # make-up salary 

#   left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
#   right_join(.decrement) %>%

  left_join(decrement.ucrp) %>% 
  left_join(bfactor) %>%
  left_join(mortality.post.ucrp %>% filter(age == age.r) %>% select(age, ax.r.W)) %>% 
  group_by(start.year, ea) %>%
  
  
  # Chnange variable names for 1976 tier
  rename(pxT = pxT.t76,
         qxr.la = qxr.la.t76,
         qxr.LSC  = qxr.LSC.t76,
         bfactor = bf.non13) %>% 
  
  # Calculate salary and benefits
  mutate(
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = pmin(fas, na2zero(bfactor * yos * fas)),      # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                                # benefit accrual at age x

    
    # ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    # ax.r = get_tla(pxm.r, i, COLA.scale),              # ax calculated with mortality table for retirees. 
    
    ax.LSC = get_tla(1 - qxm.LSC, i, COLA.scale),        # ax calculated with mortality table for LSC. When multiplied by Bx, it is the LSC amount.
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i), rep(0, max.age - r.full + 1))),                 # Similar to axR, but based on r.full.  For calculation of term benefits when costs are spread up to r.min.        
    axrs= ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i, sx[age<r.full]), rep(0, max.age - r.full + 1))),  # Similar to axRs, but based on r.full. For calculation of term benefits when costs are spread up to r.min.
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )




#*************************************************************************************************************
#                          AL and NC of life annuity for actives                                         #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.la = ifelse(yos < r.yos, 0,  
                        ifelse(age > r.full, 1, ifelse(age %in% r.min:r.full, 1, 0))),         # eligibility rule 1
          gx.la = ifelse(start.year >= 1989, gx.la, ifelse(age >= 62, 1, gx.la)),              # eligibility rule 2
  
  Bx.la  = gx.la * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  TCx.la = lead(Bx.la) * qxr.la  * lead(ax.r.W) * v,  # term cost of retirement at the internal retirement age x (start to claim benefit at age x + 1)
  # TCx.r = Bx.r * qxr.a * ax,
  PVFBx.la  = c(get_PVFB(pxT[age <= r.max], v, TCx.la[age <= r.max]), rep(0, max.age - r.max)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
  
  # NC and AL of EAN.CD
  NCx.EAN.CD.la = ifelse(age < r.max, PVFBx.la[age == min(age)]/ayx[age == r.max], 0),
  ALx.EAN.CD.la = PVFBx.la - NCx.EAN.CD.la * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.la   = ifelse(age < r.max, sx * PVFBx.la[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
  PVFNC.EAN.CP.la = NCx.EAN.CP.la * axRs,
  ALx.EAN.CP.la   = PVFBx.la - PVFNC.EAN.CP.la
  ) 


#*************************************************************************************************************
#                          AL and benefit for retirees with life annuity                        #####                  
#*************************************************************************************************************



# # Calculate AL and benefit payment for retirees having retired at different ages. 
# liab.retiree <- rbind(
#   # grids for initial retirees in year 1
#   expand.grid(ea         = r.min - 1,
#               age.retire = .benefit$age, # This ensures that year of retirement is year 1. 
#               #age.retire = r.min,
#               start.year = 1 - (.benefit$age - (r.min - 1)),
#               age        = range_age[range_age >= r.min]) %>%
#     # mutate(year.retire = start.year + age.retire - ea) %>%  
#     filter(age >= ea + 1 - start.year),  
#   
#   # grids for who retire after year 1.
#   expand.grid(ea         = range_ea[range_ea < r.max],
#               age.retire = r.min:r.max,
#               start.year = (2 - (r.max - min(range_ea))):nyear,
#               age        = range_age[range_age >=r.min]) %>%
#     # mutate(year.retire = start.year + age.retire - ea) %>%   
#     filter(age >= ea, 
#            age.retire >= ea,
#            age >= age.retire,
#            start.year + (age.retire - ea) >= 2, # retire after year 2
#            start.year + age - ea >= 2) 
# ) %>%
#   data.table(key = "start.year,ea,age.retire,age") 
# 
# liab.retiree <- liab.retiree[!duplicated(liab.retiree %>% select(start.year, ea, age, age.retire ))]
# 
# 
# liab.retiree <- merge(liab.retiree,
#                       select(liab.active, start.year, year, ea, age, Bx, ax, ax.r, COLA.scale, benefit, gx.r) %>% data.table(key = "ea,age,start.year"), 
#                       all.x = TRUE, by = c("ea", "age","start.year")) %>% 
#   arrange(start.year, ea, age.retire)
# 
# 
# !!!!!load present value of annuity for all retirement ages, ax.r.W cannot be used anymore.  
#
# liab.retiree %<>% as.data.frame  %>% # filter(start.year == -41, ea == 21, age.retire == 65) %>% 
#   # filter(ea == 54) %>% 
#   group_by(start.year, ea, age.retire) %>%  
#   mutate(
#     year.retire = start.year + age.retire - ea,
#     Bx = ifelse(is.na(Bx), 0, Bx),
#     B.r   = ifelse(year.retire < 2,
#                    benefit[year == 1] * COLA.scale / COLA.scale[year == 1],                          # Benefits for initial retirees
#                    (gx.r * Bx)[age == age.retire] * COLA.scale / COLA.scale[age == age.retire]),
#     ALx.r = B.r * ax.r  # Liability for remaining retirement benefits.
#     
#   ) %>% ungroup %>% 
#   # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
#   select(year, ea, age, year.retire,  B.r, ALx.r)
# 



#*************************************************************************************************************
#                          AL and NC of LSC for actives                                         #####                  
#*************************************************************************************************************

 liab.active %<>%   
  mutate( 
          # gx.la also applies to LSC 
          
          Bx.LSC  = gx.la * Bx * ax.r.W,  # This is the LSC amount if the employee CLAIMs at age x, not internally retire at age x. 
          TCx.LSC = lead(Bx.LSC) * qxr.LSC  * v,  # term cost of retirement at the internal retirement age x (start to claim benefit at age x + 1)
          PVFBx.LSC  = c(get_PVFB(pxT[age <= r.max], v, TCx.LSC[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.LSC = ifelse(age < r.max, PVFBx.LSC[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.LSC = PVFBx.LSC - NCx.EAN.CD.LSC * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.LSC = ifelse(age < r.max, sx * PVFBx.LSC[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.LSC = NCx.EAN.CP.LSC * axRs,
          ALx.EAN.CP.LSC = PVFBx.LSC - PVFNC.EAN.CP.LSC
  ) 



#*************************************************************************************************************
#                              LSC payments                                                              #####                  
#************************************************************************************************************* 
 B.LSC <- liab.active %>% select(start.year, year, ea, age, yos, Bx.LSC) %>% filter(age %in% r.min:r.max)





#*************************************************************************************************************
#                          AL and NC of deferred benefits for actives                        #####
#*************************************************************************************************************

# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begin to receive deferred retirement benefit at r.full.
# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min

liab.active %<>%
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0),                                     # actives become vested after reaching v.yos years of yos
         gx.v = ifelse(start.year >= 1989, gx.v, ifelse(age >= 62, 1, gx.v)),   # eligibility rule 2
         
         Bx.v = ifelse(ea < r.full, gx.v * pmin(fas, na2zero(bfactor[age == r.full] * yos * fas))* (1 + infl)^(r.full - age), 0), # initial annuity amount when the vested term retires at age r.full. Accrued benefit is CPI adjustd. 

         TCx.v   = ifelse(ea < r.full, Bx.v * qxt * lead(px_r.full_m) * v^(r.full - age) * ax.r.W[age == r.full], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.full.
         PVFBx.v = ifelse(ea < r.full, c(get_PVFB(pxT[age < r.full], v, TCx.v[age < r.full]), rep(0, max.age - r.full + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         #        TCx.v = Bx.v * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max],  # term cost of vested termination benefits
         #        PVFBx.v = c(get_PVFB(pxT[age < r.max], v, TCx.v[age < r.max]), rep(0, max.age - r.max + 1)),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),

         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/ayx[age == r.full], 0), # for testing spreading NC.v up to r.full
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,

         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.full]) * sx, 0),  # for testing spreading NC.v up to r.full
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
  ) %>%
  ungroup %>% select(start.year, year, ea, age, everything())
  

# x <- liab.active %>% filter(start.year == 1, ea == 20)





# #*************************************************************************************************************
# #                          AL for vested terminatede members                        #####                  
# #*************************************************************************************************************
# 
# Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(start.year = 2015, # (1 - (r.max - 1 - min.age)):nyear, 
                         ea = range_ea[range_ea < r.full], age = range_age, age.term = range_age[range_age < r.full]) %>% # start year no longer needs to start from -89 if we import initial benefit data.
  filter(start.year + max.age - ea >= 2015, 
         age >= ea, age.term >= ea,
         age >= age.term) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 


liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, pxRm, px_r.full_m) %>% data.table(key = "ea,age,start.year"),
                   all.x = TRUE, by = c("ea", "age","start.year")) %>% as.data.frame %>% 
             left_join(mortality.post.ucrp %>% filter(age.r == r.full) %>% select(age, ax.r.W.term = ax.r.W))   # load present value of annuity for retirement age r.full

liab.term %<>% as.data.frame %>%
  # arrange(start.year, ea, age.term, age) %>% # Very slow. Uncomment it only when we want to examine liab.term.
  group_by(start.year, ea, age.term) %>%
  mutate(year.term = year[age == age.term],

         B.v   = ifelse(age >= r.full, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.full], 0),  # Benefit payment after r.full
         ALx.v = ifelse(age <  r.full, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.full] * px_r.full_m * v^(r.full - age),
                        B.v * ax.r.W.term)

  ) %>%
  ungroup  %>%
  # select(#-start.year, -age.term,
  #        -Bx.v, -ax.r.W, -COLA.scale, -pxRm) %>%

  # select(-age.term, -Bx.v, -ax, -COLA.scale, -pxRm) %>%
#  filter(year %in% seq(init.year, len = nyear) ) %>% 
  arrange(age.term, ea, age)

# # liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])
# 


