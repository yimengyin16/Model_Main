
# # rm(list = ls())
# # gc()
# 
# library(knitr)
# library(data.table)
# library(gdata) # read.xls
# library(plyr)
# library(dplyr)
# library(ggplot2)
# library(magrittr)
# library(tidyr) # gather, spread
# library(foreach)
# library(doParallel)
# library(microbenchmark)
# library(readxl)
# library(stringr)
# 
# 
# load("Data/UCRP.inputs1.RData")
# load("Data/UCRP.inputs2.RData")
# 
# source("Functions.R")


# init.year <- 2015
# max.age <-  120
# 
# range_ea <- c(20:74)
# range_age <- 20:120
# range_age.r <- 50:75
# 


# 
# fasyears <- 3
# cola     <- 0.03
# i <- 0.0725
# 
# r.full <- 60 # age at which vested terms are assumed to retire. 
# r.yos  <- 5   
# r.min  <- min(range_age.r)
# r.max  <- max(range_age.r) 
# 
# 
# pct.F.actives <- 0.55
# pct.M.actives <- 1 - pct.F.actives
# 
# pct.F.LSC <- 0.6
# pct.M.LSC <- 1 - pct.F.LSC
# 
# pct.fac.actives.t76 <- 0.5
# pct.stf.actives.t76 <- 1 - pct.fac.actives.t76 
# pct.fac.actives.t13 <- 0.5
# pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 
# pct.fac.actives.tm13 <- 0.5
# pct.stf.actives.tm13 <- 1 - pct.fac.actives.t13 
# 
# 
# 
# pct.ca <- 0.8 * pct.F.actives + 0.6 * pct.M.actives
# pct.la <- 1 - pct.ca



#*************************************************************************************************************
#                                Prepare mortality tables for UCRP                        #####                  
#*************************************************************************************************************

mortality.ucrp <- data.frame(age = range_age) %>% 
  left_join(mortality.post %>% filter(year == 2029) %>% select(age ,qxm.post.M, qxm.post.F)) %>% 
  left_join(mortality.pre  %>% filter(year == 2029) %>% select(age ,qxm.pre.M,  qxm.pre.F)) %>% 
  mutate(qxm.pre = qxm.pre.M  * pct.M.actives + qxm.pre.F  * pct.F.actives,   # mortality for actives
         qxm.LSC = lag(qxm.post.M * pct.M.LSC + qxm.post.F * pct.F.LSC),      # mortality used for LSC, age is moved one year ahead according to AV.
         qxm.LSC = ifelse(age == 50, lead(qxm.LSC),                           # Note the hard-coded "50", it is the starting post-retirement mortality in RP2014
                          ifelse(age == max(age), 1, qxm.LSC)),
         
         qxm.post.M = ifelse(age == 50, qxm.post.M, 
                             ifelse(age == max(age), 1, lag(qxm.post.M))),
         
         qxm.post.F = ifelse(age == 50, qxm.post.F, 
                             ifelse(age == max(age), 1, lag(qxm.post.F)))) %>% 
  select(age, qxm.pre, qxm.LSC, qxm.post.M, qxm.post.F)


# compute present values of life annuity(with cola) at each retirement age, using uni-sex mortality with age dependent weights
mortality.post.ucrp <- expand.grid(age = range_age, age.r = range_age.r) %>% 
  left_join(mortality.ucrp) %>%
  filter(age >= age.r) %>% 
  group_by(age.r) %>%  
  mutate(
    pxm.post.M = 1 - qxm.post.M,
    pxm.post.F = 1 - qxm.post.F,
    
    pRxm.M     = pct.M.actives * ifelse(age == min(age), 1, lag(cumprod(pxm.post.M))),
    pRxm.F     = pct.F.actives * ifelse(age == min(age), 1, lag(cumprod(pxm.post.F))),
    
    w.M = pRxm.M / (pRxm.M + pRxm.F),
    w.F = pRxm.F / (pRxm.M + pRxm.F),
    
    qxm.post.W = qxm.post.M * w.M + qxm.post.F * w.F, # dynamically weighted mortality
    pxm.post.W = 1 - qxm.post.W,
    
    COLA.scale = (1 + cola)^(row_number() - 1 ),
    B =  COLA.scale,
    ax.r.W     =  get_tla(pxm.post.W, i, COLA.scale),
    liab.la.W = B * ax.r.W    # "la" for life annuity
  )  %>% 
  select(age, qxm.post.W, pxm.post.W, ax.r.W)

# mortality.post.ucrp %>% filter(age == age.r) %>% select(age, ax.r.W)

                              
disbrates.ucrp <- disbrates %>%  mutate(qxd = qxd.M * pct.M.actives + qxd.F * pct.F.actives)

termrates.ucrp <- termrates %>% mutate(qxt = qxt_faculty)



retrates.ucrp  <- retrates %>% mutate(qxr.t76  = qxr.t76.fac * pct.fac.actives.t76 + qxr.t76.stf * pct.stf.actives.t76,
                                      qxr.t13  = qxr.t13.fac * pct.fac.actives.t13 + qxr.t13.stf * pct.stf.actives.t13,
                                      qxr.tm13 = qxr.t13.fac * pct.fac.actives.tm13 + qxr.tm13.stf * pct.stf.actives.tm13) %>% # assume Tier 2013 and Tier modified 2013 have the same faculty retirement rates.
             
                  select(age, qxr.t76, qxr.t13, qxr.tm13) 






#*************************************************************************************************************
#                      2. Putting together decrements and calculate surviving rates  ####
#*************************************************************************************************************

# Create decrement table and calculate probability of survival
decrement.ucrp <- expand.grid(age = range_age, ea = range_ea) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  left_join(mortality.ucrp) %>%                  # mortality 
  left_join(termrates.ucrp)  %>%                 # termination
  left_join(disbrates.ucrp)  %>%                 # disability
# left_join(dbl)   %>%                           # mortality for disabled
  left_join(retrates.ucrp) %>%                   # early retirement
  left_join(LSCrates) %>%                        # LSC rate
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(ea) 



## Imposing restrictions 
decrement.ucrp %<>% mutate(
  # 1. Coerce termination rates to 0 when eligible for early retirement or reaching than r.full(when we assume terms start to receive benefits). 
  qxt = ifelse((age >= r.min & (age - ea) >= r.yos) | age >= r.full, 0, qxt),
  #qxt = ifelse(age >= r.min | age >= r.full, 0, qxt),
  
  # qxt = ifelse( age >= r.full, 0, qxt),
  # 2. Coerce retirement rates to 0 when age greater than r.max                     
  #   qxr = ifelse(age == r.max, 1, 
  #                ifelse(age %in% r.min:(r.max - 1), qxr, 0))
  #   
  qxr.t76 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
               ifelse(yos < r.yos, 0, 
                      ifelse(age %in% 50:(r.max - 1), qxr.t76, 0)
                      )
                   ),
  
  qxr.t13 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
                   ifelse(yos < r.yos, 0, 
                          ifelse(age %in% 55:(r.max - 1), qxr.t13, 0)
                   )
  ),

  qxr.tm13 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
                   ifelse(yos < r.yos, 0, 
                          ifelse(age %in% 50:(r.max - 1), qxr.tm13, 0)
                   )
  ) 
  ) 





# Adjustment to the decrement table:
# Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
# For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
# which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
# whether they will die at r.max)      

decrement.ucrp %<>% group_by(ea) %>%  
  mutate(
         # 1976 Tier: LSC, life annuity and contingent annuity
         qxr.t76 = ifelse(age == r.max - 1,
                            1 - qxt - qxm.pre - qxd, 
                            lead(qxr.t76)*(1 - qxt - qxm.pre - qxd)),                         # Total probability of retirement
         qxr.LSC.t76     = ifelse(age == r.max, 0 , qxr.t76 * lead(qxLSC.act)),               # Prob of opting for LSC
         qxr.la.t76      = ifelse(age == r.max, 0 , qxr.t76 * lead(1 - qxLSC.act) * pct.la),  # Prob of opting for life annuity
         qxr.ca.t76      = ifelse(age == r.max, 0 , qxr.t76 * lead(1 - qxLSC.act) * pct.ca),  # Prob of opting for contingent annuity

         # 2013 Tier: life annuity only  
         qxr.t13 = ifelse(age == r.max - 1,
                            1 - qxt - qxm.pre - qxd, 
                            lead(qxr.t13)*(1 - qxt - qxm.pre - qxd)),                         # Total probability of retirement
         qxr.LSC.t13     = 0,                                                                 # Prob of opting for LSC
         qxr.la.t13      = qxr.t13,                                                           # Prob of opting for life annuity
         qxr.ca.t13      = 0,                                                                 # Prob of opting for contingent annuity

         # modified 2013 Tier: LSC and life annuity.   
         qxr.tm13        = ifelse(age == r.max - 1,
                            1 - qxt - qxm.pre - qxd, 
                            lead(qxr.tm13)*(1 - qxt - qxm.pre - qxd)),                        # Total probability of retirement
         qxr.LSC.tm13       = ifelse(age == r.max, 0 , qxr.tm13 * lead(qxLSC.act)),             # Prob of opting for LSC                                                        # Prob of opting for LSC
         qxr.la.tm13      = ifelse(age == r.max, 0 , qxr.tm13 * lead(1 - qxLSC.act)),         # Prob of opting for life annuity                                                 # Prob of opting for life annuity
         qxr.ca.tm13      = 0)                                                                # Prob of opting for contingent annuity
         





# ## define decrements for status and calculte survival probabilities. 
# decrement.ucrp %<>% 
#   # For active(".a"). 
#   mutate(qxt.a   = ifelse(age >= r.max, 0, qxt),       # qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2),
#          qxd.a   = ifelse(age >= r.max, 0, qxd),       # (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
#          qxm.a   = ifelse(age >= r.max, 0, qxm.pre),   # (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
#          
#          qxr.t76.a      = qxr.t76,                             
#          qxr.LSC.t76.a  = qxr.LSC.t76,
#          qxr.la.t76.a   = qxr.la.t76,
#          qxr.ca.t76.a   = qxr.ca.t76,
#          
#          qxr.t13.a      = qxr.t13,                              
#          qxr.LSC.t13.a  = qxr.LSC.t13,
#          qxr.la.t13.a   = qxr.la.t13,
#          qxr.ca.t13.a   = qxr.ca.t13,
#          
#          qxr.tm13.a      = qxr.tm13,                                
#          qxr.LSC.tm13.a  = qxr.LSC.tm13,
#          qxr.la.tm13.a   = qxr.la.tm13,
#          qxr.ca.tm13.a   = qxr.ca.tm13
#   ) %>%
#   
#   
#   
#   # For terminated(".t"), target status are dead and retired.
#   # Terminated workers will never enter the status of "retired". Rather, they will begin to receive pension benefits 
#   # when reaching age r.max, but still with the status "terminated". So now we do not need qxr.t
#   mutate(qxm.t   = qxm.pre) #%>%
#   
#   # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
#   # Note the difference from the flows 3Darray.R. Disabled can not become retired here. 
#   # mutate(qxm.d = qxmd ) %>%
  

######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 



# Calculate various survival probabilities
decrement.ucrp %<>% 
  mutate( pxm.pre = 1 - qxm.pre,
          # pxm.r = 1 - qxm.r,
          
          pxT.t76     = 1 - qxt - qxd - qxm.pre - qxr.t76,                              # (1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          pxT.t13     = 1 - qxt - qxd - qxm.pre - qxr.t13,  
          pxT.tm13    = 1 - qxt - qxd - qxm.pre - qxr.tm13,  
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.full, 1, pxm.pre)))
          
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  )








# Final outputs

#1. decrement.ucrp
#2. mortality.post.ucrp














