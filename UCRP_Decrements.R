
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
load( "Data/UCRP.df.noAdj.RData")

source("Functions.R")


init.year <- 2015
max.age <-  120

range_ea <- c(20, 74)
range_age <- 20:120
range_age.r <- 55:75
r.max <- max(range_age.r)



fasyears <- 3
cola     <- 0.03
i <- 0.0725

r.full <- 60 # age at which vested terms are assumed to retire. 
r.yos  <- 5   
r.min  <- min(range_age.r)
r.max  <- max(range_age.r) 


pct.F.actives <- 0.55
pct.M.actives <- 1 - pct.F.actives

pct.F.LSC <- 0.6
pct.M.LSC <- 1 - pct.F.LSC

pct.fac.actives.t13 <- 0.5
pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 

pct.ca <- 0.8 * pct.F.actives + 0.6 * pct.M.actives
pct.la <- 1 - pct.ca



#*************************************************************************************************************
#                                Prepare mortality tables for UCRP                        #####                  
#*************************************************************************************************************

mortality.ucrp <- data.frame(age = range_age) %>% 
  left_join(mortality.post %>% filter(year == 2029) %>% select(age ,qxm.post.M, qxm.post.F)) %>% 
  left_join(mortality.pre  %>% filter(year == 2029) %>% select(age ,qxm.pre.M,  qxm.pre.F)) %>% 
  mutate(qxm.pre = qxm.pre.M  * pct.M.actives + qxm.pre.F  * pct.F.actives,  # mortality for actives
         qxm.LSC = lag(qxm.post.M * pct.M.LSC     + qxm.post.F * pct.F.LSC), # mortality used for LSC
         qxm.LSC = ifelse(age == 50, lead(qxm.LSC), 
                          ifelse(age == max(age), 1, qxm.LSC)),
         
         qxm.post.M = ifelse(age == min(range_age.r), qxm.post.M, 
                             ifelse(age == max(age), 1, lag(qxm.post.M))),
         
         qxm.post.F = ifelse(age == min(range_age.r), qxm.post.F, 
                             ifelse(age == max(age), 1, lag(qxm.post.F)))) %>% 
  select(age, qxm.pre, qxm.LSC, qxm.post.M, qxm.post.F)


# compute present values of life annuity(with cola) at each retirement age, using uni-sex mortality with age dependent weights
ax.r.W.ucrp <- expand.grid(age = range_age, age.r = range_age.r) %>% 
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
  filter(age == age.r) %>% 
  select(age, ax.r.W)



retrates.ucrp  <- retrates %>% mutate(qxr.t13 = qxr.t13.fac * pct.fac.actives.t13 + qxr.t13.stf * pct.stf.actives.t13) %>% 
                               select(age, qxr.t13) 
                                

disbrates.ucrp <- disbrates %>%  mutate(qxd = qxd.M * pct.M.actives + qxd.F * pct.F.actives)

termrates.ucrp <- termrates %>% mutate(qxt = qxt_faculty)

LSCrates




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
  left_join(retrates.ucrp) %>%                # early retirement
  left_join(LSCrates) %>%                     # LSC rate
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(ea) 

# decrement$qxr <- na2zero(decrement$qxr)



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
  qxr.t13 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
               ifelse(yos < r.yos, 0, 
                      ifelse(age %in% r.min:(r.max - 1), qxr.t13, 0)
               )
  )
) 





# Adjustment to the decrement table:
# Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
# For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
# which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
# whether they will die at r.max)      

decrement.ucrp %<>% group_by(ea) %>%  
  mutate(qxr = ifelse(age == r.max - 1,
                            1 - qxt - qxm.pre - qxd, 
                            lead(qxr.t13)*(1 - qxt - qxm.pre - qxd)),
         qxLSC       = ifelse(age == r.max, 0 , qxr * lead(qxLSC.act)),
         qxr.la      = ifelse(age == r.max, 0 , qxr * lead(1 - qxLSC.act) * pct.la),
         qxr.ca      = ifelse(age == r.max, 0 , qxr * lead(1 - qxLSC.act) * pct.ca))


## define decrements for status and calculte survival probabilities. 
decrement.ucrp %<>% 
  # For active(".a"). 
  mutate(qxt.a   = ifelse(age >= r.max, 0, qxt),       # qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2),
         qxd.a   = ifelse(age >= r.max, 0, qxd),       # (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
         qxm.a   = ifelse(age >= r.max, 0, qxm.pre),   # (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
         qxr.a   = qxr,                                 # ifelse(age == 64, (1 - qxt.p)*(1 - qxd.p)*(1 - qxm.p), 0)
         qxLSC.a = qxLSC,
         qxr.la.a = qxr.la,
         qxr.ca.a = qxr.ca
         
  ) %>%
  
  # For terminated(".t"), target status are dead and retired.
  # Terminated workers will never enter the status of "retired". Rather, they will begin to receive pension benefits 
  # when reaching age r.max, but still with the status "terminated". So now we do not need qxr.t
  mutate(qxm.t   = qxm.pre) #%>%
  
  # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
  # Note the difference from the flows 3Darray.R. Disabled can not become retired here. 
  # mutate(qxm.d = qxmd ) %>%
  

######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 



# Calculate various survival probabilities
decrement.ucrp %<>% 
  mutate( pxm.pre = 1 - qxm.pre,
          # pxm.r = 1 - qxm.r,
          pxT = 1 - qxt - qxd - qxm.pre - qxr, #(1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          pxRm = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.full, 1, pxm.pre)))
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  )

