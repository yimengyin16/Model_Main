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
load( "Data/UCRP.df.noAdj.RData")

source("Functions.R")


init.year <- 2015
max.age <-  120

range_ea <- c(30, 74)
range_age <- 20:120
range_age.r <- 50:75
r.max <- max(range_age.r)



fasyears <- 3
cola     <- 0.03
i <- 0.0725

r.full <- 60 # age at which vested terms are assumed to retire. 

pct.F.actives <- 0.55
pct.M.actives <- 1 - pct.F.actives

pct.F.LSC <- 0.6
pct.M.LSC <- 1 - pct.F.LSC

pct.fac.actives.t13 <- 0.5
pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 




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



retrates.ucrp <- retrates %>% mutate(qxr.t13 = qxr.t13.fac * pct.fac.actives.t13 + qxr.t13.stf * pct.stf.actives.t13)
                                     




#*************************************************************************************************************
#                                Preparation                        #####                  
#*************************************************************************************************************
# Starts with a simple case with only 2 entry ages. 

liab.active <- expand.grid(start.year = 2015, 
                           ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>%
  mutate(sx = 1.03^(age - ea)) %>% # left_join(.salary)  %>% 
#   left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
#   right_join(.decrement) %>%
  left_join(mortality.ucrp) %>% 
  left_join(retrates.ucrp) %>% 
  left_join(bfactor) %>%
  left_join(ax.r.W.ucrp) %>% 
  group_by(start.year, ea) %>%

  # Calculate salary and benefits
  mutate(
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = pmin(fas, na2zero(bf.non13 * yos * fas)),      # accrued benefits
    bx = lead(Bx) - Bx                                # benefit accrual at age x
)
    
    # ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    # ax.r = get_tla(pxm.r, i, COLA.scale),              # ax calculated with mortality table for retirees. 
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i), rep(0, max.age - r.full + 1))),                 # Similar to axR, but based on r.full.  For calculation of term benefits when costs are spread up to r.min.        
    axrs= ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i, sx[age<r.full]), rep(0, max.age - r.full + 1))),  # Similar to axRs, but based on r.full. For calculation of term benefits when costs are spread up to r.min.
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )














