# Benefit table by 3Darray
# Yimeng Yin  
# 1/1/2015

## Notes:
 # This program calculates accrued benefits, benefits accrual and various actuarial liability measures based on 
 # deterministic salary growth for each entry age.

 # We start out with the case where 
    # (1) the starting salary at each entry age increases at the rate of productivity growth plus inflation.
    # (2) The starting salary at each entry age are obtained by scaling up the the salary at entry age 20,
    #     hence the age-30 entrants at age 30 have the same salary as the age-20 entrants at age 30. 
 # The first step is to produce tables for a given year's starting salary. 


## preamble ####
# this uses memory() which is in btools
memory<-function(maxnobjs=5){
  # function for getting the sizes of objects in memory
  objs<-ls(envir=globalenv())
  nobjs<-min(length(objs),maxnobjs)
  tmp<-as.data.frame(sapply(objs, function(x) object.size(get(x)))/1048600)
  tmp<-data.frame(name=row.names(tmp), sizeMB=tmp[,1])
  tmp<-tmp[order(-tmp$sizeMB),]
  tmp$sizeMB<-formatC(tmp$sizeMB,format="f",digits=2,big.mark=",",preserve.width="common")
  print(paste("Memory available: ",memory.size(NA),sep=""))
  print(paste("Memory in use before: ",memory.size(),sep=""))
  print("Memory for selected objects: ")
  print(head(tmp,nobjs))
  print(gc())
  print(paste("Memory in use after: ",memory.size(),sep=""))
}

library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(corrplot)

wvd <- "C:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))


## Read decrement tables ###

# table 5-1 ptl and pcl liability measures for age-30 entrants ####
# key assumptions
benfactor <- 0.015  # benefit factor, 1.5% per year of yos
fasyears  <- 5      # number of years in the final average salary calculation
infl <- 0.04        # inflation
prod <- 0.01        # productivity
i <- 0.08           # interest rate
v <- 1/(1 + i)      # discount factor


# Construct a data frame contaning the following information:
# survival rates each year, survival probs up to retirment
# Annual salary, cumulative salary
# benefit accrual, accrued benefit
# Various annuity values. 


term2 <- data.frame(age = 20:110) %>% left_join(select(term, age, everything())) %>% gather(ea, qxt, -age) %>%
  mutate(ea = as.numeric(gsub("[^0-9]", "", ea)))
 filter(gam1971, age >= 20) %>% left_join(term2) %>% select(age, ea, everything()) %>% arrange(ea, age)


desc <- filter(gam1971, age>=20) %>% left_join(term2) %>% left_join(disb) %>% # survival rates
  left_join(merit) %>% # merit salary scale 
  filter(age >= ea) %>% 
  select(age, ea, everything()) %>% 
  arrange(ea, age) %>%
  group_by(ea) %>%
  # Calculate survival rates
  mutate( pxm = 1 - qxm,
          pxT = (1 - qxm) * (1 - qxt) * (1 - qxd),
          px65m = order_by(-age, cumprod(ifelse(age >= 65, 1, pxm))), # prob of surviving up to 65, mortality only
          px65T = order_by(-age, cumprod(ifelse(age >= 65, 1, pxT))), # prob of surviving up to 65, composite rate
          p65xm = cumprod(ifelse(age <= 65, 1, lag(pxm))),            # prob of surviving to x from 65, mortality only
          vrx = v^(65-age)) %>%
  # Calculate salary and benefits
  mutate(sx = scale * (1 + infl + prod)^(age - min(age)),   # Composite salary scale
         Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
         yos= age - min(age),                               # years of service
         n  = pmin(yos, fasyears),                          # years used to compute fas
         fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, 5))/n), # final average salary
         fas= ifelse(age == min(age), 0, fas),
         Bx = benfactor * yos * fas,                        # accrued benefits
         ax = ifelse(age < 65, NA, get_tla(pxm, i)), # Since retiree die at 110 for sure, the life annuity is equivalent to temporary annuity up to age 110. 
         ayx = c(get_tla2(pxT[age<=65], i), rep(0, 45)),             # need to make up the length of the vector to 81
         ayxs= c(get_tla2(pxT[age<=65], i, sx[age<=65]), rep(0, 45))  # need to make up the length of the vector to 81
  )                              




## A more complicated case: starting salary increase over time.

# A simple example. 
  # 2 entry ages: 20, 35;
  # 2 starting year:1, 2;
  # model up to age 30
  # salary growth: prod+inflation: 5%/year, prod+inflation+merit: 10%/year.
  
salary <- expand.grid(start.year = 1:2, ea = c(20, 25), age = 20:30) %>%
  arrange(start.year, ea, age) %>%
  filter(age >= ea) %>% 
  left_join(data.frame(age = 20:30, scale = cumprod(c(1, rep(1.1, 10)))))  %>% 
  left_join(data.frame(start.year = 1:2, growth = c(1, 1.05) )) %>%
  mutate(salary = scale*growth)
  

# get down to the real business!

growth <- data.frame(start.year = 1:144) %>%
  mutate(growth = (1 + infl + prod)^(start.year - 1 ))

salary <- expand.grid(start.year = 1:144, ea = seq(20, 60, 5), age = 20:110) %>% 
  filter(age >= ea) %>%
  arrange(start.year, ea, age) %>%
  left_join(merit) %>% left_join(growth) %>%
  group_by(start.year, ea) %>%
  mutate(year = start.year + age -ea,  # year index in the simulation
         sx = growth*scale*(1 + infl + prod)^(age - min(age)))


desc <- salary %>% left_join(filter(gam1971, age>=20)) %>% left_join(term2) %>% left_join(disb) %>% # survival rates
  select(start.year, year, ea, age, everything()) %>% 
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate survival rates
  mutate( pxm = 1 - qxm,
          pxT = (1 - qxm) * (1 - qxt) * (1 - qxd),
          px65m = order_by(-age, cumprod(ifelse(age >= 65, 1, pxm))), # prob of surviving up to 65, mortality only
          px65T = order_by(-age, cumprod(ifelse(age >= 65, 1, pxT))), # prob of surviving up to 65, composite rate
          p65xm = cumprod(ifelse(age <= 65, 1, lag(pxm))),            # prob of surviving to x from 65, mortality only
          vrx = v^(65-age)) %>%
  # Calculate salary and benefits
  mutate(# sx = scale * (1 + infl + prod)^(age - min(age)),   # Composite salary scale
         Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
         yos= age - min(age),                               # years of service
         n  = pmin(yos, fasyears),                          # years used to compute fas
         fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, 5))/n), # final average salary
         fas= ifelse(age == min(age), 0, fas),
         Bx = benfactor * yos * fas,                        # accrued benefits
         ax = ifelse(age < 65, NA, get_tla(pxm, i)),  # Since retiree die at 110 for sure, the life annuity is equivalent to temporary annuity up to age 110. 
         ayx = c(get_tla2(pxT[age <= 65], i), rep(0, 45)),                # need to make up the length of the vector up to age 110
         ayxs= c(get_tla2(pxT[age <= 65], i,  sx[age <= 65]), rep(0, 45))  # need to make up the length of the vector up to age 110
  ) 




























