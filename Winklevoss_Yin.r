# Yimeng Yin
# 12/13/2013

# The code in script replicate works through Winklevoss pension book following the code written by Don.(Winklevoss(7).r)
# for practice purpose. 
# Don's comments are retained as much as possible



## Preamble ####

# Defining data directory and data file
wvd <- "C:/Dropbox (Personal)/Proj-PenSim/Winklevoss/"
wvxl <- "Winklevoss(6).xlsx"
load(paste0(wvd, "winklevossdata.rdata"))

library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(tidyr) # gather, spread
library(xlsx)
library("magrittr")

source("Functions.R")

# functions created by Don
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA
ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail


#****************************************************************************************************
#
#                Read and save model-plan tables - RUN ONCE ####
#
#****************************************************************************************************

# Data table 2-1 mortality ####
# Mortality table GAM-1971 (http://mort.soa.org/Export.aspx?Type=xls&TableIdentity=818) is used. 
fn <- "GAM-1971-Male.xls"
gam1971 <- read.xls(paste0(wvd, fn), colClasses = "character")
names(gam1971) <- c("age", "qxm")
gam1971 <- gam1971 %>%  mutate_each(funs(cton)) %>%
           filter(age %in% 5:110)


# data table 2-3 termination rates ####
term <- read.xls(paste0(wvd, wvxl), sheet = "Tab2-3TermRates", colClasses = "character")
names(term) <- c("age", paste0("ea", seq(20, 60, 5)))
term <- term %>% mutate_each(funs(cton)) %>%
        filter(!is.na(age))

# data table 2-5 disability life rates ####
dbl <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-5DisbLife", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(dbl) <- c("age", "qxmd")
dbl <- dbl %>%
  mutate_each(funs(cton))


# data table 2-7 disability ####
disb <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-7Disb", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(disb) <- c("age", "qxd")
disb <- disb %>%
  mutate_each(funs(cton))


# data table 2-9 early retirement ####
er <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-9EarlyRet", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(er) <- c("age", "qxe")
er <- er %>%
  mutate_each(funs(cton))


# data table 2-10 salary merit table ####
merit <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-10Merit", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(merit) <- c("age", "scale")
merit <- merit %>%
  mutate_each(funs(cton))


# data table 4-6 hiring distribution table ####
hire <- read.xls(paste0(wvd, wvxl), sheet= "Tab4-6HireDist", colClasses = "character")
hire <- hire[-1, ]
names(hire) <- c("eage", "dist", "salscale")
hire <- hire %>%
  mutate_each(funs(cton))


save(gam1971, term, dbl, disb, er, merit, hire, file = paste0(wvd, "winklevossdata.rdata"))

## Note the difference between read.xls(gdata) and read.xlsx(xlsx)



#****************************************************************************************************
#
#                Construct various tables in the book ####
#
#****************************************************************************************************



# table 2-2 mortality - survival probabilities - p16 ####

# Implementation 1 (using arrange(desc(age)))
tab2_2 <- gam1971 %>% arrange(desc(age)) %>% 
          mutate(pxm = 1 - qxm,
                 px65m = ifelse(age >= 65, 1, pxm) %>% cumprod) %>% # survival prob from age x to 65
          arrange(age) %>%
          mutate(p65xm = ifelse(age <= 65, 1, lag(pxm)) %>% cumprod) %>% # survival prob from 65 to x. lag in dplyr is used
          filter(age %in% seq(20, 110, 5))
kable(tab2_2, digit = 4)

# Implementation 2 (using order_by() in dplyr)
tab2_2 <- gam1971 %>% 
          mutate(pxm = 1 - qxm,
                 px65m = order_by(-age, cumprod(ifelse(age >= 65, 1, pxm))), # survival prob from age x to 65
                 p65xm = ifelse(age <= 65, 1, lag(pxm)) %>% cumprod) %>% # survival prob from 65 to x. lag in dplyr is used
                 filter(age %in% seq(20, 110, 5))
kable(tab2_2, digit = 4)

# Notes:
# 1. order_by(): define the path through which the function cumprod is applied
# 2. The survival at age x is evaluated at the end of the year(or equivalently, the beginning of age x + 1)
# 3. Note that when x = 66, we have n = 1, hence we are actually calculating the prob of living through
# the age 65. 
# 4. So when we say the participant is at age x, we are actually saying she is at the begining of age x.


# table 2-4 - retention from ay until +5 or 65 ####
# implementation 1, only to reproduce the table
tab2_4 <- gather(term, entry, qxt, -age) %>%
          filter(!is.na(qxt)) %>%
          mutate(pxt = 1 - qxt,
                 entry = gsub("[^0-9]", "", entry) %>% as.numeric) %>% # deleting any character except numbers in "entry"
          group_by(entry) %>% 
          mutate(py5t  = order_by(-age, cumprod(ifelse(age >= unique(entry) + 5, 1, pxt))),
                 py65t = order_by(-age, cumprod(pxt))) %>%
          filter(age == entry)
kable(tab2_4, digit = 4)

# implementation 2, Don's code.
term %>% gather(entry, qxt, -age) %>%
  filter(!is.na(qxt)) %>%
  mutate(entry=as.numeric(gsub("[^0-9]", "", entry)),
         pxt=1-qxt) %>%
  group_by(entry) %>%
  mutate(npxt=order_by(age, cumprod(pxt))) %>% # cumulative survival rates within each entry age
  filter(age %in% c(entry+4, 64)) %>% # entry+4 reflects 5 years of survival -- entry year, plus each of next 4
  mutate(colname=ifelse(age==64, "65-yPty", "5Pty")) %>%  # to approx match column names in the book
  select(entry, colname, npxt) %>%
  ungroup() %>% # must ungroup before spread -- a bug in dplyr
  spread(colname, npxt) %>%
  mutate(`5Pty`=ifelse(entry==60, `65-yPty`, `5Pty`)) %>% # we only have 1 record for entry 60, since +4 and 64 are the same
  kable(digits=4)  

# implementation 3, practice for using rollapply. do it later. 


# table 2-6 disabled-life survival probabilities ####
tab2_6 <- mutate(dbl, pxmd = 1 - qxmd,
                 px65md = order_by(-age, cumprod(ifelse(age >= 65, 1, pxmd))),
                 p65xmd = cumprod(ifelse(age <= 65, 1, lag(pxmd)))) %>%
          filter(age %in% seq(20, 110, 5))
kable(tab2_6, digits = 4)  


# table 2-8 disability-based survival in service ####
tab2_8 <- disb %>% rbind(data.frame(age = 65, qxd = NA)) %>%
          mutate(pxd = 1 - qxd,
                 px65d = order_by(-age, cumprod(ifelse(age >= 65, 1, pxd)))) %>% # order_by and ifelse are unnecessary
          filter(age %in% seq(20, 65, 5))
kable(tab2_8, digits = 4)  


# table 2-11 merit pay #### 
infl <- 0.04 # inflation, used in book
prod <- 0.01 # productivity, used in book
 
tab2_11 <- merit %>% 
  mutate(iscale = (1 + infl)^(age - min(age)),
         pscale = (1 + prod)^(age - min(age)),
         totscale1 = scale * iscale * pscale,
         totscale2 = scale * (1 + infl + prod)^(age - min(age)),
         mtp64_1 = totscale1[age==64] / totscale1,
         mtp64_2 = totscale2[age==64] / totscale2,
         compound_1 = mtp64_1^(1/(64 - age)) - 1, 
         compound_2 = mtp64_2^(1/(64 - age)) - 1) %>%
  filter(age %in% seq(20, 60, 5))
kable(tab2_11, digits = 2)

# Dons' implementation is similar. He uses row_number() - 1, instead of age - min(age)


# table 3-1, multiple decrements: mortality, termination, disability #### 
# but not early retirment (I have not included er, anyway)
# note that my gam1971 has one more decimal place than Winklevoss

tab3_1 <- gam1971 %>% left_join(term) %>% left_join(disb) %>% # merging 3 decrement rates
  filter(age %in% 20:64) %>%
  gather(entry, qxt, -age, -qxm, -qxd) %>%
  mutate(entry = as.numeric(gsub("[^0-9]", "", entry)),
         pxT = (1 - qxm)*(1 - qxt)*(1 - qxd)*(1-qxe)) %>%
  group_by(entry) %>%
  mutate(px65T = order_by(-age, cumprod(pxT))) %>%
  ungroup() %>%
  select(age, entry, px65T) %>%
  spread(entry, px65T)
kable(tab3_1, digits = 2)
# see Don's comments on the difference between the results and the book. 

# with early retirment
tab3_1 <- gam1971 %>% left_join(term) %>% left_join(disb) %>% left_join(er) %>% # merging 3 decrement rates
  filter(age %in% 20:64) %>%
  gather(entry, qxt, -age, -qxm, -qxd, -qxe) %>%
  mutate(entry = as.numeric(gsub("[^0-9]", "", entry)),
         qxe = ifelse(is.na(qxe), 0, qxe),
         pxT = (1 - qxm)*(1 - qxt)*(1 - qxd)*(1 - qxe)) %>%
  group_by(entry) %>%
  mutate(px65T = order_by(-age, cumprod(pxT))) %>%
  ungroup() %>%
  select(age, entry, px65T) %>%
  spread(entry, px65T)
kable(tab3_1, digits = 2)


# table 3-2 (entry 20, only) - examine multiple decrements ####

tab3_2 <- gam1971 %>% left_join(select(term, age, qxt = ea20)) %>% left_join(disb) %>% 
  filter(age %in% 20:64) %>% 
  rbind_list(data.frame(age = 65, qxr = 1, qxt = 0, qxd = 0, qxm = 0)) %>%
  mutate(qxr = ifelse(age == 65, qxr, 0)) %>%
  # approximate the multiple devrement probs
  mutate(pqxm = qxm * (1 - 0.5*qxt) * (1 - 0.5*qxd) * (1 - 0.5*qxr),
         pqxt = qxt * (1 - 0.5*qxm) * (1 - 0.5*qxd) * (1 - 0.5*qxr),
         pqxd = qxd * (1 - 0.5*qxt) * (1 - 0.5*qxm) * (1 - 0.5*qxr),
         pqxr = qxr * (1 - 0.5*qxt) * (1 - 0.5*qxd) * (1 - 0.5*qxm),
         pqxT = pqxm + pqxt + pqxd + pqxr,
         ppxT = 1 - pqxT,
         # ppxT = (1 - qxm)*(1 - qxt)*(1-qxd)*(1-qxr) # alternative way to get survival prob
         cumppxT = cumprod(ppxT)) %>%
  mutate(lxT = ifelse(age == min(age), 1e6, 1e6*lag(cumppxT)),
         dxm = lxT * pqxm,
         dxt = lxT * pqxt,
         dxd = lxT * pqxd,
         dxr = lxT * pqxr,
         dxT = lxT * pqxT) %>%
  select(age, lxT, dxm, dxt, dxd, dxr, dxT)
kable(tab3_2, digits = 0)
  
# Alternative implementation using loop to update total population lxT
tab3_2 <- gam1971 %>% left_join(select(term, age, qxt = ea20)) %>% left_join(disb) %>% 
  filter(age %in% 20:64) %>% 
  rbind_list(data.frame(age = 65, qxr = 1, qxt = 0, qxd = 0, qxm = 0)) %>%
  mutate(qxr = ifelse(age == 65, qxr, 0)) %>%
  mutate(pqxm = qxm * (1 - 0.5*qxt) * (1 - 0.5*qxd) * (1 - 0.5*qxr),
         pqxt = qxt * (1 - 0.5*qxm) * (1 - 0.5*qxd) * (1 - 0.5*qxr),
         pqxd = qxd * (1 - 0.5*qxt) * (1 - 0.5*qxm) * (1 - 0.5*qxr),
         pqxr = qxr * (1 - 0.5*qxt) * (1 - 0.5*qxd) * (1 - 0.5*qxm))
         
getdec = function(df){
  for(i in 1:nrow(df)){
    df$lxT[i] = ifelse(i == 1, 1e6, df$lxe[i - 1]) # opening population
    df$dxm[i] = df$pqxm[i] * df$lxT[i]
    df$dxt[i] = df$pqxt[i] * df$lxT[i]
    df$dxd[i] = df$pqxd[i] * df$lxT[i]
    df$dxr[i] = df$pqxr[i] * df$lxT[i]
    df$dxT[i] = df$dxm[i] + df$dxt[i] + df$dxd[i] + df$dxr[i]
    df$lxe[i] = df$lxT[i] - df$dxT[i]              # ending population
  }
  df = select(df, age, lxT, dxm, dxt, dxd, dxr, dxT)
  return(df)
}

getdec(tab3_2) %>% kable(digits = 0)


# table 3-3 compond interest ####

f <- function(i, t) return((1/(1 + i))^t)
t = seq(5, 70, 5)
tab3_3 <- data.frame(t, i6 = f(0.06, t), i8 = f(0.08, t), i10 = f(0.10, t))
kable(tab3_3, d = 4)


# table 3-4 salary factors for different entry ages ####

infl <- 0.04 # inflation
prod <- 0.01 # productivity

tab3_4 <- merit %>%
  mutate(iscale = (1 + infl)^(age - min(age)),
         pscale = (1 + prod)^(age - min(age)),
         scale_tot1 = scale * iscale * pscale,                    # showed in the equations
         scale_tot2 = scale * (1 + infl + prod)^(age - min(age))) %>% # used in the table
  mutate(scale_ea20 = scale_tot2,
         scale_ea30 = ifelse(age < 30, NA, scale_tot2/scale_tot2[age == 30]),
         scale_ea40 = ifelse(age < 40, NA, scale_tot2/scale_tot2[age == 40]),
         scale_ea50 = ifelse(age < 50, NA, scale_tot2/scale_tot2[age == 50]),
         scale_ea60 = ifelse(age < 60, NA, scale_tot2/scale_tot2[age == 60])) %>%
  #select(age, num_range("scale_ea", seq(30, 60, 10)))
  #select(age, one_of(paste0("scale_ea", seq(20, 60, 10))))
  select(age, scale_ea20:scale_ea60)


# table 3-5 benefit accrual for age-30 entrant ####

benfactor = 0.015 # 1.5 percent of final average salary per yos
fasyears = 5     # 5 years of final average salary. 

infl = 0.04
prod = 0.01


# Implementation 1: calculating 5 year average using Sx - S(x-5) as the book did.
tab3_5 <- merit %>% rbind(data.frame(age = 65, scale = NA)) %>%
          filter(age >= 30) %>%
  mutate(sx = scale * (1 + infl + prod)^(age - min(age)), 
         Sx = ifelse(age == min(age), 0, lag(cumsum(sx))), # cumulative salary
         yos = age - min(age),         # years of service
         n = pmin(fasyears, yos),      # number of years for calculate final ave 
         S_ave = ifelse(yos <=5, Sx - Sx[age==min(age)], Sx - lag(Sx, fasyears)) / n) %>%
  mutate(Bx = ifelse(yos == 0, 0, benfactor * S_ave * yos),
         bx = lead(Bx) - Bx,
         Bx.p = Bx / Bx[age == 65],
         bx.p = bx / Bx[age == 65],
         
         BxCD = Bx[age == 65] * yos / (65 - 30),
         bxCD =  Bx[age == 65] / (65 - 30),
         BxCD.p = BxCD / Bx[age == 65],
         bxCD.p = bxCD / Bx[age == 65],
         
         BxCP =  Bx[age == 65] * Sx / Sx[age == 65],
         bxCP =  Bx[age == 65] * sx / Sx[age == 65],
         BxCP.p = BxCP / Bx[age == 65],
         bxCP.p = bxCP / Bx[age == 65]) %>%
  select(age, bx.p, Bx.p, bxCD.p, BxCD.p, bxCP.p, BxCP.p)
kable(tab3_5, digits = 4)


# Implementation 2: Calculating 5 year average using a MA function defined by rollapply(in zoo)
f = function(x, years) rollapply(x, width  = years, FUN = mean, partial = TRUE, align = "right") # will be applied to sx
f(1:10)

tab3_5 <- merit %>% rbind(data.frame(age = 65, scale = NA)) %>%
  filter(age >= 30) %>%
  mutate(sx = scale * (1 + infl + prod)^(age - min(age)), 
         Sx = ifelse(age == min(age), 0, lag(cumsum(sx))), # cumulative salary
         yos = age - min(age),         # years of service
         n = pmin(fasyears, yos),      # number of years for calculate final ave 
         S_ave = ifelse(yos <=5, Sx - Sx[age==min(age)], Sx - lag(Sx, fasyears)) / n, # kept for comparison
         S_ave2= ifelse(age == min(age), 0, lag(f(sx, fasyears)))) %>%
  mutate(Bx = ifelse(yos == 0, 0, benfactor * S_ave2 * yos),
         bx = lead(Bx) - Bx,
         Bx.p = Bx / Bx[age == 65],
         bx.p = bx / Bx[age == 65],
         
         BxCD = Bx[age == 65] * yos / (65 - 30),
         bxCD =  Bx[age == 65] / (65 - 30),
         BxCD.p = BxCD / Bx[age == 65],
         bxCD.p = bxCD / Bx[age == 65],
         
         BxCP =  Bx[age == 65] * Sx / Sx[age == 65],
         bxCP =  Bx[age == 65] * sx / Sx[age == 65],
         BxCP.p = BxCP / Bx[age == 65],
         bxCP.p = bxCP / Bx[age == 65])
kable(tab3_5, digits = 4)

# table 3-7 temporary life annuities #### 

# Definitions
 # In a temporary life annuity, each payment is made only if a designated person is then alive, 
   # but the payments are limited to a fixed number of years.
 # In a whole life annuity, the payments continued for the entire lifetime of a designated person. 
 # annuity-due is paid at the start of the year; annuity-immediate is paid at the end of the year
   # (or the start of the next)
 # http://actuarialsciencestudies.blogspot.com/2012/04/temporary-life-annuities.html

get_tla <- function(px, i, sx = rep(1, length(px))){
  # inputs:
    # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
    # i:  discount rate
    # sx: salary scale. default is a n vector of 1, meaning no salary scale. 
  # output:
    # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr = 1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    # pxr <- cumprod(c(1, px[j:(n - 1)]))
    SS  <- sx[j:n]/sx[j] 
    tla[j] = sum(SS * v * pxr)          # computing annuity value at j
    } 
  return(tla)
}
 
get_tla(rep(0.98, 65), 0.08)

int = 0.08

tab3_7 <- gam1971 %>% left_join(term) %>% left_join(disb) %>%
  gather(entry, qxt, -age, -qxm, -qxd) %>%
  filter(age %in% 20:64, !is.na(qxt)) %>%
  mutate(entry = as.numeric(gsub("[^0-9]", "", entry)),
         pxT = (1 - qxm) * (1 - qxt) * (1 - qxd)) %>%
  group_by(entry) %>%
  mutate(ax65 = get_tla(pxT, int)) %>%
  filter(entry %in% seq(20, 60, 10)) %>%
  ungroup %>%
  select(age, entry, ax65) %>%
  spread(entry, ax65)
kable(tab3_7, digits = 2)


# table 3-8

infl = 0.04
prod = 0.01

tab3_8 <- gam1971 %>% left_join(term) %>% left_join(disb) %>% left_join(merit) %>%
  gather(entry, qxt, -age, -qxm, -qxd, -scale) %>%
  filter(age %in% 20:64, !is.na(qxt)) %>%
  mutate(entry = as.numeric(gsub("[^0-9]", "", entry)),
         pxT = (1 - qxm) * (1 - qxt) * (1 - qxd),
         scaletot = scale * (1 + infl + prod)^(age - min(age))) %>%
  group_by(entry) %>%
  mutate(ax65s = get_tla(pxT, int, scaletot)) %>%
  filter(entry %in% seq(20, 60, 10)) %>%
  ungroup 

%>%
  select(age, entry, ax65s) %>%
  spread(entry, ax65s)
kable(tab3_8, digits = 2)


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

# We also need functions to compute two types of annuity values
# 1. function calculating temporary annuity values from age x to retirment age 65
get_tla <- function(px, i, sx = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{y, x - y}, x ruuning from a1 to aN. 
  # The length of px is f - a1 
  # Note that the last element is redundant, just used as a place holder. 
  
  # inputs:
   # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
   # i:  discount rate, scalar
   # sx: salary scale. default is a n vector of 1, meaning no salary scale. 
  # output:
   # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr = 1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    # pxr <- cumprod(c(1, px[j:(n - 1)]))
    SS  <- sx[j:n]/sx[j]                # salary scale
    tla[j] = sum(SS * v * pxr)          # computing annuity value at j
  } 
  return(tla)
}
 get_tla(rep(0.98, 65), 0.08) # test the function

# 2. function calculating temporary annuity values from a fixed entry age y to x 
get_tla2 = function(px, i, sx = rep(1, length(px))){
  # Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
  # This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 
  
  # Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
  # For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
    # eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
    # eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
  # Note that the last 2 elements are redundant in the calculation, they are just used as place holders. 
    #(calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
    # because the last annuity payment is paid at the begining of 64. )
 
  # inputs:
   # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
   # i:  discount rate, scalar
   # sx: salary scale. default is a n vector of 1, meaning no salary scale. 
  # output:
   # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla = numeric(length(px))
  n = length(tla)
  
  # tla[1] will be kept as 0, next calculate tla[2:n]:
  for(j in 1:(n - 1)){
    v   <- 1/(1 + i)^(0:(j - 1))                                  # dicount vector
    if(j == 1) pxr <- 1 else pxr <- cumprod(c(1, px[1:(j - 1)]))  # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- sx[1:j]/sx[1]                                          # salary scale
    tla[j + 1] = sum(SS * v * pxr)                                # computing annuity value at j;
  } 
  return(tla) 
}
 get_tla2(rep(0.98, 65), 0.08) # test the function

desc <- gam1971 %>% left_join(select(term, age, qxt = ea30)) %>% left_join(disb) %>% # survival rates
        left_join(merit) %>% # merit salary scale
        mutate(scale = scale/scale[age == 30]) %>%
        filter(age >= 30) %>%
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

# Following code is dropped since the scalar is just ax[age == 65].
 # # calcuate annuity value at retirment a..r, note that get_tla is still valid here.
 # arr <- get_tla(px = desc$pxm[desc$age >=65], i  = i)[1] 
 #  # use survival prob from 65 to 110. The first element is the annuity value at 65.   
           
tab5_1 <- desc %>%
  mutate(PTLx = ifelse(age < 65, Bx * ax[age == 65] * px65m * v^(65-age), Bx[age == 65] * ax),
         PCLx = ifelse(age < 65, Bx * ax[age == 65] * px65T * v^(65-age), Bx[age == 65] * ax),
         PTLx.pct = 100 * PTLx / PTLx[age == 65],
         PCLx.pct = 100 * PCLx / PCLx[age == 65]) %>%
  select(age, Bx, pxm, pxT, px65m, px65T, PTLx, PTLx.pct, PCLx, PCLx.pct)
kable(tab5_1, digits = 2)

# Got the same results as Don, but slightly different from the book. 


# table 5-2 actuarial liability and PVFB ####

tab5_2 <- desc %>% filter(age %in% 30:65) %>%
  mutate(PVFBx = ax[age == 65] * Bx[age == 65] * px65T * vrx,
         PVFBx.pct = 100 * PVFBx / PVFBx[age == 65],
         
         ABALx = Bx/Bx[age == 65] * PVFBx,
         ABALx.pct = 100 * ABALx/ABALx[age == 65],
         
         BPALx = Sx/Sx[age == 65] * PVFBx,
         BPALx.pct = 100 * BPALx/BPALx[age == 65],
         
         BDALx = (age - min(age)) / (65-min(age)) * PVFBx,
         BDALx.pct = 100 * BDALx/BDALx[age == 65],
         
         CPALx = ayxs/ayxs[age == 65] * PVFBx,
         CPALx.pct = 100 * CPALx/CPALx[age == 65],
         
         CDALx = ayx/ayx[age == 65] * PVFBx,
         CDALx.pct = 100 * CDALx/CDALx[age == 65]
         ) %>%
  select(age, ABALx.pct, BPALx.pct, BDALx.pct, CPALx.pct, CDALx.pct, PVFBx.pct, PVFBx)
kable(tab5_2, digits = 2)


## Table 6-1 Normal cost as a percent of attained age salary ####

# Keep using assumptions and data("desc") for chapter 5 tables.

tab6_1 <- desc %>% filter(age %in% 30:65) %>%
  mutate(PVFBx = Bx[age == 65] * ax[age == 65] * vrx * px65T,
         bx    = lead(Bx) - Bx) %>%
  # Calculating normal costs under various actuarial methods
  mutate(NCx.AB = bx * ax[age == 65] * px65T * vrx,
         NCx.BD = Bx[age == 65]/(65-30) * px65T * vrx * ax[age == 65],
         NCx.BP = Bx[age == 65]/Sx[age == 65] * sx * px65T * vrx * ax[age == 65],
         NCx.CD = PVFBx[age == 30] / ayx[age == 65],
         NCx.CP = PVFBx[age == 30] / (sx[age == 30] * ayxs[age == 65]) * sx) %>%
#   # alternative way of calcuating NCs: NC as a fraction of the PVFBx
#   mutate(NCx.AB = bx/Bx[age == 65] * PVFBx,
#          NCx.BD = 1/(65 - 30) * PVFBx,
#          NCx.BP = sx/Sx[age == 65] * PVFBx,
#          NCx.CD = 1/ayx[age == 65] * v^(age - 30) * c(1, cumprod(pxT[age %in% 30:64])) * PVFBx,
#          NCx.CP = sx / sx[age == 30] * 1/ayxs[age == 65] * v^(age - 30) * c(1, cumprod(pxT[age %in% 30:64])) * PVFBx) %>%
  # Calculate normal cost as a percent of attained age salary 
  mutate(NCx.AB.pctSal = NCx.AB / sx * 100,
         NCx.BP.pctSal = NCx.BP / sx * 100,
         NCx.BD.pctSal = NCx.BD / sx * 100,
         NCx.CP.pctSal = NCx.CP / sx * 100,
         NCx.CD.pctSal = NCx.CD / sx * 100) %>%
  filter(age %in% seq(30, 64, 2)) %>%
  select(age, 
         NCx.AB, NCx.BP, NCx.BD, NCx.CP, NCx.CD, 
         NCx.AB.pctSal, NCx.BP.pctSal, NCx.BD.pctSal, NCx.CP.pctSal, NCx.CD.pctSal)
kable(tab6_1, digits = 2)
   
# Current results slightly differ from the book a lot(difference in CP is significant). Need to find the cause. 
  # Since table 6-3 is succesfully reproduced, our normal cost calculation should be correct. 
  # The calculation of PVFB should be correct, b/c it can be verified in table 5-2
  # Need to double check the formula and the use of sx. 



## table 6-3 and table 6-4, percentage and cumulative percentage projected retirment benefit allocated to each age. ####

# Formulas for calculating the percentage projected retirment benefit allocated to each age is not explictly 
  # given in the book. But it is easy to infer the allocation formulas. 

# The projected retirment benefit is Bx[age == 65] * ax[age == 65], the multiplies used in each method to get 
# the allocated benefit at age x are given below: 
    # AB: bx / Bx[age == 65]  # 
    # BD: 1/(65 - 30)         # see 3.15a
    # BP: sx / Sx[age == 65]  # see 3.16a
    # CD: (ayx[age == x] - ayx[age == x - 1])/ ayx[age == 65]
    # CP: (ayxs[age == x] - ayxs[age == x - 1])/ ayx[age == 65]

# A even simpler way of calculating allocated benefit: it is just the normal cost without discounting and survival 
  # adjustment factors: allct[age == x] = NCx / (px65T * vrt)


tab6_2 <- desc %>% filter(age %in% 30:65) %>%
  mutate(PVFBx = Bx[age == 65] * ax[age == 65] * vrx * px65T,
         bx    = lead(Bx) - Bx) %>%
  # Calculating normal costs under various actuarial methods: note that they are just normal cost divided by px65T * vrx
  mutate(allct.AB = bx * ax[age == 65],
         allct.BD = Bx[age == 65]/(65-30) * ax[age == 65],   
         allct.BP = Bx[age == 65]/Sx[age == 65] * sx * ax[age == 65], 
         allct.CD = (PVFBx[age == 30] / ayx[age == 65]) / (px65T * vrx),
         allct.CP =((PVFBx[age == 30] / ayxs[age == 65]) * (sx / sx[age == 30])) / (px65T * vrx)
         # Alternative way of calculating allocated benefit at each age for CD and CP
         #allct.CD = c(diff(ayx), 0)/ayx[age == 65] * Bx[age == 65] * ax[age == 65],  # note that 0 is assigned to age 65
         #allct.CP = c(diff(ayxs),0 )/ayxs[age == 65] * Bx[age == 65] * ax[age == 65] # note that 0 is assigned to age 65
         ) %>%
  mutate(allct.AB.pct = allct.AB / (Bx[age == 65] * ax[age == 65]) * 100, 
         allct.BP.pct = allct.BP / (Bx[age == 65] * ax[age == 65]) * 100,
         allct.BD.pct = allct.BD / (Bx[age == 65] * ax[age == 65]) * 100,
         allct.CP.pct = allct.CP / (Bx[age == 65] * ax[age == 65]) * 100,
         allct.CD.pct = allct.CD / (Bx[age == 65] * ax[age == 65]) * 100
         ) %>%
  select(age, ends_with(".pct"))


tab6_3 <- tab6_2 %>%
  mutate(allct.AB.cum = cumsum(ifelse(age == 30, 0, lag(allct.AB.pct))),
         allct.BP.cum = cumsum(ifelse(age == 30, 0, lag(allct.BP.pct))),
         allct.BD.cum = cumsum(ifelse(age == 30, 0, lag(allct.BD.pct))),
         allct.CP.cum = cumsum(ifelse(age == 30, 0, lag(allct.CP.pct))),
         allct.CD.cum = cumsum(ifelse(age == 30, 0, lag(allct.CD.pct)))
         ) %>%
  select(age, ends_with(".cum"))

kable(filter(tab6_2, age %in% seq(30, 64, 2)), digit = 2) # table 6.2
kable(filter(tab6_3,!age %in% seq(31, 63, 2)), digit = 2) # table 6.3

# Results for benefit prorate and cost prorate methods are identical to the book.
# Restuls for accrued benefit method is slightly different from the book. 



# Simple workforce model, based on Don's excel file. 
wf20 <- gam1971 %>% left_join(select(term, age, qxt = ea20)) %>% left_join(disb) %>%
  filter(age >= 20) %>%
  mutate(qxr = ifelse(age == 64, 1, 0 ),
         qxt = ifelse(is.na(qxt), 0, qxt),
         qxd = ifelse(is.na(qxd), 0, qxd),
         year = 1:nrow(wf20)) %>%
  # multi-decrement approx
  mutate(pqxm = qxm * (1 - qxt/2) * (1 - qxd/2) * (1 - qxr/2),
         pqxt = (1 - qxm/2) * qxt * (1 - qxd/2) * (1 - qxr/2),
         pqxd = (1 - qxm/2) * (1 - qxt/2) * qxd * (1 - qxr/2),
         pqxr = (1 - qxm/2) * (1 - qxt/2) * (1 - qxd/2) * qxr
  ) %>%
  # initialize population
  mutate(active = ifelse(age == 20, 100, 0),
         sepv   = 0,
         sepnv  = 0,
         retr   = 0,
         disb   = 0,
         dead   = 0,
         total  = active + sepv + sepnv + retr + disb + dead)

for (i in 1:nrow(wf20)){
  if (i > 1){
    wf20$active[i] = with(wf20, active[i - 1] - activeOUT[i - 1] + activeIN[i - 1])
    wf20$sepv[i]   = with(wf20, sepv[i - 1]   - sepvOUT[i - 1]    + sepvIN[i - 1])
    wf20$sepnv[i]  = with(wf20, sepnv[i - 1]  - sepnvOUT[i - 1]   + sepnvIN[i - 1])
    wf20$disb[i]   = with(wf20, disb[i - 1]   - disbOUT[i - 1]    + disbIN[i - 1])
    wf20$retr[i]   = with(wf20, retr[i - 1]   - retrOUT[i - 1]    + retrIN[i - 1])
    wf20$dead[i]   = with(wf20, dead[i - 1]   + deadIN[i - 1])
    wf20$total[i]   = with(wf20, active[i] + sepv[i] + sepnv[i] + disb[i] + retr[i] + dead[i])
  }
  # where did the actives go at the end of the year?
  wf20$active2sepv[i]  = 0.25 * wf20$active[i] * wf20$pqxt[i]
  wf20$active2sepnv[i] = 0.75 * wf20$active[i] * wf20$pqxt[i]
  wf20$active2disb[i]  = wf20$active[i] * wf20$pqxd[i]
  wf20$active2retr[i]  = wf20$active[i] * wf20$pqxr[i]
  wf20$active2dead[i]  = wf20$active[i] * wf20$pqxm[i]
  wf20$activeOUT[i]    = with(wf20, active2sepv[i] + active2sepnv[i] + active2disb[i] + active2retr[i] + active2dead[i])
  
  # where did the vested seperated go at the end of the year?
  wf20$sepv2active[i]= 0
  wf20$sepv2sepnv[i] = 0 
  wf20$sepv2disb[i]  = 0
  wf20$sepv2retr[i]  = wf20$sepv[i] * wf20$pqxr[i]
  wf20$sepv2dead[i]  = wf20$sepv[i] * wf20$pqxm[i]
  wf20$sepvOUT[i]    = with(wf20, sepv2active[i] + sepv2sepnv[i] + sepv2disb[i] + sepv2retr[i] + sepv2dead[i])
  
  # where did the non-vested seperated go at the end of the year?
  wf20$sepnv2active[i]= 0
  wf20$sepnv2sepv[i]  = 0 
  wf20$sepnv2disb[i]  = 0
  wf20$sepnv2retr[i]  = 0
  wf20$sepnv2dead[i]  = wf20$sepnv[i] * wf20$pqxm[i]
  wf20$sepnvOUT[i]    = with(wf20, sepnv2active[i] + sepnv2sepv[i] + sepnv2disb[i] + sepnv2retr[i] + sepnv2dead[i])
  
  # where did the disabled go at the end of the year?
  wf20$disb2active[i]= 0
  wf20$disb2sepv[i]  = 0 
  wf20$disb2sepnv[i]  = 0
  wf20$disb2retr[i]  = wf20$disb[i] * wf20$pqxr[i]
  wf20$disb2dead[i]  = wf20$disb[i] * wf20$pqxm[i]
  wf20$disbOUT[i]    = with(wf20, disb2active[i] + disb2sepv[i] + disb2sepnv[i] + disb2retr[i] + disb2dead[i])
  
  # where did the retired  go at the end of the year?
  wf20$retr2active[i]= 0
  wf20$retr2sepv[i]  = 0 
  wf20$retr2sepnv[i] = 0
  wf20$retr2disb[i]  = 0
  wf20$retr2dead[i]  = wf20$retr[i] * wf20$pqxm[i]
  wf20$retrOUT[i]    = with(wf20, retr2active[i] + retr2sepv[i] + retr2sepnv[i] + retr2disb[i] + retr2dead[i])
  
  # Total inflow to each status
  wf20$activeIN[i] = with(wf20, sepv2active[i] + sepnv2active[i] + disb2active[i] + retr2active[i])
  wf20$sepvIN[i]   = with(wf20, active2sepv[i] + sepnv2sepv[i]   + disb2sepv[i]   + retr2sepv[i])
  wf20$sepnvIN[i]  = with(wf20, active2sepnv[i]+ sepv2sepnv[i]   + disb2sepnv[i]  + retr2sepnv[i])
  wf20$disbIN[i]   = with(wf20, active2disb[i] + sepv2disb[i]    + sepnv2disb[i]  + retr2disb[i])
  wf20$retrIN[i]   = with(wf20, active2retr[i] + sepv2retr[i]    + sepnv2retr[i]  + disb2retr[i])
  wf20$deadIN[i]   = with(wf20, active2dead[i] + sepv2dead[i]    + sepnv2dead[i]  + disb2dead[i] + retr2dead[i])
  
  wf20$totalIN[i]  = with(wf20, activeIN[i] + sepvIN[i] + sepnvIN[i] + disbIN[i] + retrIN[i] + deadIN[i]) 
  wf20$totalOUT[i] = with(wf20, activeOUT[i] + sepvOUT[i] + sepnvOUT[i] + disbOUT[i] + retrOUT[i]) 
  
}

options(digits = 2, scipen = 99)




## Chpater7 Supplemental Costs ####

# Learning coding supplemental costs through 3 steps:
  # Step 1: Given a increase of the supplemental cost at a single period, calculate the path of amortization.
  # Step 2: Given assumed deterministic paths of inflow and outflow of the pension fund, calculate 
    # amortization for all period and the supplemental cost at each period.
  # Step 3: Similar to step 2, but the inflow and outflow of fund are governed by the acutarial assumptions, 
    # acutuarial methods and other factors that 
    # the funding stutus. 
# Finally, we need to know exactly how to model each of the 5 possible sources of unfunded liabilities mentioned in Ch7. 
# We also need to figure out the role of interest rate in the amortization procedure. 


# a..|-m(m year period certain annuity): sum of the present value of m year fixed payment of $1.

# Step 1
p <- 100 # principle
m <- 15 # year of amortization

infl <- 0.04        # inflation
prod <- 0.01        # productivity
g <- (1 + infl)*(1 + prod) - 1
i <- 0.08           # interest rate
d <- i/(1 + i)      # discount factor 


## Amortization by three methods

# Constant dollar amortization method
cd <- rep(pmt(p, i, m), m)

# Constant percent amortization method
cp <- gaip2(p, i, m, g)*(g + 1)^(1:m - 1)

# Strait line method
pp <- p*(1 + i)
sl <- d*(pp - pp*(1:m)/m) + pp/m


# Simulate amortization

amort <- data.frame(year = 0:m, UL.cd = c(p, rep(0, m)), UL.cp = c(p, rep(0, m)), UL.sl = c(p, rep(0, m)),
                                cd = c(0, cd), cp = c(0, cp), sl = c(0, sl)) 

for (j in 1:15){
  amort[amort$year == j, "UL.cd"] <- amort[amort$year == j - 1, "UL.cd"]*(1 + i) - amort[amort$year == j, "cd"]
  amort[amort$year == j, "UL.cp"] <- amort[amort$year == j - 1, "UL.cp"]*(1 + i) - amort[amort$year == j, "cp"]
  amort[amort$year == j, "UL.sl"] <- amort[amort$year == j - 1, "UL.sl"]*(1 + i) - amort[amort$year == j, "sl"]
}

kable(amort)

# Reproduce the graphs in p103 and p104

# figure 7-1a
amort %>% select(year, cd, cp, sl) %>% filter(year>0) %>%
  gather(methods, SC, -year) %>% 
  ggplot(aes(x = year, y = SC, color = methods)) + theme_bw() + 
  geom_line(size = 1) + geom_point(size = 3) +  
  scale_x_continuous(breaks = 1:m, limits = c(1, m)) + 
  scale_y_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
  scale_color_discrete(label = c("Constant Dollar","Constant Percent", "Straight Line")) + 
  theme(legend.justification=c(0,0), legend.position=c(0, 0))
  

# figure 7-2a
amort %>% select(year, UL.cd, UL.cp, UL.sl) %>%
  gather(methods, balance, -year) %>% 
  ggplot(aes(x = year, y = balance, color = methods)) + theme_bw() + 
  geom_line(size = 1) + geom_point(size = 3) +  
  scale_x_continuous(breaks = 0:m, limits = c(0, m)) + 
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100)) +
  scale_color_discrete(label = c("Constant Dollar","Constant Percent", "Straight Line")) + 
  theme(legend.justification=c(0,0), legend.position=c(0, 0))


# Step 2 

# The two fundamental variables are acturial liabilities(AL) and assets(AS)
 # The actual AL is determined by   AL(n+1) = [AL(n) + NC(n) - B(n)]*(1 + i)   
 # The actual AS is determined by   AS(n+1) = [AS(n) + Cont(n) - B(n)]*(1 + i)   

# The expected AL(n+1) and AS(n+1) at n, i.e. E[AL(n+1)] and E[AS(n+1)], also follows the formula above 
   # but uses the expected NC(n), Cont(n) and B(n):
   # E[AL(n+1)]n = [E[AL(n)]n + E[NC(n)]   - E[B(n)] ]*(1 + E[i])
   # E[AS(n+1)]n = [E[AS(n)]n + E[Cont(n)] - E[B(n)] ]*(1 + E[i])
 # We here assume the contribution is equal to the sum of payments for normal cost and total supplemental costs
   # Cont(n)    = PNC(n) + sum(SC(n)) 
   # E[Cont(n)] = E[PNC(n)] + sum(SC(n)); where E[PNC(n)] = E[NC(n)]
   # Note that sum(SC(n)) includes the amortized payments for unfunded liablities from all previous periods. 
   # Note that the sponsor is expected to pay the normal cost each period, but the acutal payment may differ. 
   
 # The sources of unfunded liabilities
   # - AL(n) vs E[AL(n)]n
   # - AS(n) vs E[AS(n)]n
   # - NC(n) vs E[NC(n)]
   # - PNC(n) vs E[PNC(n)] (E[NC(n)])
   # - i vs E[i] 
   # - (here we assume the amortized UL are always paid in full at each period)

 # Note that expectation notation E[.] is used to denote the values without any events that may lead 
   # to supplemental costs(eg. actuarial gain/loss, assumption changes). So "expectation" here has little
   # to do with probability.   

 # The inital inputs to the simulation:
   # - initial values for AL and AS
   # - paths of expected NC, B, i
   # - paths of actual   NC, B, i
   # - (note that contribution is determined by NC and SC in this model.)
   
 # By setting the paths for actual AL, AS, NC, B, i, we can simulate the 5(6?) sources of supplemental costs. 
   # - Experience Variations
   # - Assumption Changes
   # - Benefit changes
   # - Past service accruals 
   # - Contribution variations
   # - (unexpected low/high rate of return on asset.)


# Parameters ()
m <- 5 # year of amortization
infl <- 0.04        # inflation
prod <- 0.01        # productivity
g <- (1 + infl)*(1 + prod) - 1
i <- 0.08           # interest rate
d <- i/(1 + i)      # discount factor 

nyear <- 10

AL1 <- 1e3

# Creating data frame for assets and liabilities

# Set up data frame
penSim <- data.frame(year = (1:(nyear + m))) %>%
  mutate(AL = ifelse(year == 1, AL1, 0),  # actual AL(n)
         EAL= ifelse(year == 1, AL1, 0),  # expected AL: E[AL(n+1)]n
         AS = ifelse(year == 1, AL1, 0),  # actual AS(n)
         EAS= ifelse(year == 1, AL1, 0),  # expected AS: E[AS(n+1)]n
         UL = 0,                          # unfunded liability: AL - AS
         EUL= 0,                          # expected unfunded liability: E[AL(n+1)]n - E[AS(n+1)]n
         dUL= 0,                          # change in unexpected liablity: dUL(n) = UL(n+1) - E[UL(n+1)]
         NC  = 0,                         # actual NC(n)
         ENC = 0,                         # expected NC: E[NC(n)]
         B   = 0,                         # Benefit payment
         SC  = 0,                         # supplement cost
         #SC2 = 0,                         # supplement cost by alternative approach
         PNC = 0,                         # actual contribution for NC
         EPNC= 0,                         # Expected contribution for NC
         Cont= 0,                         # acutal contribution: PNC + SC
         ECont = 0,                       # expected contribution: EPNC + SC
         i   = i,                         # actual rate of return
         Ei  = i                          # expected rate of reurn
                    )

# set up scenario of paths

# Start with a simple case: the only source of supplemental costs is the discrepancy between PNC and NC

penSim %<>% mutate(NC = seq(10, by = 0, l = nyear + m),
                   ENC= NC,                               # assume no unexpected NC
                   B  = seq(10, by = 0, l = nyear + m),
                   EPNC = NC,                             # sponsor is expected to pay full normal cost
                   PNC  = ifelse(year>=nyear, EPNC, ifelse(year<= nyear/2, 8, 12))
                   ) 


# matrix representation of amortization: better visualization but large size, used in this excercise
SC_amort <- matrix(0, nyear + m + m, nyear + m + m)
SC_amort
# data frame representation of amortization: much smaller size, can be used in real model later.
#SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))


for (j in 2:(nyear + m)){
  
  

  # Actual and expected AL
  penSim[penSim$year == j, "AL"] <- (penSim[penSim$year == j - 1, "AL"] + penSim[penSim$year == j - 1, "NC"] - penSim[penSim$year == j - 1, "B"]) * 
                                        (penSim[penSim$year == j - 1, "Ei"] + 1)
  penSim[penSim$year == j, "EAL"] <- (penSim[penSim$year == j - 1, "AL"] + penSim[penSim$year == j - 1, "ENC"] - penSim[penSim$year == j - 1, "B"]) * 
                                        (penSim[penSim$year == j - 1, "Ei"] + 1) 
  
    # Note that when calculating liability, no matter actual or expected, the assumed interest rate is used. 
  
  # Actual and expected AS
  
  penSim[penSim$year == j - 1, "Cont"]  <- penSim[penSim$year == j - 1, "PNC"]  + penSim[penSim$year == j - 1, "SC"] 
  penSim[penSim$year == j - 1, "ECont"] <- penSim[penSim$year == j - 1, "EPNC"] + penSim[penSim$year == j - 1, "SC"] 
                                           
  penSim[penSim$year == j, "AS"] <- (penSim[penSim$year == j - 1, "AS"] + penSim[penSim$year == j - 1, "Cont"] - penSim[penSim$year == j - 1, "B"]) * 
    (penSim[penSim$year == j - 1, "i"] + 1) 
  penSim[penSim$year == j, "EAS"] <- (penSim[penSim$year == j - 1, "AS"] + penSim[penSim$year == j - 1, "ECont"] - penSim[penSim$year == j - 1, "B"]) * 
    (penSim[penSim$year == j - 1, "Ei"] + 1) 
  
  # UL and EUL
  penSim[penSim$year == j, "UL"]  <- penSim[penSim$year == j, "AL"] - penSim[penSim$year == j, "AS"] 
  penSim[penSim$year == j, "EUL"] <- penSim[penSim$year == j, "EAL"] - penSim[penSim$year == j, "EAS"] 

  # change in UL
  penSim[penSim$year == j - 1, "dUL"] <- penSim[penSim$year == j, "UL"] - penSim[penSim$year == j, "EUL"]
  
  # Amortize dUL at j over the next m years
  SC_amort[j, j:(j + m - 1)] <- rep(pmt(penSim[penSim$year == j - 1, "dUL"], i, m), m)  # constant dollar amortization
  #SC_amort[j, j:(j + m - 1)] <- gaip(penSim[penSim$year == j - 1, "dUL"], i, m, g)*(g + 1)^(1:m - 1)  # constant percent amortization
  # Supplemental cost in j
  penSim[penSim$year == j, "SC"] <- sum(SC_amort[, j])
  #penSim[penSim$year == j, "SC2"] <- pmt(penSim[penSim$year == j, "UL"], i, m)
  
}

SC_amort
kable(penSim, digits = 3)

# In a real model, EUL(t+1)t is computed using similar formulas defined above, while UL(t+1) is obtained from the real experience of the model.
  # The gain/loss is then calculated as dUL = EU(t+1)t - UL(t+1). This formula is applicable when arbitrary types of sources of UL are present, but 
  # it cannot distinguish these sources. 
  # # - Experience Variations
    # - Assumption Changes
    # - Benefit changes
    # - Past service accruals 
    # - Contribution variations
    # - (unexpected low/high rate of return on asset.)


# In last period, unfunded liabilities are "almost" funded, but not exactlyu. Any problem in the code?
  
  # Possible reason: There should be another component in Cont(n): the interest of the unfunded liability. 
  # This component plus the normal cost paid in full will make the unfunded liablity constant over time. 

  # update: above is actually not the reason.  Sticking to Winklevoss book solves the problem. 
  # Note dUL(n) is the change in UL at the END of period n. 

pmt(penSim[penSim$year == 2, "UL"], i, m)



# Chapter 8 Ancillary benefits ####

benfactor <- 0.015  # benefit factor, 1.5% per year of yos
fasyears  <- 5      # number of years in the final average salary calculation
infl <- 0.04        # inflation
prod <- 0.01        # productivity
i <- 0.08           # interest rate
v <- 1/(1 + i)      # discount factor
yos.v <- 5          # yos required for vesting
age.d <- 40         # age required for eligibility for disability benefit
yos.d <- 10         # yos required for eligibility for disability benefit
yos.s <- 5
M <- 0.85            # prob that the participant has a surviving spouse at death
# Assuming no waiting period for disability benefit. w = 0
# Assuming the spouse is at the same age as the dead participant. u = 0.

desc <- rename(gam1971, qxm.p = qxm) %>% left_join(select(term, age, qxt.p = ea30)) %>% left_join(rename(disb, qxd.p = qxd)) %>% left_join(rename(dbl, qxmd.p = qxmd)) %>% # survival rates
  left_join(merit) %>% # merit salary scale
  mutate(scale = scale/scale[age == 30]) %>%
  filter(age >= 30) %>%
  # Calculate survival rates
  mutate( 
          pxm = 1 - qxm.p,
          pxmd = 1 - qxmd.p,
          pxT = (1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          px65m = order_by(-age, cumprod(ifelse(age >= 65, 1, pxm))), # prob of surviving up to 65, mortality only
          px65T = order_by(-age, cumprod(ifelse(age >= 65, 1, pxT))), # prob of surviving up to 65, composite rate
          p65xm = cumprod(ifelse(age <= 65, 1, lag(pxm))),            # prob of surviving to x from 65, mortality only
          
          qxt = qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2) * 1,
          qxd = (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
          qxm = (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
          
          vrx = v^(65-age)) %>%
  # Calculate salary and benefits
  mutate(sx = scale * (1 + infl + prod)^(age - min(age)),   # Composite salary scale
         Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
         yos= age - min(age),                               # years of service
         n  = pmin(yos, fasyears),                          # years used to compute fas
         fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, 5))/n), # final average salary
         fas= ifelse(age == min(age), 0, fas),
         Bx = benfactor * yos * fas,                        # accrued benefits
         bx = lead(Bx) - Bx, 
         ax = get_tla(pxm, i), # Since retiree die at 110 for sure, the life annuity is equivalent to temporary annuity up to age 110. Mortality only
         ax65 = c(get_tla(pxT[age<65],i), rep(0, 46)),             # aT..{x:65-x-|} discount value of 65 at age x, using composite decrement       
         ax65s= c(get_tla(pxT[age<65],i, sx[age<65]), rep(0, 46)), # ^s_aT..{x:65-x-|}
         axd = get_tla(pxmd, i),                             # Value of life time annuity calculated using mortality for disabled. Used in ancillary benefit. 
         ayx = c(get_tla2(pxT[age<=65], i), rep(0, 45)),             # need to make up the length of the vector to 81
         ayxs= c(get_tla2(pxT[age<=65], i, sx[age<=65]), rep(0, 45))  # need to make up the length of the vector to 81
  )                              


get_PVFB <- function(px, v, TC){
  n <- length(px)
  #PVFB <- numeric(n)
  
  PVFB <- sapply(1:n, function(j) sum(cumprod(px[j:n] * v) / v * TC[j:n]))
  
#   for(j in 1:n){
#     PVFB[j] <- sum(cumprod(px[j:n] * v) / v * TC[j:n]) 
#   }
  return(PVFB)
}
get_PVFB(rep(0.98, 64), v, seq(1,1.5, len = 64)) # test the function



tab8_1 <- desc %>% 
  # vested benefits
  mutate(PVFBx.r = Bx[age == 65] * ax[age == 65] * vrx * px65T,
         gx.v  = ifelse(yos >= yos.v, 1, 0), # grading function equal to the proportion of accrued benefit vested at age x. For now, fully vested after a given number of yos.
         TCx.v = gx.v * Bx * qxt * lead(px65m) * v^(65 - age) * ax[age == 65],  # term cost of vested termination benefits
         TCx.v_pct = 100 * TCx.v / sx,
         PVFBx.v = c(get_PVFB(pxT[age <= 64], v, TCx.v[age <= 64]), rep(0, 46)),
         PVFBx.v_pct = 100 * PVFBx.v / PVFBx.r
         ) %>%
  # disabled benefits
  mutate(gx.d = ifelse(yos >= yos.d & age >= age.d, 1, 0),
         TCx.d = gx.d * Bx * qxd * v * lead(axd),
         TCx.d_pct = 100 * TCx.d / sx,
         PVFBx.d = c(get_PVFB(pxT[age <= 64], v, TCx.d[age <= 64]), rep(0, 46)),
         PVFBx.d_pct = 100 * PVFBx.d / PVFBx.r
         )%>%  
  # surviving spouse benefits
  mutate(gx.s = ifelse(yos >= yos.s, 0.5, 0),
         TCx.s = M * gx.s * Bx * qxm * v * lead(ax),
         TCx.s_pct = 100 * TCx.s / sx,
         PVFBx.s = c(get_PVFB(pxT[age <= 64], v, TCx.s[age <= 64]), rep(0, 46)),
         PVFBx.s_pct = 100 * PVFBx.s / PVFBx.r) %>% 
  filter(age <=65) %>% select(age, TCx.v, TCx.d, TCx.s, 
                                   PVFBx.v, PVFBx.d, PVFBx.s,
                                   TCx.v_pct, PVFBx.v_pct,
                                   TCx.d_pct, PVFBx.d_pct,
                                   TCx.s_pct, PVFBx.s_pct)

kable(tab8_1, digits = 2)

# Numbers are similar but not identical to Winklevoss. 



tab8_2 <- desc %>% 
  # vested benefits
  mutate(gx.v  = ifelse(yos >= yos.v, 1, 0), # grading function equal to the proportion of accrued benefit vested at age x. For now, fully vested after a given number of yos.
         TCx.v = gx.v * Bx * qxt * lead(px65m) * v^(65 - age) * ax[age == 65],  # term cost of vested termination benefits
         PVFBx.v = c(get_PVFB(pxT[age <= 64], v, TCx.v[age <= 64]), rep(0, 46)),
         TCax.v = bx/Bx * TCx.v,                                                 # term cost accural of vested termination benefits
         NCx.v = c(get_PVFB(pxT[age <= 64], v, TCax.v[age <= 64]), rep(0, 46))   # Normal cost component of vested benefit
  ) %>%
  # disabled benefits
  mutate(gx.d = ifelse(yos >= yos.d & age >= age.d, 1, 0),
         TCx.d = gx.d * Bx * qxd * v * lead(axd),
         PVFBx.d = c(get_PVFB(pxT[age <= 64], v, TCx.d[age <= 64]), rep(0, 46)),
         TCax.d = bx/Bx * TCx.d,                                                 # term cost accural of vested termination benefits
         NCx.d = c(get_PVFB(pxT[age <= 64], v, TCax.d[age <= 64]), rep(0, 46))   # Normal cost component of vested benefit
  )%>%  
  # surviving spouse benefits
  mutate(gx.s = ifelse(yos >= yos.s, 0.5, 0),
         TCx.s = M * gx.s * Bx * qxm * v * lead(ax),
         PVFBx.s = c(get_PVFB(pxT[age <= 64], v, TCx.s[age <= 64]), rep(0, 46)),
         TCax.s = bx/Bx * TCx.s,                                                 # term cost accural of vested termination benefits
         NCx.s = c(get_PVFB(pxT[age <= 64], v, TCax.s[age <= 64]), rep(0, 46))   # Normal cost component of vested benefit
  ) %>% 
  # retirement benefits
  mutate(PVFBx.r = Bx[age == 65] * ax[age == 65] * vrx * px65T,
         NCx.r   = bx/Bx[age == 65] * PVFBx.r,
         ALx.r.PUC = Bx/Bx[age == 65] * PVFBx.r
  ) %>% 
  # Calculate Normal costs and ALs.
  mutate(PVFBx.T = PVFBx.r[age == 30] + PVFBx.v[age == 30] + PVFBx.d[age == 30] + PVFBx.s[age == 30],
         
         NCx.PUC = NCx.r + NCx.v + NCx.d + NCx.s,
         NCx.EAN.CD = PVFBx.T/ayx[age == 65],
         NCx.EAN.CP = PVFBx.T/(sx[age == 30] * ayxs[age == 65]) * sx,
         
         ALx.PUC = ALx.r.PUC + PVFBx.v + PVFBx.d + PVFBx.s,
         ALx.EAN.CD = PVFBx.r - NCx.EAN.CD * ax65,
         ALx.EAN.CP = PVFBx.r - NCx.EAN.CP * ax65s
         ) %>%
  filter(age %in% seq(30, 60, 5)) %>% 
  select(age, TCx.v, TCx.d, TCx.s, 
         PVFBx.v, PVFBx.d, PVFBx.s,
         NCx.PUC, NCx.EAN.CD, NCx.EAN.CP,
         ALx.PUC, ALx.EAN.CD, ALx.EAN.CP
                              )
                             

kable(tab8_2, digits = 2)



