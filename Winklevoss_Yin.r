# Yimeng Yin
# 12/13/2013

# The code in script replicate works through Winklevoss pension book following the code written by Don.(Winklevoss(7).r)
# for practice purpose. 
# Don's comments are retained as much as possible



## Preamble ####

# Defining data directory and data file
wvd <- "E:/Dropbox (Personal)/Proj-PenSim/Winklevoss/"
wvxl <- "Winklevoss(6).xlsx"


library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(tidyr) # gather, spread
library(xlsx)

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

load(paste0(wvd, "winklevossdata.rdata"))

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
  ungroup %>%
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
               ayxs= c(get_tla2(pxT[age<=65], i, sx[age<65]), rep(0, 45))  # need to make up the length of the vector to 81
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


  








  













