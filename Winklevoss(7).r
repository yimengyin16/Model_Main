# Winklevoss(#).r
# Don Boyd
# 12/7/2014

# Work through parts of the Winklevoss pension mathematics book. 
# I try to use his notation, where practical.
# There is a useful glossary in the back of the book. Also, the Wikipedia site is useful.
# http://en.wikipedia.org/wiki/Actuarial_notation

# I created an Excel workbook that has:
# (a) most of his "model plan" decrement tables and other assumptions,
# (b) my calculations when I was trying to figure out how he got a particular result.
# It is in the dropbox.

wvd <- "C:/Dropbox (Personal)/Proj-PenSim/Winklevoss/"
wvxl <- "Winklevoss(6).xlsx"

library(zoo) # rollapply
library(knitr)
library(gdata)
library(dplyr)
library(ggplot2)
library(tidyr) # gather, spread


# also needs gdata, dplyr, ggplot2, and possibly a few other packages I load automatically
# if a function I use doesn't work, a quick google search should identify the package it's in, and you'll have to load that
# in addition, it uses two functions in my "btools" package - you can install btools from github
# and load, or else just use the functions below:
 cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric
 ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail


#****************************************************************************************************
#
#                Read and save model-plan tables - RUN ONCE ####
#
#****************************************************************************************************
# data table 2-1 mortality ####
# you can get various mortality tables from the SOA website. Winklevoss uses a table called GAM-1971, so rather
# than type it in I downloaded it from the SOA site and put it in the Winklevoss directory as GAM-1971-Male.xls. 
# The url is: http://mort.soa.org/Export.aspx?Type=xls&TableIdentity=818

fn <- "GAM-1971-Male.xls"
gam1971 <- read.xls(paste0(wvd, fn), colClasses="character")
ht(gam1971)
dim(gam1971)
names(gam1971) <- c("age", "qxm")
gam1971 <- gam1971 %>% mutate_each(funs(cton)) %>%
  filter(age %in% 5:110)

# data table 2-3 termination rates ####
term <- read.xls(paste0(wvd, wvxl), sheet="Tab2-3TermRates", colClasses="character")
term <- term[-c(1:2), ]
names(term) <- c("age", paste0("ea", seq(20, 60, 5)))
term <- term %>% mutate_each(funs(cton))

# data table 2-5 disability life rates ####
dbl <- read.xls(paste0(wvd, wvxl), sheet="Tab2-5DisbLife", colClasses="character")
dbl <- dbl[-c(1:1), ]
names(dbl) <- c("age", "qxmd")
dbl <- dbl %>% mutate_each(funs(cton))

# data table 2-7 disability ####
disb <- read.xls(paste0(wvd, wvxl), sheet="Tab2-7Disb", colClasses="character")
disb <- disb[-c(1:1), ]
names(disb) <- c("age", "qxd")
disb <- disb %>% mutate_each(funs(cton))

# data table 2-9 early retirement ####
er <- read.xls(paste0(wvd, wvxl), sheet="Tab2-9EarlyRet", colClasses="character")
er <- er[-c(1:1), ]
names(er) <- c("age", "qxe")
er <- er %>% mutate_each(funs(cton))

# data table 2-10 salary merit table ####
merit <- read.xls(paste0(wvd, wvxl), sheet="Tab2-10Merit", colClasses="character")
merit <- merit[-c(1:1), ]
names(merit) <- c("age", "scale")
merit <- merit %>% mutate_each(funs(cton))

# data table 4-6 hiring distribution table ####
hire <- read.xls(paste0(wvd, wvxl), sheet="Tab4-6HireDist", colClasses="character")
hire <- hire[-c(1:1), ]
names(hire) <- c("eage", "dist", "salscale")
hire <- hire %>% mutate_each(funs(cton))
hire
sum(hire$dist)
hire %>% mutate(pctshare=dist / sum(dist) * 100)

save(gam1971, term, dbl, disb, er, merit, hire, file=paste0(wvd, "winklevossdata.rdata"))


#****************************************************************************************************
#
#                Construct various tables in the book ####
#
#****************************************************************************************************

load(paste0(wvd, "winklevossdata.rdata"))

# table 2-2 mortality - survival probabilities - p.16 ####
gam1971 %>% arrange(age) %>%
  mutate(pxm=1-qxm, 
         pxm.65mx=order_by(desc(age), cumprod(ifelse(age<65, pxm, 1))), 
         pxm.xm65=order_by(age, cumprod(ifelse(age>65, lag(pxm), 1)))) %>%  # note use of lag(in dplyr not in stats)
  filter(age %in% seq(20, 110, 5)) %>%
  kable(digits=4)

# Notes:
  # 1. order_by(): define the path through which the function cumprod is applied
  # 2. The survival at age x is evaluated at the end of the year(or equivalently, the beginning of age x + 1)
  # 3. Note that when x = 66, we have n = 1, hence we are actually calculating the prob of living through
       # the age 65. 
  # 4. So when we say the participant is at age x, we are actually saying she is at the begining of age x.

# table 2-4 - retention from ay until +5 or 65 ####
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

# table 2-6 disabled-life survival probabilities ####
dbl %>% arrange(age) %>%
  mutate(pxmd=1-qxmd, 
         pxmd.65mx=order_by(desc(age), cumprod(ifelse(age<65, pxmd, 1))), 
         pxmd.xm65=order_by(age, cumprod(ifelse(age>65, lag(pxmd), 1)))) %>%  # note use of lag
  filter(age %in% seq(20, 110, 5)) %>%
  kable(digits=4)

# table 2-8 disability-based survival in service ####
disb %>% rbind(data.frame(age=65, qxd=NA)) %>% # add age 65, to complete the table
  arrange(age) %>%
  mutate(pxd=1-qxd,
         pxd.65mx=order_by(desc(age), cumprod(ifelse(age<65, pxd, 1)))) %>%  
  filter(age %in% seq(20, 65, 5)) %>%
  kable(digits=4)

# table 2-11 - merit pay ####
infl <- .04 # inflation, used in book
prod <- .01 # productivity, used in book
merit %>% arrange(age) %>%
  mutate(iscale=(1+infl)^(row_number()-1),
         pscale=(1+prod)^(row_number()-1),
         mtscale=scale*iscale*pscale,   # this seems like the most correct way to compute the combined scale
         mtscale2=(1 +infl +prod)^(row_number()-1)*scale, # this appears to be how they did it - no interaction between inflation and productivity
         mtscale64=mtscale2[age==64] / mtscale2,
         aagr=mtscale64^(1/(64-age))-1) %>%
  select(age, mtscale64, aagr) %>%
  filter(age %in% seq(20, 60, 5)) %>%
  kable(digits=3)

# table 3-1,multiple decrements: mortality, termination, disability ####
# but not early retirement (I have not included er, anyway)
# note that my gam1971 has one more decimal place than Winklevoss
gam1971 %>% left_join(term) %>%
  left_join(disb) %>%
  filter(age>=20, age<=64) %>%
  gather(entry, qxt, -age, -qxm, -qxd, na.rm=TRUE) %>%
  mutate(entry=as.numeric(gsub("[^0-9]", "", entry)),
         pxtot=(1-qxm) * (1-qxd) * (1-qxt)) %>%
  group_by(entry) %>%
  mutate(pxtot.65mx=order_by(desc(age), cumprod(ifelse(age<65, pxtot, 1)))) %>% # don't really need the ifelse here since no one is > 64
  ungroup() %>%
  select(age, entry, pxtot.65mx) %>%
  spread(entry, pxtot.65mx) %>%
  kable(digits=3, caption="Survival in service to age 65 from different ages, varying by age of entry (columns)")
# I think the book may be wrong for age 50 entrants - it has big jump from age 54 (y+4) to 55 (x); no earlier entry age does (but ea55 does)
# maybe the differences for earlier years can be explained by number of decimal places in gam1971
# maybe he reflected early retirement in some fashion but the book (p.25) suggests not. furthermore, I looked at the numbers
# and I don't think they reflected it


# table 3-2 (entry age 20, only) - examine multiple decrements  ####
# look at the decrement tables
glimpse(gam1971)
glimpse(term)
glimpse(db)

# one way to update beginning population with prior ending population is with a loop, implemented in the following function
getdec <- function(df) {
  for(i in 1:nrow(df)){
    if(i>1) df$lxb[i] <- df$lxe[i-1] else df$lxb[i] <- 1e6 # opening population
    df$dxm[i] <- df$qxm2[i] * df$lxb[i]
    df$dxt[i] <- df$qxt2[i] * df$lxb[i]
    df$dxd[i] <- df$qxd2[i] * df$lxb[i]
    df$dxtot[i] <- df$dxm[i] + df$dxt[i] + df$dxd[i]
    df$lxe[i] <- df$lxb[i] - df$dxtot[i] # ending population
  }
  return(df)
}
dtab <- select(gam1971, age, qxm) %>%
  left_join(select(term, age, qxt=ea20)) %>% # entry age 20 termination probs
  left_join(select(disb, age, qxd)) %>%
  filter(age>=20) %>%
  mutate(pxm=1-qxm, pxt=1-qxt, pxd=1-qxd) %>%
  # approximate the multiple decrement probabilities - see eq 3.2b
  mutate(qxm2=qxm * (1-qxt/2) * (1-qxd/2), 
         qxt2=(1-qxm/2) * qxt * (1-qxd/2),
         qxd2=(1-qxm/2) * (1-qxt/2) * qxd,
         qxtot=qxm2 + qxt2 + qxd2) %>%
  arrange(age) %>%
  do(getdec(.)) # calculate the decrements
dtab %>% filter(age %in% 20:65) %>%
  select(age, lxb, dxm, dxt, dxd, dxtot) %>% 
  kable(digits=0) # Table 3-2
dtab

# an alternative approach that does not require a loop - calc lxb all at once from cumlative probs
gam1971 %>% left_join(select(term, age, qxt=ea20)) %>% # entry age 20 termination probs
  left_join(disb) %>%
  filter(age %in% 20:65) %>%
  arrange(age) %>%
  mutate(pxtot=(1-qxm) * (1-qxd) * (1-qxt),
         npxtot=cumprod(ifelse(age<65, pxtot, 1)),
         lxb=ifelse(age==20, 1e6, 1e6*lag(npxtot))) %>%
  # approximate the multiple decrement probabilities - see eq 3.2b
  mutate(qxm2=qxm * (1-qxt/2) * (1-qxd/2), 
         qxt2=(1-qxm/2) * qxt * (1-qxd/2),
         qxd2=(1-qxm/2) * (1-qxt/2) * qxd,
         qxtot=qxm2 + qxt2 + qxd2) %>%
  mutate(dxm=qxm2 * lxb,
         dxt=qxt2 * lxb,
         dxd=qxd2 * lxb,
         dxtot=dxm + dxt + dxd) %>% 
  select(age, lxb, dxm, dxt, dxd, dxtot) %>% 
  kable(digits=0) # Table 3-2
  

# table 3-3 compound interest ####
f <- function(i, t) return((1/(1+i)) ^ t)
t <- seq(0, 70, 5)
data.frame(t, i6=f(.06, t), i8=f(.08, t), i10=f(.1, t)) %>% 
  kable(digits=4)

# table 3-4 salary factors for different entry ages ####
infl <- .04 # inflation, used in book
prod <- .01 # productivity, used in book
merit %>% arrange(age) %>%
  mutate(iscale=(1+infl)^(row_number()-1),
         pscale=(1+prod)^(row_number()-1),
         mtscale2=(1 + infl + prod)^(row_number()-1)*scale, # this appears to be how they did it - no interaction between inflation and productivity
         m20=ifelse(age>=20, mtscale2/mtscale2[age==20], NA),
         m30=ifelse(age>=30, mtscale2/mtscale2[age==30], NA),
         m40=ifelse(age>=40, mtscale2/mtscale2[age==40], NA),
         m50=ifelse(age>=50, mtscale2/mtscale2[age==50], NA),
         m60=ifelse(age>=60, mtscale2/mtscale2[age==60], NA)) %>%
  select(age, m20:m60) %>%
  kable(digits=3)

  # Note: the none-interaction way is not consistent with eq.3.8

# table 3-5 benefit accrual for age-30 entrant ####
benfactor <- .015
fasyears <- 5

# use ma function instead of difference in cumulative salaries
ma <- function(x, years) rollapply(x, years, function(x) mean(x, na.rm=TRUE), partial=TRUE, align="right") # use partial to compute average of short series
 cbind(1:10, ma(1:10, 5)) # check to make sure ma works properly
merit %>% rbind_list(data.frame(age=65, scale=NA)) %>% # need to add a 65th year so we can calc Bx for it
  filter(age %in% 30:65) %>%
  arrange(age) %>%
  mutate(ssea30=scale / scale[age==30] * {(1+infl+prod) ^ (age-30)},
         Sx=ifelse(age>30, lag(cumsum(ssea30)), 0), # cumulative salary - note that it starts at zero - per p.38 at top
         fas=ifelse(age>30, ma(lag(ssea30), fasyears), 0),
         Bx=benfactor * (age-30) * fas,
         bx=lead(Bx) - Bx,
         bxpct=bx / Bx[age==65] * 100,
         Bxpct=Bx / Bx[age==65] * 100) %>%
  # now do constant collar and constant percent calculations
  mutate(bx.CD=Bx[age==65]/(65-30),   # corrected an error
         Bx.CD=Bx[age==65] * (age-30) / (65-30),
         bxpct.CD=bx.CD / Bx.CD[age==65] * 100,
         Bxpct.CD=Bx.CD / Bx.CD[age==65] * 100,
         bx.CP=Bx[age==65] * ssea30 / Sx[age==65], 
         Bx.CP=Bx[age==65] * Sx / Sx[age==65],
         bxpct.CP=bx.CP / Bx.CP[age==65] * 100,
         Bxpct.CP=Bx.CP / Bx.CP[age==65] * 100) %>%
  select(age, bxpct, Bxpct, bxpct.CP, Bxpct.CP, bxpct.CD, Bxpct.CD) %>%
  kable(digits=2)

# this also can be done using cumulative salaries Sx instead of ma (cum salary - 5 year lag of cum salary)
# construct the unmodified part of table 3-5
# need to fix this up - I did this earlier and think the cumulative salary should be zero in first year
merit %>% rbind_list(data.frame(age=65, scale=NA)) %>% # need to add a 65th year so we can calc Bx for it
  filter(age %in% 30:65) %>%
  arrange(age) %>%
  mutate(ssea30=scale / scale[age==30] * {(1+infl+prod) ^ (age-30)},
         Sx=cumsum(ssea30), # cumulative salary
         yos.eoy=age - 30 + 1, # years of service at end of year
         n=pmin(fasyears, yos.eoy),
         salprior=ifelse(row_number()<=fasyears, 0, lag(Sx, fasyears)),
         fas=(Sx - salprior) / n,
         Bx=ifelse(row_number()==1, 0, lag(fas) * lag(yos.eoy) * benfactor)) %>%
  mutate(bx=lead(Bx) - Bx,
         bxpct=bx / Bx[age==65] * 100,
         Bxpct=Bx / Bx[age==65] * 100) %>%
  select(-scale) %>%
  kable(digits=2)


# table 3-7 temporary life annuities ####
# In a temporary life annuity, each payment is made only if a designated person is then alive, but the payments are limited to a fixed number of years.
# In a whole life annuity, the payments continue for the entire lifetime of a designated person.
# http://actuarialsciencestudies.blogspot.com/2012/04/temporary-life-annuities.html
# annuity-due is paid at the start of the year; annuity-immediate is paid at the end of the year (or start of the next)
# eq 3.22 aTxn = sum[0, n-1] of tpxtot * v^t
# for this we need pxtot at entry age 30
f <- function(df, i){
  for(age in 30:63){
    idx=age - 30 + 1 # runs from 1 to 34
    n <- nrow(df) - 1 # always 34
    # get the survival-weighted present value of an annuity paid at the beginning of each year, starting at the current
    # age and continuing through age 64 (recognizing that the annuity will not be paid if the person dies, is disabled, or is terminated)
    probs=c(1, df$pxtot[idx:n])
    vt <- (1/ (1+i)) ^ (0:(64 - age))
    df$tla[idx] <- sum(cumprod(probs) * vt)
  }
  df$tla[35] <- 1
  return(df)
}
gam1971 %>% left_join(select(term, age, qxt=ea30)) %>% # entry age 30 termination probs
  left_join(disb) %>%
  filter(age %in% 30:64) %>%
  arrange(age) %>%
  mutate(pxtot=(1-qxm) * (1-qxd) * (1-qxt)) %>%
  select(age, pxtot) %>%
  do(f(., i=.08)) %>% 
  kable(digits=2)
# This is close but is not quite identical to the book. Maybe it is just rounding differences?
# Come back to it. We need to get it right for some of the cost methods.


# table 4-7 population statistics - come back to this as it requires some guesswork to figure out what he did ####


# table 5-1 ptl and pcl liability measures ####
# key assumptions
benfactor <- .015  # benefit factor - 1.5% per year of service yos
fasyears <- 5 # number of years in the final average salary calculation
infl <- .04 # inflation, used in book
prod <- .01 # productivity, used in book
i <- .08
v <- 1/(1+i)

# first, build a df with the necessary info - use entry age 30
# decrements
decs <- select(gam1971, age, qxm) %>%
  left_join(select(term, age, qxt=ea30)) %>% # entry age 30 termination probs
  left_join(select(disb, age, qxd)) %>%
  filter(age>=30) %>%
  mutate(pxm=1-qxm, pxt=1-qxt, pxd=1-qxd) %>%
  arrange(age) %>%
  mutate(pxtot=pxm*pxt*pxd, surv=cumprod(pxtot))
# salaries and benefits
salben <- merit %>% rbind_list(data.frame(age=65, scale=NA)) %>% # need to add a 65th year so we can calc Bx for it
  filter(age %in% 30:65) %>%
  arrange(age) %>%
  mutate(ssea30=scale / scale[age==30] * {(1+infl+prod) ^ (age-30)},
         Sx=cumsum(ssea30), # cumulative salary
         yos.eoy=age - 30 + 1, # years of service at end of year
         n=pmin(fasyears, yos.eoy),
         salprior=ifelse(row_number()<=fasyears, 0, lag(Sx, fasyears)),
         fas=(Sx - salprior) / n,
         Bx=ifelse(row_number()==1, 0, lag(fas) * lag(yos.eoy) * benfactor)) %>%
  mutate(bx=lead(Bx) - Bx,
         bxpct=bx / Bx[age==65] * 100,
         Bxpct=Bx / Bx[age==65] * 100) %>%
  select(-scale)

df <- left_join(decs, salben) %>% 
  mutate(Bx=ifelse(age>65, Bx[age==65], Bx), bx=ifelse(age>64, 0, bx))
# df %>% select(age, pxm, Bx) %>% head(10)

# calculate the scalar a..r (pv at age r of life annuity, paid at boy, p.46), which will be used in subsequent calcs
pvla.r <- df %>% filter(age>=65) %>%
  select(age, pxm) %>%
  arrange(age) %>%
  # calculate pxm65p - the probability, at age 65 and beyond (65-plus, hence 65p), of surviving to a given year
  # since payment is at beginning of year, I start age 65 with prob=1, and then for age 66
  # I use the pxm value for 65 (the lag) - the prob of surviving 1 year from 65
  # to 66, since for age 66 the payment is at beginning of year. And so on. In other words, with payments
  # at beginning of year, I need prob of surviving to beginning, not end, hence use of lagged values
  mutate(pxm65p=cumprod(ifelse(age<=65, 1, lag(pxm))), # since we definitely get to age 65, 65 and prior probs are 1
         pmt= (1 / (1+i)) ^ (age-65) * pxm65p) %>%
  summarise(pvla=sum(pmt)) %>%
  as.numeric
# pvla.r is the value, at age r (65), of receiving a dollar at the beginning of each year, annually for life,
# given the interest rate i and the mortality probabilities pxm


# now we can calculate ptl plan termination liability, for age-30 entrants. For that we need
# pxm65 - the prob of surviving to 65 from any earlier age,
# v^(r-x) where r=65 and x is any age from 30 to 65
# also do pcl - plan continuation liability
df %>% filter(age<=65) %>%
  arrange(desc(age)) %>% 
  mutate(pxm65=cumprod(ifelse(age<65, pxm, 1)),
         pxtot65=cumprod(ifelse(age<65, pxtot, 1)),
         vrx= (1/(1+i))  ^ (65-age),
         ptlx=Bx * pxm65 * vrx * pvla.r,
         ptlxpct=ptlx / ptlx[age==65] * 100,
         pclx=Bx * pxtot65 * vrx * pvla.r,
         pclxpct=pclx / pclx[age==65] * 100) %>%
  select(age, Bx, pxm, pxm65, pxtot, pxtot65, vrx, ptlx, ptlxpct, pclx, pclxpct) %>%
  arrange(age) %>%
  kable(digits=3)
# note that these are close to the values in the book, but not identical
# I don't see anything I am doing wrong, though


# compute ptl, pcl [i.e., Br * a..x for x>=r] for years 65+ - the last column of table 5-1
pvla <- function(dfz, i) {  # present value of life annuity of $1 paid annually for rest of life, beginning of year
  # computed separately for EACH new year, looking only at the (expected) remaining years of life
  getayear <- function(pxm, i) {
    v <- 1 / (1 + i)
    vvalues <- v^(0:(length(pxm)-1))
    pvla <- sum(cumprod(pxm) * vvalues)
    return(pvla)
  }
  # I haven't figured out anything more efficient than looping through the data and getting a new (shorter) vector
  # of survival probabilities for each successive age
  for(row in 1:nrow(dfz)) dfz$pvla[row] <- getayear(c(1, dfz$pxm[row:nrow(dfz)]), i)
  return(dfz)
}

df %>% filter(age>=65) %>%
  select(age, pxm) %>%
  arrange(age) %>%
  do(pvla(., i=.08)) %>%
  mutate(pvlapct=pvla / pvla[age==65] * 100) %>%
  kable(digits=5)
# the pvlapct values match what I computed on http://www.benassist.org/WebForm1.aspx (see my Winkelvoss workbook,
# in the worksheet ptlxcalc),  but they differ from what is in the book on p.72
# possibly due to different mortality table? a different interest rate? Those really can be the only 2 things I think of


# table 5-2 various liability measures ####
# first, build a df with the necessary info - use entry age 30
# repeat what was done before, but cleaned up
infl <- .04 # inflation, used in book
prod <- .01 # productivity, used in book
i <- .08
fasyears <- 5
# decrements
dec5.2 <- select(gam1971, age, qxm) %>%
  left_join(select(term, age, qxt=ea30)) %>% # entry age 30 termination probs
  left_join(select(disb, age, qxd)) %>%
  filter(age>=30)
# salaries and benefits
salben5.2 <- merit %>% rbind_list(data.frame(age=65, scale=NA)) %>% # need to add a 65th year so we can calc Bx for it
  filter(age %in% 30:65) %>%
  arrange(age) %>%
  mutate(ssea30=scale / scale[age==30] * {(1+infl+prod) ^ (age-30)},
         Sx=cumsum(ssea30), # cumulative salary
         yos.eoy=age - 30 + 1, # years of service at end of year
         nfasyears=pmin(fasyears, yos.eoy),
         salprior=ifelse(age < 30 + fasyears, 0, lag(Sx, fasyears)),
         fas=(Sx - salprior) / nfasyears,
         Bx=ifelse(age==30, 0, lag(fas) * lag(yos.eoy) * benfactor)) %>%
  mutate(bx=lead(Bx) - Bx, Sx=ifelse(age>=65, Sx[age==64], Sx)) %>% # we'll need accumulated salary at 65
  select(-scale, -ssea30, -nfasyears, -yos.eoy)

df <- left_join(dec5.2, salben5.2) %>%
  # fill in important missing data
  arrange(age) %>%
  mutate(Bx=ifelse(age>65, Bx[age==65], Bx), 
         bx=ifelse(age>64, 0, bx),
         pxm=1-qxm, pxt=1-qxt, pxd=1-qxd,
         pxtot=ifelse(age<65, pxm*pxt*pxd, pxm)) %>%
  select(age, starts_with("px"), Sx, bx, Bx)
df

# start building table 5-2
glimpse(df)

# do some preliminary calculations that will be needed in multiple places
df2 <- df %>% arrange(desc(age)) %>% # just to be safe
  mutate(pxtot65m=cumprod(ifelse(age>=65, 1, pxtot))) %>% # prob of surviving in service to 65
  arrange(age) %>%
  mutate(pxm65p=cumprod(ifelse(age<=65, 1, lag(pxm))), # since we definitely get to age 65, 65 and prior probs are 1
         pxtot65p=cumprod(ifelse(age<=65, 1, lag(pxtot))),
         vrx=(1/(1+i)) ^ (65-age))
  
# get pvla.r (a..r) - pv at age r of life annuity, paid at boy
pvla.r <- df2 %>% filter(age>=65) %>%
  summarise(pvla=sum(pxm65p * (1/(1+i)) ^ (age-65))) %>%
  as.numeric

# accrued benefit method
glimpse(df2)
df2 %>% filter(age %in% 30:65) %>%
  mutate(AB=Bx * pxtot65m * vrx * pvla.r,
         AB=AB / AB[age==65] * 100) %>%
  kable(digits=2) # this looks to be close enough to the book to be attributable to rounding differences in GAM1971

# benefit prorate or projected unit credit methods PUC
# constant dollar BD and constant percent BP
tla <- function(df, i){ # this is from before - must update
  for(age in 30:63){
    idx=age - 30 + 1 # runs from 1 to 34
    n <- nrow(df) - 1 # always 34
    # get the survival-weighted present value of an annuity paid at the beginning of each year, starting at the current
    # age and continuing through age 64 (recognizing that the annuity will not be paid if the person dies, is disabled, or is terminated)
    probs=c(1, df$pxtot[idx:n])
    vt <- (1/ (1+i)) ^ (0:(64 - age))
    df$tla[idx] <- sum(cumprod(probs) * vt)
  }
  df$tla[35] <- 1
  return(df)
}
yae <- 30 # age of entry
df2 %>% filter(age %in% 30:65) %>%
  mutate(PVFB=Bx[age==65] * pxtot65m * vrx * pvla.r,
         PVFBpct=PVFB / PVFB[age==65] * 100,
         AB=Bx / Bx[age==65] * PVFB, # accrued benefit
         ABpct=AB / AB[age==65] * 100,   # this looks close enough to the book to be attributable to rounding differences in GAM1971
         BD=(age-yae) / (65-yae) * PVFB, 
         BDpct=BD / BD[age==65] * 100, # looks identical to the book
         # we need to shift the accumulated salary, which makes sense (I think); results, with shift, match the book
         Sxshift=ifelse(age==30, 0, lag(Sx)),
         BP=Sxshift / Sxshift[age==65] * PVFB,
         BPpct=BP / BP[age==65] * 100, # looks identical to the book
         # now the cost prorate methods - requires temporary life annuity
         
         # calculate yos share and salary share, for comparison
         yospct=(age-yae) / (65-yae) * 100,
         salpct=Sxshift / Sxshift[age==65] * 100) %>% 
  select(age, ABpct, BPpct, BDpct, PVFBpct, salpct, yospct) %>%
  kable(digits=2)


