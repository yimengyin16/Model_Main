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


















