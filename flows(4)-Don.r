# Don Boyd
# flows(#).r
# 12/28/2014

# first part shows one way of calculating flows (changes in status of retirement system members) over time, using
# a data frame
# it does not (yet) add new hires

# second part examines speed and memory issues that could arise when we make inflation stochastic; these issues arise because
# then each simulation will have its own path for salary and benefits, and we will have to calculate and store that information
# in each of 10k sims; the best approach I came up with below (approach #4) should work with 100 "cells" (unique age, entry age, salary cells) and
# probably 1,000 cells; after that I think problems will get pretty large and we may need alternative solutions

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
library(tidyr) # gather, spread
library(xlsx)

wvd <- "E:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))


# Part 1: member flows ####

# create a simple initial workforce
# cellid indicates a unique combination of age, entry age (ea), and salary; we could have hundreds or even (perhaps?) a thousand
# of these at some point, if we have new hires entering at many ages
wf1 <- data.frame(cellid=1:4, status="active", num=c(100, 200, 150, 100), age=c(20, 30, 35, 40),
                  ea=c(20, 20, 30, 30), salary=c(40, 60, 62, 78) * 1000)
wf1


# get decrements - for now make simplifying assumption that single decrements are really multiples ####
qxrdf <- data.frame(age=20:110) %>% mutate(qxr.p=ifelse(age<65, 0, 1)) # use .p to signify prime, for single decrement

# make term probs dependent on entry age
term2 <- data.frame(age=20:110) %>% left_join(term) %>% 
  gather(ea, qxt.p, -age) %>% # use .p to signify prime, for single decrement
  mutate(ea=as.numeric(gsub("[^-.0-9]", "", ea)),
         qxt.p=ifelse(is.na(qxt.p), 0, qxt.p))

dtab <- filter(gam1971, age>=20) %>% rename(qxm.p=qxm) %>% # use .p to signify prime, for single decrement
  left_join(term2) %>%
  left_join(rename(disb, qxd.p=qxd)) %>%
  left_join(qxrdf) %>%
  mutate(qxd.p=ifelse(is.na(qxd.p), 0, qxd.p)) %>%
  select(ea, age, everything()) %>%
  group_by(ea) %>%
  arrange(age)


# create a full data.frame to fill out, starting at the first age we have for each person
wf2 <- wf1 %>% mutate(active=num, term.vest=0, term.notv=0, disb=0, retired=0, dead=0, year=1) %>%
  select(-status, -salary)

nyears <- 100
base <- expand.grid(year=1:nyears, cellid=1:4) %>% 
  left_join(wf2) %>%
  group_by(cellid) %>% 
  mutate(age = age[year==1] + year - 1, ea=ea[year==1])
base
# add the single decrements to this file
base2 <- left_join(base, select(dtab, age, ea, contains(".p"))) # hmmm...should we do anything about NA qx's for age>110?

# pretend for convenience here that single decrements are really multiple decrements
memflows <- function(df){ # member flows
  if(nrow(df)==1) return(df)
  # this assumes it receives a data frame sorted in year order, with no missing years
  # initialize temporary variables
  statuses <- c("active", "term.vest", "term.notv", "disb", "retired", "dead")
  fromto <- matrix(data=0, nrow=6, ncol=6, dimnames=list(statuses, statuses)) # from-rows to-cols
  
  fillmat <- function(fromto, i){ # keep this function inside wfflows, as it is not needed elsewhere
    # fill out the fromto matrix for next year
    fromto[, ] <- 0 # reinitialize, so that any elements we don't fill explicitly will be zero
    # for now, treat the single decrements as if they were appropriate multiple decrements
    # where do the actives go?
    fromto["active", "term.vest"] <- c(df$active[i] * df$qxt.p[i]*.25)
    fromto["active", "term.notv"] <- c(df$active[i] * df$qxt.p[i]*.75)
    fromto["active", "disb"] <- c(df$active[i] * df$qxd.p[i])
    fromto["active", "retired"] <- c(df$active[i] * df$qxr.p[i])
    fromto["active", "dead"] <- c(df$active[i] * df$qxm.p[i])
    # where do the term-vesteds go?
    fromto["term.vest", "retired"] <- c(df$term.vest[i] * df$qxr.p[i])
    fromto["term.vest", "dead"] <- c(df$term.vest[i] * df$qxm.p[i])      
    # where do the term-notvesteds go?
    fromto["term.notv", "dead"] <- c(df$term.notv[i] * df$qxm.p[i])  
    # where do the disabled go?
    fromto["disb", "retired"] <- c(df$disb[i] * df$qxr.p[i])
    fromto["disb", "dead"] <- c(df$disb[i] * df$qxm.p[i])      
    # where do the retired go?
    fromto["retired", "dead"] <- c(df$retired[i] * df$qxm.p[i])
    return(fromto)
  } # end fillmat
  
  fromto <- fillmat(fromto, 1) # initialize with year1 end-year values, so that we are primed for year 2
  
  # in each new year, calculate status changes and store in the fromto matrix
  # I hate looping in R but that seems unavoidable in this case. Member databases for deterministic runs will be small enough that it
  # will not be a speed issue, I think.
    for(i in 2:nrow(df)){ # start with year 2
      # adjust for activity that occurred at end of last year, and then fill the fromto matrix for next year    
      df$active[i] <- df$active[i-1] - sum(fromto["active", ]) + sum(fromto[, "active"]) # subtract outflows, add inflows (zero for actives)
      df$term.vest[i] <- df$term.vest[i-1] - sum(fromto["term.vest", ]) + sum(fromto[, "term.vest"])
      df$term.notv[i] <- df$term.notv[i-1] - sum(fromto["term.notv", ]) + sum(fromto[, "term.notv"])
      df$disb[i] <- df$disb[i-1] - sum(fromto["disb", ]) + sum(fromto[, "disb"])
      df$retired[i] <- df$retired[i-1] - sum(fromto["retired", ]) + sum(fromto[, "retired"])
      df$dead[i] <- df$dead[i-1] - sum(fromto["dead", ]) + sum(fromto[, "dead"]) # of course there are no outflows from dead so if speed slows, drop them
      df$num[i] <- df$active[i] + df$term.vest[i] + df$term.notv[i] + df$disb[i] + df$retired[i] + df$dead[i] # a check on our work
      
      fromto <- fillmat(fromto, i) # get ready for next year     
    } # end for
  return(df)
} # end memflows



a <- proc.time()
tmp <- base2 %>% group_by(cellid) %>% arrange(year) %>%
  do(memflows(.))
b <- proc.time()
b - a # not a speed problem as long as member flows are deterministic; if we had to do 10k times, 1 for each sim, might be slow

tmp
  
  
# Part 2: speed and memory testing to see what would happen if we base salary growth on stochastic inflation ####  
# how much would we slow things down if we had a row for every sim with 100 different cells (worker types)? (but we could have 1000 or more)

# approach #1 ####
base2 <- expand.grid(simid=1:10e3, cellid=1:100, age=20:120)  # 10k x 100 x 101 = 101m cells
base2$salary[base2$age==20] <- 40e3
a <- proc.time()
base2 <- base2 %>% group_by(cellid, simid) %>%
  arrange(age) %>%
  mutate(salary=salary[1]*(1.06^(age-19)), Sx=cumsum(salary)) # in a real simulation, the salary growth rate would vary from sim to sim
b <- proc.time() # 5 mins
b - a
head(base2)
memory() # 6gb - this will easily use too much memory if we build a lot of these data structures - and we might easily have
# 1000 cells rather than 100 cells
# but if we only build one or two, should be fine - for example, if we only have salary and benefits that are dependent upon stochastic inflation
# and we then summarize payroll and benefits by year for purposes of cash flow
# slice(base2) # time consuming, don't use
system.time(print(base2[base2$simid==19 & base2$cellid==37 & base2$age<30, ])) # 13 secs
# what if we wanted to get the total payroll and benefits by year for each sim, to put that inside the stochastic loop?
# for now act as if getting a summary by age is the same as getting it by year
nrow(base2)/1e6 # 101m rows
a <- proc.time()
payroll <- base2 %>% group_by(simid, age) %>%
  summarise(payroll=sum(salary)/1e6)
b <- proc.time()
b - a # 14 secs
nrow(payroll)/1e6 # only 1m rows needed for this
# this could be feasible, if all we need is total payroll and benefits by year, with inflation stochastic


# conclusions:
# - WHEN we make salary and benefits dependent upon stochastic inflation we will not be able to fit this
# into an 8gb machine using approach #1
# - IF we need to maintain all information in memory at one
# - consider alternatives within R
# - and also alternatives outside of R

# approach #2 ####
# next, consider looping - this probably will be too slow, but it won't use much memory
base3 <- expand.grid(cellid=1:100, age=20:120)
base3$salary[base3$age==20] <- 40e3
a <- proc.time()
for(sim in 1:10e3){
  base3 <- base3 %>% group_by(cellid) %>%
    arrange(age) %>%
    mutate(salary=salary[1]*(1.06^(age-19)), Sx=cumsum(salary))  
}
b <- proc.time()
b - a # 50 seconds - not bad
# in this approach, we would need to do each sim individually, and then write its results
# would it be faster with matrices?


# what if we have: 10k sims, 100 years, 1k cells (e.g., age, entryage, salary combos), and 5 items to track (salary, benefit, yos, etc)
# that's 10k x 100 x 1k x 5 items: =500m items; if it takes 4 bytes per item, that's 2gb, plus R usually makes multiple copies

# the basic problem is that (a) each sim iteration is independent, so we can minimize memory usage if we do a sim iteration,
# store results, move on to the next sim iteration, etc., but (b) R is not efficient if we loop through 10000 sims this way


# approach #3 ####
# what if we do this the way I did my stochastic sim framework, so that we do all 10k sims at once, but only keep 5 years at a time in memory?
# set up the data frame
ncells <- 100
nsims <- 10e3
nyears <- 100
base4 <- expand.grid(cellid=1:ncells, simid=1:nsims) # only 1m recs -- has 5 ages at a time - we'd just have to keep track of them
base4$ea <- 20 # entry age might be different in each cell but for now make them the same for all
base4$esalary <- 40e3 # entry salary
base4$sal1 <- base4$esalary
ht(base4)
salgrow <- as.data.frame(matrix(rnorm(nsims*nyears, mean=.06, sd=.02), nrow=nsims, ncol=nyears))
salgrow$simid <- 1:nsims
ht(salgrow)
idx <- match(base4$simid, salgrow$simid)
base4$sal2 <- base4$sal1*salgrow[idx, 1]
base4$sal3 <- base4$sal2*salgrow[idx, 2]
base4$sal4 <- base4$sal3*salgrow[idx, 3]
base4$sal5 <- base4$sal4*salgrow[idx, 4]
sal14 <- paste0("sal", 1:4)
sal25 <- paste0("sal", 2:5)
a <- proc.time()
for(year in 1:100){ # sort of like doing age 20:120
  base4[, sal14] <- base4[, sal25]
  base4$sal5 <- base4$sal4*salgrow[idx, year] # in a real simulation we would have a vector of sal growths that vary randomly by simid
  payroll <- base4 %>% group_by(simid) %>% summarise(sal1=sum(sal1, na.rm=TRUE)) # get the unique payroll for each sim
}
b <- proc.time() # 10 secs with 100 cells, so this is very fast; scales well to 1000 cells -- 88 secs
b - a
head(base4)
head(base4[, sal14])
head(base4[, sal25])

# approach #4 ####
# let's try with a column for each year - will this be a memory hog?
ncells <- 100
nsims <- 10e3
nyears <- 100
sal <- expand.grid(cellid=1:ncells, simid=1:nsims) # only 1m recs -- has 5 ages at a time - we'd just have to keep track of them
sal$ea <- 20 # entry age might be different in each cell but for now make them the same for all
sal$esalary <- 40e3 # entry salary
# add empty columns to the dataframe
scols <- as.data.frame(matrix(nrow=ncells*nsims, ncol=nyears)) # this is slow - mayber there's a faster way to add empty cols to a df?
names(scols) <- paste0("sal", 1:nyears)
ht(scols)
sal <- cbind(sal, scols)
rm(scols) # try to keep memory under control
sal$sal1 <- sal$esalary
ht(sal)
salgrow <- as.data.frame(matrix(rnorm(nsims*nyears, mean=.06, sd=.02), nrow=nsims, ncol=nyears))
salgrow$simid <- 1:nsims
ht(salgrow)
idx <- match(sal$simid, salgrow$simid)
a <- proc.time()
for(year in 2:100){ # sort of like doing age 20:120
  sal[, paste0("sal", year)] <- sal[, paste0("sal", year-1)]*(1+salgrow[idx, year])
}
b <- proc.time() # 1.5 secs with 100 cells, so this is very fast; scales well to 1000 cells -- 16 secs
b - a
head(sal)

# now get payroll for each year for each simulation - about 5 secs with 100 cells, 30 secs with 1000 cells
system.time(payroll <- sal %>% group_by(simid) %>% summarise_each(funs(sum(., na.rm=TRUE)/1e6), starts_with("sal")))
head(payroll)
glimpse(payroll)
memory()
# memory usage for sal: 785mb with 100 cells, 7.9gb with 1000 cells, so it scales linearly




