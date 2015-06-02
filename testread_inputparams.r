

rm(list = ls())

library(knitr)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
#library(corrplot)

# additional djb libraries
library(readxl)
library(stringr)


# source("getParamsYY.R") # allows us to get YY's inputs

# Alternatively, read parameters from a file
rundf <- read_excel("RunControl.xlsx", sheet="RunControl", skip=2) %>% filter(!is.na(runname))
glimpse(rundf)

# what to do with the inputs once we get them?

# temporary - put into global variables ####

# The following code is dangerous and temporary. It simply shows that we could put write over global variables with the values in the df.
# This would allow us to use the info in the df without modifying the main program much at all. That's why I used exactly the same
# names in the Excel file that YY used in the main model.
# A better approach may be to put the parameters for a single run into a list, and then pass the list to the model.
getparms.old <- function(rundf, runname, excludes) {
  # Assign the data from the spreadsheet for a single runname to the global variables. This is dangerous and temporary.
  assignvars <- setdiff(names(rundf), excludes)
  assign1var <- function(x) assign(x, unclass(rundf[which(rundf$runname==runname), x])[[1]], envir = .GlobalEnv)
  sapply(assignvars, assign1var)
}

getparms.old(rundf, "underfunded2", excludes=c("runname", "rundesc", "include", "demogsource"))
# this shows the values as character, but if you check the Environment pane in RStudio you'll see that it has kept
# the numeric variables as numeric






# Better approach: put parameters into a list ####
get_parmsList <- function(rundf, runname) { # don't exclude anything
  # Assign the data from the spreadsheet for a single runname to a list. We'll pass the list to the model.
  runlist <- as.list(rundf[which(rundf$runname==runname), ])
  return(runlist)
}

paramlist <- get_parmsList(rundf, "underfunded2")
str(paramlist)
paramlist
# attach(paramlist)
# detach(paramlist)

assign_parmsList <- function(paramlist, excludes = NULL){
  varNames   <- setdiff(names(paramlist), excludes)
  assign_var <- function(x) assign(x, paramlist[[x]], envir = .GlobalEnv)
  sapply(varNames, assign_var)
}
assign_parmsList(paramlist)




# later we'd have a call to the main program - something such as the following set of commands:
globalparams <- list()

globalparams$nsims <- 10e3
globalparams$nyears <- 100

for(runid in df$runname) {
  paramlist <- getparms.list(rundf, runid)
  resultslist <- runmod(paramlist, globalparams) # obviously we'd have to write the function runmod
  writeresults(resultslist) # we'd have to write the writeresults function
}

# clean up, and then do some analysis of results


#*********************************************************************************************************
# Actives: limiting age and ea to 20:70 ####
#*********************************************************************************************************
# From Email with Don, May 27, 2015

# I revised the pp.prototypes package to keep ea in 20:70, simply by forcing any violations into the lower or upper bound. 
# That can create duplicative cells (for example, a cell that was age=20, ea=18 would become age=20, ea=20,
#  and there may already have been a 20-20 cell so now we would have 2). I collapse any such cells, using the weighted 
#  average salary of the group. I used the function you will find below. Does that seem reasonable to you?


ageea_adj <- function(df){
  # force age and ea for actives to fall in 20:70 by putting any violations into the boundary case
  # and then collapsing any duplicate age-ea combinations, getting weighted average salary
  df2 <- df %>% mutate(age=ifelse(age<20, 20, age),
                       ea=ifelse(ea<20, 20, ea),
                       age=ifelse(age>70, 70, age),
                       ea=ifelse(ea>70, 70, ea)) %>%
    group_by(age, ea) %>%
    summarise(salary=sum(salary*nactives) / sum(nactives), # CAUTION: this must be done BEFORE changing the meaning of nactives
              nactives=sum(nactives))
  return(df2)
}




#*********************************************************************************************************
# Checking Prototypes ####
#*********************************************************************************************************
# From Email with Don, May 27, 2015


# Code for looking at pp.prototypes
# devtools::install_github("donboyd5/pp.prototypes")
library("pp.prototypes")
library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("knitr")

data(package="pp.prototypes")
?actives
?retirees
?salgrowth.hist
?salgrowth.assume

glimpse(actives)
count(actives, planname)
count(actives, age)
count(actives, ea) %>% data.frame

actives %>% filter(planname=="average") %>% select(age, ea, nactives) %>% spread(age, nactives) %>% kable(digits=2)
actives %>% filter(planname=="average") %>% select(age, ea, salary) %>% spread(age, salary) %>% kable(digits=0)
actives %>% filter(planname=="underfunded") %>% select(age, ea, nactives) %>% spread(age, nactives) %>% kable(digits=2)
actives %>% filter(planname=="underfunded") %>% select(age, ea, salary) %>% spread(age, salary) %>% kable(digits=0)


actives %>% filter(planname=="average") %>% select(nactives) %>% sum    # 1000
retirees %>% filter(planname=="average") %>% select(nretirees) %>% sum  # 476

actives %>% filter(planname=="underfunded") %>% select(nactives) %>% sum    # 1000
retirees %>% filter(planname=="underfunded") %>% select(nretirees) %>% sum  # 625


actives %>% group_by(planname) %>%
  summarise(age=sum(age*nactives) / sum(nactives),
            nactives.sum=sum(nactives), # rename so that it does not mess up next calculation
            salary=sum(salary*nactives) / sum(nactives))

tmp <- actives %>% group_by(planname, age) %>%
  summarize(nactives.sum=sum(nactives), salary=sum(nactives*salary) / nactives.sum)
qplot(age, nactives.sum, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, salary, data=tmp, colour=planname, geom=c("point", "line")) # a little odd


glimpse(retirees)
count(retirees, planname)
count(retirees, age)
retirees %>% group_by(planname) %>%
  summarise(age=sum(age*nretirees) / sum(nretirees),
            nretirees.sum=sum(nretirees), # rename so that it does not mess up next calculation
            benefit=sum(benefit*nretirees) / sum(nretirees))
tmp <- retirees %>% group_by(planname, age) %>%
  summarize(nretirees.sum=sum(nretirees), benefit=sum(nretirees*benefit) / nretirees.sum)
qplot(age, nretirees.sum, data=tmp, colour=planname, geom=c("point", "line"))
qplot(age, benefit, data=tmp, colour=planname, geom=c("point", "line")) # a little odd

glimpse(salgrowth.hist)
qplot(age, sscale.hist.rate, data=salgrowth.hist, colour=planname, geom=c("point", "line"))

glimpse(salgrowth.assume)
qplot(age, sscale.assume.rate, data=salgrowth.assume, colour=planname, geom=c("point", "line"))


# Code for working with saved lists:
# Each prototype is a list with 4 data frames. Create 4 data frames, each of which has data for all prototypes.
  
protonames <- c("average", "underfunded")

getproto <- function(proto) readRDS(paste0(draw, paste0(proto, ".rds")))
biglist <- llply(protonames, getproto)

actives <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$actives))
retirees <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$retirees))
salgrowth.hist <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.hist))
salgrowth.assume <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.assume))




















