

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
rundf <- read_excel("./Data/RunControl.xlsx", sheet="RunControl", skip=2) %>% filter(!is.na(runname))
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
getparms.list <- function(rundf, runname) { # don't exclude anything
  # Assign the data from the spreadsheet for a single runname to a list. We'll pass the list to the model.
  runlist <- as.list(rundf[which(rundf$runname==runname), ])
  return(runlist)
}

paramlist <- getparms.list(rundf, "underfunded2")
str(paramlist)
paramlist



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





