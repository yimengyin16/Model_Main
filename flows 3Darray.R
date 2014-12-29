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

wvd <- "c:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))


## setting range of age/entry age, and number of years to be simulated.
range_ea  <- 20:64
range_age <- 20:110 
nyears    <- 100 

## A 3D array is created for each of the 6 status: 
#  (1)Active
#  (2)Terminated, vested
#  (3)Terminated, non-vested
#  (4)Disabled
#  (5)Retired
#  (6)Dead

# The transition process between status are illustrated in 
# https://www.lucidchart.com/documents/edit/8b99d535-261b-4982-9723-5cbae5f5e86e

## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
#  dimension 3(depth) represents number of year. 
wf_dim <- c(length(range_ea), length(range_age), nyears)
wf_dimnames <- list(range_ea, range_age, 1:nyears)

wf_active  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_v  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_vn <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb    <- array(0, wf_dim, dimnames = wf_dimnames)
wf_retired <- array(0, wf_dim, dimnames = wf_dimnames)
wf_dead    <- array(0, wf_dim, dimnames = wf_dimnames)

# In each iteration we work with a slice of the array wf_XXX[, , i], which is a matrix
class(wf_active[, , 1])

# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Where do the active go
p_active2term_v  <-
p_active2term_nv <- 
p_active2disb    <- 
p_active2retried <- 
p_active2dead    <- matrix(0, nrow = range_ea, ncol = range_age)

# Where do the terminated_vested go
p_term_v2retried <- 
p_term_v2dead    <- matrix(0, nrow = range_ea, ncol = range_age)

# Where do the terminated_non-vested go
p_term_nv2dead    <- matrix(0, nrow = range_ea, ncol = range_age)

# Where do the disabled go
p_disb2retried <- 
p_disb2dead    <- matrix(0, nrow = range_ea, ncol = range_age)

# Where do the retired go
p_retired2dead    <- matrix(0, nrow = range_ea, ncol = range_age)


# New entrants in each cell. 
wf_new <- matrix(0, nrow = range_ea, ncol = range_age)



# Moving matrix

M <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)]




# Now the next slice of the array (array[, , i + 1]) is defined

# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% M + wf_new[, , i + 1]
# i runs from 2 to nyears. 























