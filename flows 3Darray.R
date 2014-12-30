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

# library(xlsx)

wvd <- "c:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))



## setting range of age/entry age, and number of years to be simulated.
range_ea  <- seq(20, 60, 5) # For now, assume new entrants only enter the workforce with interval of 5 years. 
range_age <- 20:110 
nyears    <- 100 
 # Age and entry age make a total of 91 * 9 = 819 cells. 


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


## Defining transition matrices ####

# Importing decrement rates

# get decrements - for now make simplifying assumption that single decrements are really multiples ####
qxrdf <- data.frame(age=20:110) %>% mutate(qxr.p=ifelse(age == 64, 1, 0)) # use .p to signify prime, for single decrement

# We need to think carefully how to define the retirment process. Rather than giving a unit prob to 
  # retirment at age 64, here I will define it indirectly, that is, all those who do not become ineligible
  # to retirement(term-nonvested)/impossible to retire(dead) will retire when they turn 65. 
  
# When the only purpose is simulating workforce flow, we do not ditinguish between active-turned retirement,
  # disabled-turned retirement and vested-turned retirement. But when we want to caluculate retirement 
  # benefits, I need to distinguish between them because they will have different benefits. (active-turned and 
  # disabled/vested-turned retiree receive different amount of annuity even thay have the same age and entry age.) 

# make term probs dependent on entry age
term2 <- data.frame(age=20:110) %>% left_join(term) %>% 
  gather(ea, qxt.p, -age) %>% # use .p to signify prime, for single decrement
  mutate(ea=as.numeric(gsub("[^-.0-9]", "", ea)),
         qxt.p=ifelse(is.na(qxt.p), 0, qxt.p))

dtab <- filter(rename(gam1971, qxm.p = qxm), age>=20) %>% # use .p to signify prime, for single decrement
  left_join(rename(dbl, qxmd.p = qxmd)) %>%
  left_join(term2) %>%
  left_join(rename(disb, qxd.p = qxd) ) %>%
  left_join(qxrdf) %>%
  mutate(qxd.p=ifelse(is.na(qxd.p), 0, qxd.p)) %>%
  select(ea, age, everything()) %>%
  group_by(ea) %>%
  arrange(age)

# Since the valid target status vary across the 6 status, the multiple decrement probs vary across status as well. 
# Hence we need to compute status-specific multiple decrement rates. 

dtab %<>% 
  # For active(denoted by ".a"), target status are term-vested, term-non-vested, disabled, retired, dead. 
  # For now, assume 25% terminated workers become vested. 
  mutate(qxtv.a  = qxt.p         * (1 - qxd.p/2) * (1 - qxr.p/2) * (1 - qxm.p/2) * 0.25,
         qxtnv.a = qxt.p         * (1 - qxd.p/2) * (1 - qxr.p/2) * (1 - qxm.p/2) * 0.75,
         qxd.a   = (1 - qxt.p/2) * qxd.p         * (1 - qxr.p/2) * (1 - qxm.p/2),
         qxm.a   = (1 - qxt.p/2) * (1 - qxd.p/2) * (1 - qxr.p/2) * qxm.p, 
         qxr.a   = ifelse(age == 64, 1 - qxm.a  - qxtnv.a, 0),
         # set probs of vested and disabled to 0 at 64, since they are already included in the prob of retirement. 
         # this will be modified later when multiple retirement matrices are added. 
         qxtv.a  = ifelse(age >= 64, 0, qxtv.a),
         qxd.a   = ifelse(age >= 64, 0, qxd.a)) %>%
  # For terminated-vested(".v"), target status are dead and retired. 
  mutate(qxm.v   = (1 - qxr.p/2) * qxm.p, 
         qxr.v   = ifelse(age == 64, 1 - qxm.v, 0)) %>%
  # For terminated-vested(".n"), target status is dead only. 
  mutate(qxm.n   = qxm.p) %>%
  # For disabled(".d"), target status are dead and retired. Note that we need to use the mortality for disabled 
  mutate(qxm.d   = (1 - qxr.p/2) * qxmd.p, 
         qxr.d   = ifelse(age == 64, 1 - qxm.d, 0)) %>%
  # For retired(".r"), the only target status is dead
  mutate(qxm.r   = qxm.p)



make_dmat <- function(qx, df = dtab) {
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx) %>% select(-age) %>% t
  dimnames(df) <- wf_dimnames[c(1,2)]
  return(df)
}
p_active2term_v <- make_dmat("qxtv.a")


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 

# Where do the active go
p_active2term_v  <- make_dmat("qxtv.a")
p_active2term_nv <- make_dmat("qxtnv.a")
p_active2disb    <- make_dmat("qxd.a")
p_active2dead    <- make_dmat("qxm.a")
p_active2retried <- make_dmat("qxr.a")

# Where do the terminated_vested go
p_term_v2dead    <- make_dmat("qxm.v")
p_term_v2retried <- make_dmat("qxr.v")

# Where do the terminated_non-vested go
p_term_nv2dead   <- make_dmat("qxm.n")

# Where do the disabled go
p_disb2retried   <- make_dmat("qxr.d")
p_disb2dead      <- make_dmat("qxm.d")

# Where do the retired go
p_retired2dead   <- make_dmat("qxm.r")


# New entrants in each cell. 
wf_new <- matrix(0, nrow = range_ea, ncol = range_age)



# s matrix

A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)]




# Now the next slice of the array (array[, , i + 1]) is defined

# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyears. 























