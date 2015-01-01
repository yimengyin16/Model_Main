# Workflow by 3D array
# Yimeng Yin
# 12/29/2014


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
# library(xlsx)

wvd <- "E:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))


## setting range of age/entry age, and number of years to be simulated.
range_ea  <- seq(20, 60, 5) # For now, assume new entrants only enter the workforce with interval of 5 years. 
range_age <- 20:110 
nyears    <- 100 
 # Age and entry age make a total of 91 * 9 = 819 cells. 


## A 3D array is created for each of the 6 status: ####
#  (1)Active
#  (2)Terminated, vested
#  (3)Terminated, non-vested
#  (4)Disabled
#  (5)Retired
#  (6)Dead
 
 # Retired can be further divided into 3 types by source this feature will be add in future versions
  #  (5)-1 Retired from active 
  #  (5)-2 Retired from teriminated, vested
  #  (5)-3 Retired from disabled

## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
   # dimension 3(depth) represents number of year. 
wf_dim <- c(length(range_ea), length(range_age), nyears)
wf_dimnames <- list(range_ea, range_age, 1:nyears)

wf_active  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_v  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_nv <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb    <- array(0, wf_dim, dimnames = wf_dimnames)
wf_retired <- array(0, wf_dim, dimnames = wf_dimnames)
wf_dead    <- array(0, wf_dim, dimnames = wf_dimnames)

# In each iteration we work with a slice of the array wf_XXX[, , i], which is a matrix
class(wf_active[, , 1])
dim(wf_active[, , 1]) # 9 entry ages and 91 ages, 819 cells

# idea: we can even combine the 6 arrays above into a 4D array, where the 4th dimension represents the status. 
  # While this may make the the code slightly less straitforward, it can make the code neater and the summarization 
  # easier.
  # eg. extract the active: wf[, , , "active"]
  # eg. get the total population over time: apply(wf, 3, sum)



## Defining transition matrices ####

# The transition process between status are illustrated in 
  # https://www.lucidchart.com/documents/edit/8b99d535-261b-4982-9723-5cbae5f5e86e

# Importing decrement rates

# Notes
# 1. We need to think carefully how to define the retirment process. Here I will define it indirectly, that is, 
# all those who do not become ineligible to retirement(term-nonvested)/impossible to retire(dead) will retire
# when they turn 65. Doing this will ensure all active workers who become disabled or terminated-vested during 
# age 64 will enter the status Retired when they turn 65, rather than entering the pool disabled or terminated-vested
# when they turn 65. Without doing so (see the the excel file Winkelvoss(13).xlsx shee flow(2)) will cause 
# the status terminated-vested and disabled still have positive number of members in them after age 65, while 
# all of them are supposed to retire at age 65. 

# 2. When the only purpose is simulating workforce flow, we do not ditinguish between active-turned retirement,
# disabled-turned retirement and vested-turned retirement. But when we want to caluculate retirement 
# benefits, I need to distinguish between them because they will have different benefits. (active-turned and 
# disabled/vested-turned retiree receive different amount of annuity even thay have the same age and entry age.) 


# get decrements - for now make simplifying assumption that single decrements are really multiples ####
qxrdf <- data.frame(age=20:110) %>% mutate(qxr.p=ifelse(age == 64, 1, 0)) # use .p to signify prime, for single decrement

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


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = dtab) {
  # inputs:
   # qx: character, name of the transition probability to be created.
   # df: data frame, decrement table.
  # returns:
   # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}
p_active2term_v <- make_dmat("qxtv.a")  # test the function


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

# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)


# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
  # A square matrix with the dimension length(range_age)
  # created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 
corrplot(A, is.corr = F)


# Initialize workforce 
 # case 1: Initial workforece only have workers at eath entry age
wf_active[, as.character(range_ea), 1] <- diag(seq(1000, by  = -100, length = 9))
wf_active[, , 1] # check


# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, no.entrants = FALSE){
 # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
   # growth rate. 
 # inputs:
    # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
    # wf1: a matrix of workforce after decrement.  
    # delta: 
 # returns:
    # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
      # and 0 in all other cells. 
  
  # working age
  working_age <- 20:64
  # age distribution of new entrants
  dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)])
  size1 <- sum(wf1[,as.character(working_age)])
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               #  vector, number of new entrants by age

  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  }

}

# test the function 
wf0 <- wf_active[, , 1]
wf1 <- wf_active[, , 1]*(1 - p_active2term_v)
sum(wf0) - sum(wf1)
sum(calc_entrants(wf0, wf1, 0))


# Now the next slice of the array (array[, , i + 1]) is defined

# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyears. 


a <- proc.time()
for (i in 1:(nyears - 1)){

# compute the inflow to and outflow
active2term_v  <- wf_active[, , i]*p_active2term_v
active2term_nv <- wf_active[, , i]*p_active2term_nv
active2disb    <- wf_active[, , i]*p_active2disb
active2dead    <- wf_active[, , i]*p_active2dead
active2retried <- wf_active[, , i]*p_active2retried

# Where do the terminated_vested go
term_v2dead    <- wf_term_v[, , i]*p_term_v2dead
term_v2retried <- wf_term_v[, , i]*p_term_v2retried

# Where do the terminated_non-vested go
term_nv2dead   <- wf_term_nv[, , i]*p_term_nv2dead

# Where do the disabled go
disb2retried   <- wf_disb[, , i]*p_disb2retried
disb2dead      <- wf_disb[, , i]*p_disb2dead

# Where do the retired go
retired2dead   <- wf_retired[, , i]*p_retired2dead


# Total inflow and outflow for each status
out_active   <- active2term_v + active2term_nv + active2disb + active2retried + active2dead 
new_entrants <- calc_entrants(wf_active[, , 1], wf_active[, , 1] - out_active, 0.01, no.entrants = FALSE) # new entrants

out_term_v <- term_v2dead + term_v2retried
in_term_v  <- active2term_v

out_term_nv <-term_nv2dead
in_term_nv  <-active2term_nv 

out_disb <- disb2dead + disb2retried
in_disb  <- active2disb

out_retired <- retired2dead
in_retired  <- active2retried + term_v2retried + disb2retried

in_dead <- active2dead + term_v2dead + term_nv2dead + disb2dead + retired2dead

# Calculate workforce for next year. 
wf_active[, , i + 1]  <- (wf_active[, , i] - out_active) %*% A + new_entrants
wf_term_v[, , i + 1]  <- (wf_term_v[, , i] + in_term_v - out_term_v) %*% A
wf_term_nv[, , i + 1] <- (wf_term_nv[, , i] + in_term_nv - out_term_nv) %*% A
wf_disb[, , i + 1]    <- (wf_disb[, , i] + in_disb - out_disb) %*% A
wf_retired[, , i + 1] <- (wf_retired[, , i] + in_retired - out_retired) %*% A
wf_dead[, , i + 1]    <- (wf_dead[, , i] + in_dead) %*% A
}
b <- proc.time()
Time <- b-a 
Time # seems pretty fast



# Test the loop with no new entrants, set no.entrants = TRUE in cal_entrants()
  apply(wf_active, 3, sum) + 
  apply(wf_retired, 3, sum) + 
  apply(wf_term_v, 3, sum) + 
  apply(wf_term_nv, 3, sum) + 
  apply(wf_disb, 3, sum) + 
  apply(wf_dead, 3, sum)


# summarizing the results

# popluation by status
apply(wf_active, 3, sum)  %>%  plot(type = "b") 
apply(wf_retired, 3, sum) %>%  plot(type = "b")   
apply(wf_term_v, 3, sum)  %>%  plot(type = "b")  
apply(wf_term_nv, 3, sum) %>%  plot(type = "b")  
apply(wf_disb, 3, sum)    %>%  plot(type = "b")  
apply(wf_dead, 3, sum)    %>%  plot(type = "b")  


# Total population
 (apply(wf_active, 3, sum) + 
  apply(wf_retired, 3, sum) + 
  apply(wf_term_v, 3, sum) + 
  apply(wf_term_nv, 3, sum) + 
  apply(wf_disb, 3, sum)) %>% plot(type = "b")


apply(wf_active, c(1,3), sum)





j <- 50
corrplot(wf_active[, , j], is.corr = F)
corrplot(wf_retired[, , j], is.corr = F)
corrplot(wf_dead[, , j], is.corr = F)
corrplot(wf_term_v[, , j], is.corr = F)
corrplot(wf_term_nv[, , j], is.corr = F)
corrplot(wf_disb[, , j], is.corr = F)




















