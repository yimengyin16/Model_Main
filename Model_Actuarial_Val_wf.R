
# Workforce Simulation by 3D array, for actuarial valuation model
# Yimeng Yin
# 2/3/2015

# This program is adapted from flows 3Darray.R



## preamble ####

library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
# library(corrplot)

wvd <- "E:\\Dropbox (FSHRP)\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))



## Inputs and Initialization ####
# Currently only initial workforce is defined in this section. 
# Need a complete list of initial inputs:
 # Initial workforce
 # Decrement table
 # vesting policy
 # growth of workforce(controlled by growth rate or pre-defined path of growth.)


## A 3D array is created for each of the 6 status:
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


## Setting range of age/entry age, and number of years to be simulated.
 #  These parameters are defined in Model_Actuarial_Val.R
   range_ea    # all possible entry ages  
   range_age   # range of age
   nyear      # number of years in simulation


## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
# dimension 3(depth) represents number of year. 
wf_dim <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, range_age, 1:nyear)

wf_active  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_v  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term_nv <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb    <- array(0, wf_dim, dimnames = wf_dimnames) 
wf_retired <- array(0, wf_dim, dimnames = wf_dimnames)
wf_dead    <- array(0, wf_dim, dimnames = wf_dimnames)



## Setting workforce at time 1
# For convenience, first define a function that can fill cells in a 3D arrary. 

fill_cell <- function(Fill, year, wf){
  # This function fill cells in a workforce array.
  # Input:
  # Fill: numeric with 3 elements or matrix/dataframe with 3 columns. 1: entry age, 2: attained age, 3: number of people 
  # year: numeric, which year in the array to fill
  # wf  : name of the arrary to be filled.
  # Output:
  # wf  : the workforce array filled.  
  
  if(class(Fill) == c("numeric")){
    wf[as.character(Fill[1]), as.character(Fill[2]) , year] = Fill[3]} else {
      for (i in 1:nrow(Fill))
        wf[as.character(Fill[i,1]), as.character(Fill[i,2]) , year] = Fill[i,3]
    }  
  return(wf)
}

# Test the function
#fill_cell(c(50, 65, 30), 1, wf_active)
#fill_cell(matrix(c(20, 25, 100, 30, 64, 80), 2,3, byrow = T), 2, wf_active)
#fill_cell(data.frame(c(20, 30), c(25, 64), c(100, 80)), 2, wf_active)

# Setting an inital distribution of workforce and retirees.


wf_active <- fill_cell(init_active, 1, wf_active)

wf_retired <- fill_cell(init_retired, 1, wf_retired)



## Transition matrices ####

# get decrements
 # Retirement
 # Termination
 # disability
 # mortality
 # mortality for disabled

# Workers retire at 65 with probability 1 without other competing exit rates.  
qxrdf <- data.frame(age=range_age) %>% mutate(qxr.p=ifelse(age == 64, 1, 0)) # use .p to signify prime, for single decrement

# termination probs dependent on entry age
term2 <- data.frame(age=range_age) %>% left_join(term) %>% 
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
  filter(age >= ea) %>%
  group_by(ea) %>%
  arrange(age)


# Since the valid target status vary across the 6 status, the multiple decrement probs vary across status as well. 
# Hence we need to compute status-specific multiple decrement rates. 

dtab %<>% 
  # For active(denoted by ".a"), target status are term-vested, term-non-vested, disabled, retired, dead. 
  # For now, no one will become vested 
  mutate(
#          qxtv.a  = qxt.p         * (1 - qxd.p/2) * (1 - qxr.p/2) * (1 - qxm.p/2) * 0,
#          qxtnv.a = qxt.p         * (1 - qxd.p/2) * (1 - qxr.p/2) * (1 - qxm.p/2) * 1,
#          qxd.a   = (1 - qxt.p/2) * qxd.p         * (1 - qxr.p/2) * (1 - qxm.p/2),
#          qxm.a   = (1 - qxt.p/2) * (1 - qxd.p/2) * (1 - qxr.p/2) * qxm.p, 
#          qxr.a   = ifelse(age == 64, 1 - qxm.a  - qxtnv.a - qxd.a, 0),
         # Correct the code abvoe:
           # Retirement is not a risk competing with other risks(death, terminatin, diability). Rather, it is
           # an event that happens for sure for all participant who have survived all other risks at the beginning of age 65. 
         qxtv.a  = qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2) * 0,
         qxtnv.a = qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2) * 1,
         qxd.a   = (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
         qxm.a   = (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
         qxr.a   = ifelse(age == 64, 1 - qxm.a  - qxtnv.a - qxd.a, 0), 
    
         # set probs of vested to 0 at 64, since they are already included in the prob of retirement. 
         # this will be modified later when multiple retirement matrices are added. 
         #qxd.a   = ifelse(age >= 64, 0, qxd.a),
         qxtv.a  = ifelse(age >= 64, 0, qxtv.a)
         ) %>%
  # For terminated-vested(".v"), target status are dead and retired. 
  mutate(qxm.v   = qxm.p, 
         qxr.v   = ifelse(age == 64, 1 - qxm.v, 0)) %>%
  # For terminated-nonvested(".n"), target status is dead only. 
  mutate(qxm.n   = qxm.p) %>%
  # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
  # Note the difference from the flows 2Darray.R. Disabled can not become retired here. 
  mutate(qxm.d = qxmd.p # for now disabled do not retire and receive benefits
         #qxm.d   = (1 - qxr.p/2) * qxmd.p, 
         #qxr.d   = ifelse(age == 64, 1 - qxm.d, 0)
         ) %>%
  # For retired(".r"), the only target status is dead
  mutate(qxm.r   = qxm.p)


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = dtab) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}
#(p_active2term_nv <- make_dmat("qxtnv.a"))  # test the function


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 

# Where do the active go
p_active2term_v  <- make_dmat("qxtv.a")
p_active2term_nv <- make_dmat("qxtnv.a")
p_active2disb    <- make_dmat("qxd.a")
p_active2dead    <- make_dmat("qxm.a")
p_active2retired <- make_dmat("qxr.a")

# Where do the terminated_vested go
p_term_v2dead    <- make_dmat("qxm.v")
p_term_v2retired <- make_dmat("qxr.v")

# Where do the terminated_non-vested go
p_term_nv2dead   <- make_dmat("qxm.n")

# Where do the disabled go
#p_disb2retired   <- make_dmat("qxr.d")
p_disb2dead      <- make_dmat("qxm.d")

# Where do the retired go
p_retired2dead   <- make_dmat("qxm.r")

# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)


# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 


# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- 20:64
  # age distribution of new entrants
  dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
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
wf1 <- wf_active[, , 1]*(1 - p_active2term_nv)
sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
sum(calc_entrants(wf0, wf1, 0), na.rm = T)


# Now the next slice of the array (array[, , i + 1]) is defined
# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyear. 


wf_growth <- 0.00
a <- proc.time()
for (j in 1:(nyear - 1)){
#i <-  1  
  # compute the inflow to and outflow
  active2term_v  <- wf_active[, , j]*p_active2term_v
  active2term_nv <- wf_active[, , j]*p_active2term_nv
  active2disb    <- wf_active[, , j]*p_active2disb
  active2dead    <- wf_active[, , j]*p_active2dead
  active2retired <- wf_active[, , j]*p_active2retired
  
  # Where do the terminated_vested go
  term_v2dead    <- wf_term_v[, , j]*p_term_v2dead
  term_v2retired <- wf_term_v[, , j]*p_term_v2retired
  
  # Where do the terminated_non-vested go
  term_nv2dead   <- wf_term_nv[, , j]*p_term_nv2dead
  
  # Where do the disabled go
  #disb2retired   <- wf_disb[, , j]*p_disb2retired
  disb2dead      <- wf_disb[, , j]*p_disb2dead
  
  # Where do the retired go
  retired2dead   <- wf_retired[, , j]*p_retired2dead
  
  
  # Total inflow and outflow for each status
  out_active   <- active2term_v + active2term_nv + active2disb + active2retired + active2dead 
  new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, no.entrants = TRUE) # new entrants
  
  out_term_v <- term_v2dead + term_v2retired
  in_term_v  <- active2term_v
  
  out_term_nv <- term_nv2dead
  in_term_nv  <- active2term_nv 
  
  out_disb <- disb2dead  # + disb2retired
  in_disb  <- active2disb
  
  out_retired <- retired2dead
  in_retired  <- active2retired + term_v2retired  # + disb2retired
  
  in_dead <- active2dead + term_v2dead + term_nv2dead + disb2dead + retired2dead
  
  # Calculate workforce for next year. 
  wf_active[, , j + 1]  <- (wf_active[, , j] - out_active) %*% A + new_entrants
  wf_term_v[, , j + 1]  <- (wf_term_v[, , j] + in_term_v - out_term_v) %*% A
  wf_term_nv[, ,j + 1] <- (wf_term_nv[, , j] + in_term_nv - out_term_nv) %*% A
  wf_disb[, ,   j + 1]    <- (wf_disb[, , j] + in_disb - out_disb) %*% A
  wf_retired[, ,j + 1] <- (wf_retired[, , j] + in_retired - out_retired) %*% A
  wf_dead[, ,   j + 1]    <- (wf_dead[, , j] + in_dead) %*% A
}
b <- proc.time()
Time <- b-a 
Time # seems pretty fast





# ## Look at the results ####
# 
# # The workforce arrays
# wf_active
# wf_retired
# wf_term_nv
# wf_term_v
# wf_disb
# wf_dead
# 
# 
# 
# # Test the loop with no new entrants, set no.entrants = TRUE in cal_entrants()
#   
#   apply(wf_active, 3, sum) + 
#   apply(wf_retired, 3, sum) + 
#   apply(wf_term_v, 3, sum) + 
#   apply(wf_term_nv, 3, sum) + 
#   apply(wf_disb, 3, sum) + 
#   apply(wf_dead, 3, sum)
# 
# 
# # summarizing the results
# 
# # popluation by status
# apply(wf_active, 3, sum)  %>%  plot(type = "b") 
# apply(wf_retired, 3, sum) %>%  plot(type = "b")   
# apply(wf_term_v, 3, sum)  %>%  plot(type = "b")  
# apply(wf_term_nv, 3, sum) %>%  plot(type = "b")  
# apply(wf_disb, 3, sum)    %>%  plot(type = "b")  
# apply(wf_dead, 3, sum)    %>%  plot(type = "b")  
# 
# 
# # Total population
# (apply(wf_active, 3, sum) + 
#    apply(wf_retired, 3, sum) + 
#    apply(wf_term_v, 3, sum) + 
#    apply(wf_term_nv, 3, sum) + 
#    apply(wf_disb, 3, sum)) %>% plot(type = "b")
# 
# # Look at workforce by entry age
# apply(wf_active, c(1,3), sum)
# 
# # Look at workforce by age
# # options(digits = 4, scipen = 99)
# apply(wf_active, c(2,3), sum)
# # Potential problem, values for age over 65 are not exact 0s, although may be computationally equivalent to 0s.
# 
# 
















