# LJAF white papers(#).r
# Don Boyd
# 5/2/2014


# Start with the costs white paper

# Workforce movement (p.2) ####

# initial workforce wf0 age of entry x age current at time t  [ae x at -- rows, columns] -- upper triangular, ae <= at
# k x k, where k=az - a0 + 1
# the cells are the number of employees who entered at a given age, and their current age
# the diagonal represents entry age distribution of new hires

a0 <- 20 # youngest age of entry
az <- 25 # last possible year of working
k <- az - a0 + 1 # number of rows and columns in the wf matrix (number of years); one version of the paper forgot to add 1
k


wf0 <- matrix(nrow=k, ncol=k, dimnames=list(a0:az, a0:az)) # initial workforce
# make up some data for number of employees - each cell gives number of workers by age at entry (rows) and age current (columns)
# a worker's current age can't be less than age at entry so the lower left triangle is empty
# start by creating the workers upper triangle (represented as a vector)
workersut <- c(100, 90, 80, 70, 60, 50,
                    80, 70, 65, 55, 40,
                        60, 50, 30, 15,
                            30, 28, 26,
                                25, 24,
                                    12)
wf0[upper.tri(wf0, diag=TRUE)] <- workersut # fill in matrix
wf0[lower.tri(wf0)] <- 0




# create S, the separation-probabilities or transitions matrix -- k x k
# start simply, with a separation vector
sv <- seq(.05, 1, length=k) # separations must be 100% in final year
sv
# now create age-entry by age-current matrix
# simplification: give everyone the same separation rate at a given attained age regardless of age of entry
s <- matrix(rep(sv), nrow=k, ncol=k, byrow = TRUE)
s[lower.tri(s)] <- 0
s

# get w-prime -- net of separations but before aging (see p.3 of the benefits paper)
wp <- (1 - s) * wf0 # Hadamard product - element-by-element multiplication - is just multiplication in R
wp


## aging of the workorce

# compute total workforce
n <- sum(wf0)
m <- sum(wp)

# compute the number of new entrants at each age.

delta <- 0         # workforce growth rate
l <- n*(1 + delta) # size of the workforce next year
p <- l - m         # number of workers need to hire

e <- 1/k * rep(1,k) # distribution of new entrans
names(e) <- a0:az

ne <- e*p     # k vector, number of new entrants by age

NE <- diag(ne) # place ne on the diagnal of a matrix


# Define the shift matrix A, which shifts wp by 1 element to the right(aging by 1 year)
A <- diag(k+1)[-1, -(k+1)]

wpp <- wp %*% A # aged workforce


## Creating new workforce matrix wf1

wf1 <- NE + wpp
wf1


## Defining a workforce updating function
wf0
s
delta
e
# underlying parameters: a0, az, sv

update_wf <- function(wf0, s, delta, e){
  # Inputs: 
   # 1. current workforce matrix, wf0, k x k matrix
   # 2. seperation matrix, s, k x k matrix
   # 3. workforce growth rate, delta, numeric
   # 4. distribution vector of new entrants, e, k numeric vector
  
  # output: updated workforce matrix# seperation
  
  wp <- wf0 * (1 - s)
  
  # compute the size of workforce before and after separation
  n <- sum(wf0)
  m <- sum(wp)
  
  # computing new entrants
  
  l <- n*(1 + delta) # size of the workforce next year
  p <- l - m         # number of workers need to hire
  ne <- e*p          # k vector, number of new entrants by age
  NE <- diag(ne)     # place ne on the diagnal of a matrix
  
  # aging current workforce
  A <- diag(k+1)[-1, -(k+1)]
  wpp <- wp %*% A # aged w
  
  # generate new workforce
  wf1 <- wpp + NE
  
  # return result
  return(wf1)
}
     
update_wf(wf0, s, delta, e)


## Creating a path of workforce

# The workforce data in each period are stored in the 1st and 2nd dimension (k x k) of the array.
# 3rd dimension represents the time period
wf_sim <- array(dim = c(k, k, 30))
wf_sim[, , 1] <- wf0

# Simulate the workforce evolution using a loop
for(i in 2:30)wf_sim[, , i] <- update_wf(wf_sim[, , i-1], s, delta, e)


apply(wf_sim, c(1,3), sum)


# 



# Pension Benefits








