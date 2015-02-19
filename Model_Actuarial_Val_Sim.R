## Actuarial Valuation


# Now we do the actuarial valuation at period 1 and 2. 
# In each period, following values will be caculated:
# AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
# NC: Normal Cost  
# AA: Value of assets.
# UAAL: Unfunded accrued actuarial liability, defined as AL - NC
# EUAAL:Expected UAAL. 
# LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
# i is assumed interest rate. ELs of each period will be amortized seperately.  
# SC: Supplement cost 
# C : Actual contribution, assume that C(t) = NC(t) + SC(t)
# B : Total beneift Payment   
# Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
# Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
# Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
# I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
# S : Total payrol
# Funded Ratio: AA / AL

# Formulas
# AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
# AA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
# I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
# Ia(t) = i * AA(t)
# Ib(t) = i * B(t)
# Ic(t) = i * C(t)
# EI(t) = Ia(t) - Ib(t) + Ic(t) 
# C(t) = NC(t) + SC(t)
# UAAL(t) = AL(t) - AA(t)
# EUAAL(t) = [UAAL(t-1) + NC(t-1)](1+i(t-1)) - C(t-1) - Ic(t-1)
# LG(t) =   UAAL(t) - EUAAL for t>=2 ; LG(1) = -UAAL(1) (LG(1) may be incorrect, need to check)
# More on LG(t): When LG(t) is calculated, the value will be amortized thourgh m years. This stream of amortized values(a m vector) will be 
# placed in SC_amort[t, t + m - 1]
# SC = sum(SC_amort[,t])
# ExF = B(j) - C(j)

# About gains and losses
# In this program, the only source of gain or loss is the difference between assumed interest rate i and real rate of return i.r,
# which will make I(t) != Ia(t) + Ic(t) - Ib(t)



# Set up data frame
penSim0 <- data.frame(year = 1:nyear) %>%
  mutate(AL   = 0, #
         AA   = 0, #
         FR   = 0, #
         ExF  = 0, # 
         UAAL = 0, #
         EUAAL= 0, #
         LG   = 0, #
         NC   = 0, #
         SC   = 0, #
         C    = 0, #
         B    = 0, #                        
         I.r  = 0, #                        
         I.e  = 0, #
         Ia   = 0, #                         
         Ib   = 0, #                         
         Ic   = 0, #  
         i    = i,
         i.r  = 0)

# matrix representation of amortization: better visualization but large size, used in this excercise
SC_amort0 <- matrix(0, nyear + m, nyear + m)
#SC_amort0
# data frame representation of amortization: much smaller size, can be used in real model later.
#SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))


cl <- makeCluster(ncore) 
registerDoParallel(cl)

start_time_loop <- proc.time()

#penSim_results <- list()
#for(k in 1:nsim){

penSim_results <- foreach(k = 1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
  # k <- 1
  # initialize
  penSim <- penSim0
  SC_amort <- SC_amort0 
  penSim[,"i.r"] <- i.r[, k]
  
  for (j.year in 1:nyear){
    # j.year <- 1
    # AL(j)
    
    penSim[j.year, "AL"] <- sum(wf_active[, , j.year] * liab_list[[paste0("ALx.", actuarial_method)]][[j.year]]) + 
      sum(wf_retired[, , j.year] * liab_list[["ALx.r"]][[j.year]])
    # NC(j)
    penSim[j.year, "NC"] <- sum(wf_active[, , j.year] * liab_list[[paste0("NCx.", actuarial_method)]][[j.year]]) 
    
    # B(j)
    penSim[j.year, "B"] <-  sum(wf_retired[, , j.year] * liab_list[["B"]][[j.year]])
    
    # for testing purpose
    # penSim[penSim$year == j.year, "B"] <-  sum(extract_slice("B",j.year))
    # penSim[penSim$year == j.year, "B"] <-  sum(wf_retired[, , j.year])
    
    
    # AA(j)  
    if(j.year == 1) penSim[j.year, "AA"] <- switch(init_AA,
                                                   AA0 = AA_0,                           # Use preset value
                                                   AL0 = penSim[j.year, "AL"]) # Assume inital fund equals inital liability. 
    if(j.year > 1)  penSim[j.year, "AA"] <- with(penSim, AA[j.year - 1] + I.r[j.year - 1] + C[j.year - 1] - B[j.year - 1])
    
    # UAAL(j)
    penSim$UAAL[j.year] <- with(penSim, AL[j.year] - AA[j.year]) 
    
    # LG(j)
    if (j.year == 1){
      penSim$EUAAL[j.year] <- 0
      penSim$LG[j.year] <- with(penSim,  UAAL[j.year])
    }
    if (j.year > 1){
      penSim$EUAAL[j.year] <- with(penSim, (UAAL[j.year - 1] + NC[j.year - 1])*(1 + i[j.year - 1]) - C[j.year - 1] - Ic[j.year - 1])
      penSim$LG[j.year] <- with(penSim,  UAAL[j.year] - EUAAL[j.year])
    }   
    
    # Amortize LG(j)
    SC_amort[j.year, j.year:(j.year + m - 1)] <- amort_LG(penSim$LG[penSim$year == j.year], i, m, g, end = FALSE, method = amort_method)  
    
    # Supplemental cost in j.year
    penSim$SC[j.year] <- sum(SC_amort[, j.year])
    
    
    # C(j)
    penSim$C[j.year] <- with(penSim, NC[j.year] + SC[j.year]) 
    
    # Ia(j), Ib(j), Ic(j)
    penSim$Ia[j.year] <- with(penSim, AA[j.year] * i[j.year])
    penSim$Ib[j.year] <- with(penSim,  B[j.year] * i[j.year])
    penSim$Ic[j.year] <- with(penSim,  C[j.year] * i[j.year])
    
    # I.e(j)
    penSim$I.e[j.year] <- with(penSim, Ia[j.year] + Ic[j.year] - Ib[j.year])
    
    # I.r(j)
    penSim$I.r[j.year] <- with(penSim, i.r[j.year] *( AA[j.year] + C[j.year] - B[j.year]))
    
    # Funded Ratio
    penSim$FR[j.year] <- with(penSim, AA[j.year] / AL[j.year])
    
    # External fund
    penSim$ExF[j.year] <- with(penSim, B[j.year] - C[j.year])
  }
  
  
  #penSim_results[[k]] <- penSim
  penSim
}

end_time_loop <- proc.time()

stopCluster(cl)

Time_loop <- end_time_loop - start_time_loop 
