## Actuarial Valuation


# Now we do the actuarial valuations 
# In each period, following values will be caculated:
# AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
# NC: Normal Cost  
# MA: Market value of assets.
# AA: Actuarial value of assets.
# EAA:Expected actuarial value of assets.
# UAAL: Unfunded accrued actuarial liability, defined as AL - NC
# EUAAL:Expected UAAL.
# PR: payroll 
# LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
# AM: Amount to be amortized at period t. 
# i is assumed interest rate. ELs of each period will be amortized seperately.  
# SC: Supplement cost 
# ADC: actuarially required contribution by employer. NC + SC - EEC
# C : Actual contribution
# C_ADC: shortfall in paying ADC
# B : Total beneift Payment   
# Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
# Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
# Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
# I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
# Funded Ratio: AA / AL
# C_PR: contribution as % of payroll

# Formulas
# AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
# MA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
# EAA(t+1)= AA(t) + EI(t)
# AA(t+1) = (1-w)*EAA(t+1) + w*MA(t+1)
# I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
# Ia(t) = i * AA(t)
# Ib(t) = i * B(t)
# Ic(t) = i * C(t)
# EI(t) = Ia(t) - Ib(t) + Ic(t)
# ADC   = NC(t) + SC(t)
# ADC.ER = NC(t) + SC(t) - EEC(t)
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

start_time_prep_loop <-  proc.time()

# Set up data frame
penSim0 <- data.frame(year = 1:nyear) %>%
  mutate(AL   = 0, #
         MA   = 0, #
         AA   = 0, #
         EAA  = 0, #
         FR   = 0, #
         ExF  = 0, # 
         UAAL = 0, #
         EUAAL= 0, #
         LG   = 0, #
         AM   = 0, # amount to be amortized: AM(t) = LG(t) + [ADC(t - 1) - C(t-1)]*[1 + i(t-1)], i.e. actuarial loss/gain plus shortfall in paying NC+SC in last period(plus interests) 
         NC   = 0, #
         SC   = 0, #
         EEC  = 0, #
         ERC  = 0, #
         ADC  = 0, #
         ADC.ER = 0, #
         C    = 0, #
         C_ADC= 0, #
         B    = 0, #                        
         I.r  = 0, #                        
         I.e  = 0, #
         Ia   = 0, #                         
         Ib   = 0, #                         
         Ic   = 0, #  
         i    = i,
         i.r  = 0,
         PR   = 0,
         ADC_PR = 0,
         C_PR = 0)

# matrix representation of amortization: better visualization but large size, used in this excercise
SC_amort0 <- matrix(0, nyear + m, nyear + m)
#SC_amort0
# data frame representation of amortization: much smaller size, can be used in real model later.
#SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))

# select actuarial method for AL and NC
# ALx.method <- paste0("ALx.", actuarial_method)
# NCx.method <- paste0("NCx.", actuarial_method)


## Calculate total liabilities, NCs and benefits

# Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

df_wf_active <- adply(wf_active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
                rename(year = X1) %>%
                gather(age, number.a, -ea, -year) %>% 
                mutate(year = f2n(year), age = f2n(age))

df_wf_retired <- adply(wf_retired, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
                 rename(year = X1) %>% 
                 gather(age, number.r, -ea, -year) %>% 
                 mutate(year = f2n(year), age = f2n(age))

df_wf_term <- expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.term = 1:nyear)
df_wf_term$number.v <- as.vector(wf_term)


# The following method is much slower:
# df_wf_term <- adply(wf_term, c(3,4), .fun = function(x){df = as.data.frame(x); df$ea = as.numeric(rownames(x)); df}, .parallel = F) %>% 
#                rename(year = X1, year.term = X2) %>% 
#                gather(age, number.v, -ea, -year, - year.term) %>% 
#                mutate(year = f2n(year), year.term = f2n(year.term), age = f2n(age))
# array(1:256, c(4,4,4,4)) %>% as.vector


# Join population data frames and liability data frames. 

#liab_tot_active <- 

liab <- left_join(df_wf_active, df_wf_retired) %>% 
        left_join(liab)
liab[-(1:3)] <- colwise(na2zero)(liab[-(1:3)])
liab %<>%        mutate(ALx.tot =  (ALx + ALx.v) * number.a + ALx.r * number.r, 
                 NCx.tot = (NCx + NCx.v) * number.a,
                 PR.tot  = sx * number.a,
                 B.tot = B * number.r
                 ) %>% 
          group_by(year) %>% 
          summarise(ALx.tot = sum(ALx.tot, na.rm = TRUE), 
                    NCx.tot = sum(NCx.tot, na.rm = TRUE),
                    PR.tot  = sum(PR.tot,  na.rm = TRUE),
                    B.tot   = sum(B.tot, na.rm = TRUE)) %>% 
                    as.matrix # extracting elements from matrices is much faster than from data.frame
# 
# liab_tot_retired <- liab %>% left_join(df_wf_retired) %>% 
#   mutate(B.tot = B * number,
#          ALx.tot.r = ALx.r * number) %>% 
#   group_by(year) %>% 
#   summarise(B.tot     = sum(B.tot, na.rm = TRUE),
#             ALx.tot.r = sum(ALx.tot.r, na.rm = TRUE)) %>% 
#   as.matrix
# 

# x <- as.matrix(liab_tot_active)
# microbenchmark(
# x[1, "ALx.tot"],
# x[1, 2],
# liab_tot_active$ALx.tot[1] , times = 10000)

                 
                    

# liab_tot_retired$ALx.tot.r[1]
#rm(liab) # free up memory

#liab_tot_term <- 
# liab.term1 <- 
# rm(liab.term)

liab.term <-  left_join(df_wf_term, liab.term) 
# liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])  
liab.term %<>%mutate(ALx.tot.v = ALx.v * number.v,
                      B.tot.v   = B.v   * number.v) %>% 
               group_by(year) %>% 
               summarise(ALx.tot.v = sum(ALx.tot.v, na.rm = TRUE),
                         B.tot.v   = sum(B.tot.v  , na.rm = TRUE)) %>% 
               as.matrix

end_time_prep_loop <-  proc.time()




start_time_loop <- proc.time()

cl <- makeCluster(ncore) 
registerDoParallel(cl)

#penSim_results <- list()
#for(k in 1:nsim){

penSim_results <- foreach(k = 1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
  # k <- 1
  # initialize
  penSim <- penSim0
  SC_amort <- SC_amort0 
  penSim[,"i.r"] <- i.r[, k]
  

  for (j in 1:nyear){
    # j <- 1
    # AL(j) 
    
    # AL(j)
    #penSim[j, "AL"] <- liab_tot_active[j, "ALx.tot"] + liab_tot_retired[j, "ALx.tot.r"] + liab_tot_term[j, "ALx.tot.v"]
    penSim[j, "AL"] <- liab[j, "ALx.tot"] + liab.term[j, "ALx.tot.v"]
    # NC(j)
    #penSim[j, "NC"] <- sum(wf_active[, , j] * ll2[[NCx.method]][[j]]) 
    penSim[j, "NC"]  <- liab[j, "NCx.tot"] 
    # B(j)
    #penSim[j, "B"] <-  sum(wf_retired[, , j] * ll2[["B"]][[j]])
    penSim[j, "B"]  <-  liab[j, "B.tot"] + liab.term[j, "B.tot.v"]
    # PR(j)
    #penSim[j, "PR"] <-  sum(wf_active[, , j] * ll2[["sx"]][[j]])
    penSim[j, "PR"]  <-  liab[j, "PR.tot"]
    
    # MA(j) and EAA(j) 
    if(j == 1) {penSim[j, "MA"] <- switch(init_MA,
                                          MA = MA_0,                 # Use preset value
                                          AL = penSim[j, "AL"]) # Assume inital fund equals inital liability.
                penSim[j, "EAA"] <- switch(init_EAA,
                                           AL = EAA_0,                # Use preset value 
                                           MA = penSim[j, "MA"]) # Assume inital EAA equals inital market value.                      
    } else {
      penSim[j, "MA"]  <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
      penSim[j, "EAA"] <- with(penSim, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
    }
    
    # AA(j)
    penSim[j, "AA"] <- with(penSim, (1 - w) * EAA[j] + w * MA[j]  )
    
    
    # UAAL(j)
    penSim$UAAL[j] <- with(penSim, AL[j] - AA[j]) 
    
  
    # LG(j)
     # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
    if (j == 1){
      penSim$EUAAL[j] <- 0
      penSim$LG[j] <- with(penSim,  UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
      penSim$AM[j] <- with(penSim, LG[j])
      
    } else {
      penSim$EUAAL[j] <- with(penSim, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
      penSim$LG[j] <- with(penSim,  UAAL[j] - EUAAL[j])
      penSim$AM[j] <- with(penSim, LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
    }   
    
    
    # Amortize LG(j)
    if(amort_type == "closed") SC_amort[j, j:(j + m - 1)] <- amort_LG(penSim$AM[j], i, m, g, end = FALSE, method = amort_method)  
    
    # Supplemental cost in j
    penSim$SC[j] <- switch(amort_type,
                           closed = sum(SC_amort[, j]),
                           open   = amort_LG(penSim$UAAL[j], i, m, g, end = FALSE, method = amort_method)[1])
    
    #Employee contribution
    penSim$EEC[j] <- with(penSim, PR[j] * EEC_rate)
    
    # ADC(j)
    penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
    penSim$ADC.ER[j] <- with(penSim, NC[j] + SC[j] - EEC[j])  
 
    # C(j)
    penSim$ERC[j] <- ifelse(j %in% c(10), 0,  
                   switch(ConPolicy,
                          ADC     = with(penSim, ADC.ER[j]),                          # Full ADC
                          ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                          Fixed   = with(penSim, PR_pct_fixed * PR[j])             # Fixed percent of payroll
      ) 
    )
    penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
    
    # C(j) - ADC(j)
    penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
    
    # Ia(j), Ib(j), Ic(j)
    penSim$Ia[j] <- with(penSim, AA[j] * i[j])
    penSim$Ib[j] <- with(penSim,  B[j] * i[j])
    penSim$Ic[j] <- with(penSim,  C[j] * i[j])
    
    # I.e(j)
    penSim$I.e[j] <- with(penSim, Ia[j] + Ic[j] - Ib[j])
    
    # I.r(j)
    penSim$I.r[j] <- with(penSim, i.r[j] *( AA[j] + C[j] - B[j]))
    
    # Funded Ratio
    penSim$FR[j] <- with(penSim, 100*AA[j] / exp(log(AL[j]))) # produces NaN when AL is 0.
    
    # External fund
    penSim$ExF[j] <- with(penSim, B[j] - C[j])
    
    # ADC and contribution as percentage of payroll
    penSim$ADC_PR[j] <- with(penSim, ADC[j]/PR[j])
    penSim$C_PR[j]   <- with(penSim, C[j]/PR[j])
    
  }
  
  #penSim_results[[k]] <- penSim
  penSim
}



end_time_loop <- proc.time()

stopCluster(cl)

Time_loop <- end_time_loop - start_time_loop 
Time_prep_loop <- end_time_prep_loop - start_time_prep_loop



# x <- matrix(rep(1,10000),1000); colnames(x) = 1:10
# xdf <- as.data.frame(x)
# xdf %>% head
# y = 10
# 
# microbenchmark(
# x[400, "10"],
# x[400, 10],
# x[400, y],
# xdf$V4[400],
# xdf[400, "V4"], times = 50000
# )
# head(x)

# x <- matrix(1:10, 2)
# colnames(x) <- paste0("v",1:5)
# microbenchmark( data.frame(x),
#                 as.data.frame(x)) # faster