

run_sim <- function(      .i.r = i.r, 
                          .AggLiab   = AggLiab,
                          .paramlist = paramlist,
                          .Global_paramlist = Global_paramlist){

# Run the section below when developing new features.  
#     .i.r = i.r 
#     .AggLiab   = AggLiab
#     .paramlist = paramlist
#     .Global_paramlist = Global_paramlist
  
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())
  
  
  #*************************************************************************************************************
  #                                     Defining variables in simulation ####
  #*************************************************************************************************************  
  
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
           I.dif= 0,
           Ia   = 0, #                         
           Ib   = 0, #                         
           Ic   = 0, #  
           i    = i,
           i.r  = 0,
           PR   = 0,
           ADC_PR = 0,
           C_PR = 0,
           nactives  = 0,
           nretirees = 0,
           nterms    = 0)
  penSim0 <- as.list(penSim0)
  
  # matrix representation of amortization: better visualization but large size, used in this excercise
  SC_amort0 <- matrix(0, nyear + m, nyear + m)
  # SC_amort0
  # data frame representation of amortization: much smaller size, can be used in real model later.
  # SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))
  
  # Vector used in asset amortization
  s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 
  
  
#*************************************************************************************************************
#                                       Simuation  ####
#*************************************************************************************************************
  
  # AL(j)
  penSim0$AL.act  <- .AggLiab$active[, "ALx.a.sum"]
  penSim0$AL.ret  <- .AggLiab$retiree[,"ALx.r.sum"]
  penSim0$AL.term <- .AggLiab$active[, "ALx.v.sum"] 
  penSim0$AL.Ben  <- .AggLiab$retiree[,"ALx.r.sum"]  + .AggLiab$term[, "ALx.v.sum"]
  penSim0$AL      <- .AggLiab$active[, "ALx.av.sum"] + .AggLiab$term[, "ALx.v.sum"] + .AggLiab$retiree[,"ALx.r.sum"] 
  
  .AggLiab$active[, "ALx.v.sum"]
  
  # NC(j)
  penSim0$NC.act  <- .AggLiab$active[, "NCx.a.sum"]
  penSim0$NC.term <- .AggLiab$active[, "NCx.v.sum"] 
  penSim0$NC      <- .AggLiab$active[, "NCx.av.sum"] 
  
  # B(j)
  penSim0$B    <- .AggLiab$retiree[, "B.r.sum"] + .AggLiab$term[, "B.v.sum"]
  penSim0$B.v  <- .AggLiab$term[, "B.v.sum"]
  
  # PR(j)
  penSim0$PR <- .AggLiab$active[, "PR.sum"]
  
  # nactives, nretirees, nterms
  penSim0$nactives  <- .AggLiab$active[,  "nactives"]
  penSim0$nretirees <- .AggLiab$reitree[, "nretirees"]
  penSim0$nterms    <- .AggLiab$term[,    "nterms"]
  
  cl <- makeCluster(ncore) 
  registerDoParallel(cl)
  
  #penSim_results <- list()
  #for(k in 1:nsim){
  
  penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
    # k <- 1
    # initialize
    penSim <- penSim0
    SC_amort <- SC_amort0 
    penSim[["i.r"]] <- .i.r[, as.character(k)]
    
    source("Functions.R")
    
    for (j in 1:nyear){
      # j <- 2
      # AL(j) 
      
   
      # MA(j) and EAA(j) 
      if(j == 1) {penSim$MA[j]  <- ifelse(k == -1, penSim$AL[j],
                                           switch(init_MA, 
                                                  MA = MA_0,                        # Use preset value
                                                  AL = penSim$AL[j],                # Assume inital fund equals inital liability.
                                                  AL_pct = penSim$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
                                   ) 
      penSim$EAA[j] <- switch(init_EAA,
                              AL = EAA_0,                       # Use preset value 
                              MA = penSim$MA[j])                # Assume inital EAA equals inital market value.
      penSim$AA[j]  <- switch(smooth_method,
                              method1 =  with(penSim, MA[j]),   # we may want to allow for a preset initial AA.
                              method2 =  with(penSim, (1 - w) * EAA[j] + w * MA[j])
      )
      } else {
        penSim$MA[j]  <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim$EAA[j] <- with(penSim, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
                                method2 = with(penSim, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }
      
      # do we need do consider interest when using asset smoothing method1? 
      
      
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
        penSim$LG[j]    <- with(penSim,  UAAL[j] - EUAAL[j])
        penSim$AM[j]    <- with(penSim,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
      }   
      
      
      # Amortize LG(j)
      if(amort_type == "closed") SC_amort[j, j:(j + m - 1)] <- amort_LG(penSim$AM[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)  
      
      # Supplemental cost in j
      penSim$SC[j] <- switch(amort_type,
                             closed = sum(SC_amort[, j]),
                             open   = amort_LG(penSim$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      
      # Employee contribution, based on payroll. May be adjusted later. 
      penSim$EEC[j] <- with(penSim, PR[j] * EEC_rate)
      
      # ADC(j)
      
      if(nonNegC){
        penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
        penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
        
        # Adjustment of EEC
        if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
        
      } else {
        # Allow for negative ADC and C  
        penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
        
        if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
        # EEC is not fixed
        # 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
        } else if(with(penSim, ADC[j] > EEC[j])) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
          # 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
        } else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
          penSim$ADC.ER[j] <- 0
          penSim$EEC[j]    <- with(penSim, ADC[j])
          # 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
        } else if(with(penSim, ADC[j] <= 0)) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j])
          penSim$EEC[j]    <- 0
        }
        
      }
      
      
      # ERC
      penSim$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
      
      if(j %in% plan_contributions$year) {
        penSim$ERC[j] <- as.numeric(plan_contributions[j == plan_contributions$year, "pct_ADC"]) * penSim$ERC[j]
      }
      

      
      # C(j)
      penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
      
      # C(j) - ADC(j)
      penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim$Ia[j] <- with(penSim,  MA[j] * i[j])
      penSim$Ib[j] <- with(penSim,  B[j] * i[j])
      penSim$Ic[j] <- with(penSim,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim$I.e[j] <- with(penSim, Ia[j] + Ic[j] - Ib[j])
      penSim$I.e[j] <- with(penSim, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim$I.r[j] <- with(penSim, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j)
      penSim$I.dif[j] <- with(penSim, I.r[j] - I.e[j])
      
      
    }
    
    # penSim_results[[k]] <- penSim
    as.data.frame(penSim)
  }
  
  stopCluster(cl)

  

    
#*************************************************************************************************************
#                                  Combining results into a data frame.   ####
#*************************************************************************************************************

   
  penSim_results <- bind_rows(penSim_results) %>% 
    mutate(sim     = rep(-1:nsim, each = nyear),
           runname = runname,
           FR      = 100 * AA / exp(log(AL)),
           FR_MA   = 100 * MA / exp(log(AL)),
           UAAL_PR = 100 * UAAL / PR,
           MA_PR   = 100 * MA / PR,
           AA_PR   = 100 * AA / PR,
           AL_PR   = 100 * AL / PR,
           AL.act_PR    = 100 * AL.act / PR, 
           AL.ret_PR    = 100 * AL.ret / PR, 
           AL.term_PR   = 100 * AL.term / PR, 
           AL.Ben_PR    = 100 * AL.Ben / PR,
           ADC_PR  = 100 * ADC / PR,
           NC_PR   = 100 * NC / PR,
           NC.act_PR    = 100 * NC.act / PR,
           NC.term_PR   = 100 * NC.term / PR,
           SC_PR   = 100 * SC / PR, 
           ERC_PR  = 100 * ERC / PR, 
           C_PR    = 100 * C / PR,
           B_PR    = 100 * B / PR,
           ExF     = C - B,
           ExF_PR  = 100 * ExF / PR,
           ExF_MA  = 100 * ExF / MA,
           PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA)) %>%
    select(runname, sim, year, everything())
  
  return(penSim_results)
  
}



start_time_loop <- proc.time()

penSim_results <- run_sim()

end_time_loop <- proc.time()
Time_loop <- end_time_loop - start_time_loop 
Time_loop


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

# (dt1 <- data.table(A = letters[1:10], X = 1:10))
# (dt2 <- data.table(A = letters[5:14], Y = 1:10))
# merge(dt1, dt2, by = "A")
# merge(dt1, dt2, by = "A", all = TRUE)
# 
# (dt1 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(1:3, 2)], X = 1:6, key = "A,B"))
# (dt2 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(2:4, 2)], Y = 6:1, key = "A,B"))
# merge(dt1, dt2)
# merge(dt1, dt2, by="B", allow.cartesian=TRUE)
# 
# x <- as.matrix(liab_tot_active)
# microbenchmark(
# x[1, "ALx.tot"],
# x[1, 2], # fastest
# liab_tot_active$ALx.tot[1] , times = 10000)


