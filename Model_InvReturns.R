# This module create investment return series. 

gen_returns <- function( #.paramlist = paramlist,
                         #.Global_paramlist = Global_paramlist,
  
                        nyear   = Global_paramlist$nyear,
                        nsim    = Global_paramlist$nsim,
                        ir.mean = paramlist$ir.mean,
                        ir.sd   = paramlist$ir.sd,
                        seed    = 1234) {

#assign_parmsList(.Global_paramlist, envir = environment())
#assign_parmsList(.paramlist,        envir = environment())
  
  
  set.seed(seed)
  i.r <- matrix(rnorm(nyear  *nsim, mean = ir.mean, sd = ir.sd),nrow = nyear, ncol = nsim)
  
  if (all(i.r >= -1)) return(i.r) 
  else {
    warning("A draw is discarded because it contains value(s) smaller than -1.")
    gen_returns(nyear = nyear, nsim = nsim, ir.mean = ir.mean, ir.sd = ir.sd, seed = seed + 1)}
}




if(devMode){
  set.seed(1234)
  #i.r <- with(Global_paramlist, matrix(rnorm(nyear*nsim, mean = 0.08, sd = 0.12),nrow = nyear, ncol = nsim)) 
  i.r <- with(Global_paramlist, matrix(0.08, nrow = nyear, ncol = nsim))
  i.r[10,] <- 0.00 # Create a loss due to zero return in year 10. For the purpose of checking amortization of UAAL
  
} else {
  
  if(paramlist$return_type == "simple") i.r <- gen_returns()
  
  if(paramlist$return_type == "internal"){
    
    if(sum(paramlist$plan_returns$duration) != Global_paramlist$nyear) stop("Length of return series does not match nsim.", call. = FALSE)
    
    # set.seed(1234)
    i.r <- with(paramlist, mapply(gen_returns, 
                                    nyear   = paramlist$plan_returns$duration,
                                    nsim    = Global_paramlist$nsim,
                                    ir.mean = paramlist$plan_returns$ir.mean,
                                    ir.sd   = paramlist$plan_returns$ir.sd,
                                    SIMPLIFY  = (nrow(paramlist$plan_returns) == 1)
    )) %>% 
      do.call(rbind, .)
  }
  
  
  if(paramlist$return_type == "external") source(paste0(folder_run, "/getReturn.R"))
  
  
  ## Add two additional runs as run 0 and run -1.
   # Run 0: deterministic return the same as ir.mean - ir.sd^2/2
   # Run -1: deterministic return the same as i. 
  
  i.r <- cbind(rep(paramlist$i, Global_paramlist$nyear), # Check consistency
               rep(paramlist$ir.mean - paramlist$ir.sd^2/2 , Global_paramlist$nyear), # Deterministic run
               i.r)
  colnames(i.r) <- c(-1:Global_paramlist$nsim)
  
}


paramlist$plan_returns


