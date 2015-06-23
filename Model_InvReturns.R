# This module create investment return series. 

gen_returns <- function(nyear   = Global_paramlist$nyear,
                        nsim    = Global_paramlist$nsim,
                        ir.mean = paramlist$ir.mean,
                        ir.sd   = paramlist$ir.sd,
                        seed    = 1234) {
  set.seed(seed)
  i.r <- matrix(rnorm(nyear*nsim, mean = ir.mean, sd = ir.sd),nrow = nyear, ncol = nsim)
  
  if (all(i.r >= -1)) return(i.r) 
  else {
    warning("A draw is disgarded because it contains value(s) smaller than -1.")
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
                                  nyear = plan_returns$duration, 
                                  nsim  = Global_paramlist$nsim,
                                  ir.mean = plan_returns$ir.mean,
                                  ir.sd = plan_returns$ir.sd,
                                  SIMPLIFY = (nrow(plan_returns) != 1)
    )) %>% 
      do.call(rbind, .)
  }
   
}
