
#**************************************
#    1. PV of Annuities           #####
#**************************************

# 1.1 function calculating temporary annuity values from age x to retirment age (fixed end)
get_tla <- function(px, i, scale = rep(1, length(px)), cashflow = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN. 
  # The length of px is f - a1 
  # Note that the last element is redundant, just used as a place holder. 
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
  # i:  discount rate, scalar
  # scale: how the annuity scale up over time. eg: 
  #        1) salary scale. default is a n vector of 1, meaning no salary scale. used when calculating career based annuity
  #        2) simple COLA scale: COLA increasing at a fixed percentage very year.  
  # cashflow: The cashflow of the annuity can be specified by this argument. This is useful when calculating PV of benefits with COLA.
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr = 1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- scale[j:n]/scale[j]                # scale
    tla[j] = sum(SS * v * pxr)                # computing annuity value at j
  } 
  return(tla)
}
get_tla(rep(0.98, 55), 0.08) # test the function

# 1.2 function calculating temporary annuity values from a fixed entry age y to x (fixed start)
get_tla2 = function(px, i, sx = rep(1, length(px))){
  # Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
  # This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 
  
  # Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
  # For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
  # eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
  # eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
  # Note that the last 2 survival rates and last salary scale are redundant in the calculation, they are just used as place holders. 
  #(calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
  # because the last annuity payment is paid at the begining of 64. )
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n. The minimum length of px allowed is 2. 
  # i:  discount rate, scalar
  # sx: salary scale. default is an n vector of 1, meaning no salary scale. 
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla = numeric(length(px))
  n = length(tla)
  
  # tla[1] will be kept as 0, next calculate tla[2:n]:
  for(j in 1:(n - 1)){
    v   <- 1/(1 + i)^(0:(j - 1))                                  # dicount vector
    if(j == 1) pxr <- 1 else pxr <- cumprod(c(1, px[1:(j - 1)]))  # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- sx[1:j]/sx[1]                                          # salary scale
    tla[j + 1] = sum(SS * v * pxr)                                # computing annuity value at j;
  } 
  return(tla) 
}

# 1.2a A simpler implementation of 1.2
get_tla2a <- function(px, i, sx = rep(1, length(px))){
  
  n <- length(px)
  tla <- numeric(n)
  v <- 1/(1 + i)
  
  tla[-1] <- cumsum(cumprod(c(1,px[1:(n-2)])* v * sx[1:(n - 1)]/sx[1])/v)
  
  return(tla)   
 }


get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65))  # test the function
get_tla2a(rep(0.98, 65), 0.08, rep(1.1, 65))# test the function

# 1.3 PVFB of term costs
get_PVFB <- function(px, v, TC){ # present values of subsets of TC (fixed end)
  # This function compute the total present value of TC[j:n] at the beginning of time j, with j running from 1 to n. 
  # The function can be used to calculate PVFB of term costs of ancillary benefits or retirement benefits with multiple
  # retirement ages. 
  
  # Inputs
    # px: numeric vector of length n. Probability of survival at time 1 through n
    # v : numeric. discount factor 1/(1 + i)
    # TC: numeric vector of length n. A series of term costs. Term costs are valued at the begninning of period. 
  # Returns
    # PVFBs of fixed end contracting windows of TC. 
  
  n <- length(px)
  
  PVFB <- sapply(seq_len(n), function(j) ifelse(j == n, TC[j], sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n])))
  
  return(PVFB)
}

# 1.4 NC of UC and PUC
get_NC.UC <- function(px, v, TC){
  # This function is a variation of get_PVFB. It is used to calculate NC under UC and PUC methods.
  # Below we explain the major difference between get_NC.UC and get_PVFB:
    # 1. Why TC[(j + 1):n]?  Remember NC is the discounted value of benefit accrual. During age x, the individual can 
    #    accrue benefit for age x + 1 to r'', so the corresponding elements in TC are TC[(j + 1):n]. Note that 
    #    TC[j+1] is gx.r(j+1)*qxr(j+1)*ax(j+1) in PUC. 
    # 2. Why start discounting from the 1st element? Since at j the individual starts accruing benefit from j + 1, 
    #    we need to discount the value in j + 1.  
  # Note The last elements (at age r'') of the result is NA by construction. 
  # px must be survival probability from min(age) to r''.
  # TC must be defined as  
    #  UC for retirement:    gx.r(x) * qxr(x) * ax(x), x running from y (entry age) to r'' (eg. 20 to 65 in Winklevoss book)
    #  PUC for retirement:   Bx(x)/(x - y) * gx.r(x) * qxr(x) * ax(x), x running from entry age (y) to r'' (0 when x = y)      
    #  PUC for vested terms: Bx(x)/(x - y) * gx.r(x) * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max]
  n <- length(px) # n is r''
  
  Fun_NC <- function(j) ifelse(j == n, NA, sum(cumprod(px[j:(n - 1)]) * v^(1:(n-j)) * TC[(j + 1):n]))
  
  NC <- sapply(seq_len(n), Fun_NC)
  
  return(NC)
}

# 1.5 AL of PUC
get_AL.PUC <- function(px, v, TC){
  # This function is a variation of get_PVFB. It is used to calculate AL under PUC methods.
  
  # Note that the only difference between get_AL.PUC and get_PVFB is that TC[j] is multiplied by (j - 1)
  
  # Note that y(entry age) corresponds to index 1 and age x corresponds to index j, so at age x the individual
  # has been accruing benefits for x - y years, which is equal to j - 1 years. (eg. Assuming y = 20, then when x = 21 and j = 2 the 
  # individual have accrued benefits for 1 year (x - y = 21 - 20 and j - 1 =  2 - 1).
  
  # TC must be defined the same way as in get_NC.UC. 
  # the first element (age y) should be zero, the last element should be the same as the last element in TC.
  
  n <- length(px) # n is r'' - y + 1
  AL <- sapply(seq_len(n), 
                 function(j) ifelse(j == n, TC[j]*(j-1), sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n] * (j - 1)))
                 )
  return(AL)
}



#**************************************
# 2. Amortization Functions       #####
#**************************************

pmt <- function(p, i, n, end = FALSE){
  # amortization function with constant payment at each period 
  # p = principle, i = interest rate, n = periods. 
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i)
  a_n <- (1 - (1 + i)^(-n))/(1 - 1/(1 + i))
  pmt <- p / a_n
  return(pmt)  
}

# pmt(100, 0.08, 10)
# pmt2(100, 0.08, 10, TRUE)
# pmt2(-100, 0.08, 10)

gaip <- function(p, i, n, g, end = FALSE){
  # p=principal, i=interest rate, n=periods, g=growth rate in payments
  # calculating gaip directly
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i) 
  k <- (1 + g)/(1 + i)
  a_sn <- (1 - k^n )/(1 - k)
  pmt <- p/a_sn
  return(pmt)
}

# gaip2(100, 0.08, 10, 0.04)
# gaip3(100, 0.08, 10, 0.02, end = TRUE)


# Constant dollar amortization method
amort_cd <- function(p, i, m, end = FALSE) rep(pmt(p, i, m, end), m)

# Constant percent amortization method
amort_cp <- function(p, i, m, g, end = FALSE) gaip(p, i, m, g, end)*(g + 1)^(1:m - 1)

# Strait line method #
amort_sl <- function(p, i, m, end = FALSE){
  # Straitline amortization method
  # See Winklevoss(1993, p101)
  if(end){
    sl <- i*(p - p*(0:(m - 1))/m) + p/m
  } else {
    d <- 1/(1+i)
    sl <- d*(p - p*(1:m)/m) + p/m}
  return(sl)
  }
    
# Test the functions
amort_cd(100, 0.08, 10, F)
amort_cp(100, 0.08, 10, 0.05, F)
amort_sl(100, 0.08, 10, F)

# Function for choosing amortization methods

amort_LG <- function(p, i, m, g, end = FALSE, method = "cd"){
  # amortize the gain/loss using specified amortization method
  switch(method,
         cd = amort_cd(p, i ,m, end),
         cp = amort_cp(p, i, m, g, end),
         sl = amort_sl(p, i, m, end)
         )
  }


#********************************
# 3.Utility functions        ####
#********************************
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA

ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail

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

na2zero <- function(x){x[is.na(x)] <- 0 ;return(x)}

f2n <- function(x) as.numeric(levels(x)[x])
#f2n2 <- function(x) as.numeric(as.character(factor(x))) # much slower than f2n




## spline smoothing 
splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar<-names(df)[length(names(df))]
  nonfillvar<-setdiff(names(df),c(fillvar,valvar))
  f<-function(x) {
    if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
    spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
    dfout<-data.frame(x=spl$x, y=spl$y)
    names(dfout)<-c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
  return(dfl2)
}



## Functions for model control
assign_parmsList <- function(paramlist, excludes = NULL, ...){
  varNames   <- setdiff(names(paramlist), excludes)
  assign_var <- function(x) assign(x, paramlist[[x]], ...)
  sapply(varNames, assign_var)
}




# pmt <- function(p, i, n){
#   # amortization function, with payment at the end of period. 
#   # p = principle, i = interest rate, n = periods. 
#   pmt <- p * (1 + i)^n * i/((1 + i)^n - 1)
#   return(pmt)  
# }

# gaip2 <- function(p, i, n, g){
#   # p=principal, i=interest rate, n=periods, g=growth rate in payments
#   # calculating gaip directly
#   # end: , if TRUE, payment at the end of period. 
#   #if(end) p <- p*(1 + i) 
#   k <- (1 + i)/(1 + g)
#   gaf <- (1 + i) * (1 - k)/(k * (k^(-n) - 1))
#   return(gaf*p)
# }




