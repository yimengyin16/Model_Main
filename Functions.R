

# 1. function calculating temporary annuity values from age x to retirment age 65
get_tla <- function(px, i, sx = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{y, x - y}, x runing from a1 to aN. 
  # The length of px is f - a1 
  # Note that the last element is redundant, just used as a place holder. 
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
  # i:  discount rate, scalar
  # sx: salary scale. default is a n vector of 1, meaning no salary scale. 
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr = 1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- sx[j:n]/sx[j]                # salary scale
    tla[j] = sum(SS * v * pxr)          # computing annuity value at j
  } 
  return(tla)
}
get_tla(rep(0.98, 65), 0.08) # test the function

# 2. function calculating temporary annuity values from a fixed entry age y to x 
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
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
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

# 2a. function calculating temporary annuity values from a fixed entry age y to x 
get_tla2a <- function(px, i, sx = rep(1, length(px))){
  
  n <- length(px)
  tla <- numeric(n)
  v <- 1/(1 + i)
  
  tla[-1] <- cumsum(cumprod(c(1,px[1:(n-2)])* v * sx[1:(n - 1)]/sx[1])/v)
  
  return(tla)   
 }


get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65))  # test the function
get_tla2a(rep(0.98, 65), 0.08, rep(1.1, 65))# test the function


# 2. Amortization Functions

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



# functions created by Don

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


