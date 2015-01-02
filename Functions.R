

# 1. function calculating temporary annuity values from age x to retirment age 65
get_tla <- function(px, i, sx = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{y, x - y}, x ruuning from a1 to aN. 
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
    # pxr <- cumprod(c(1, px[j:(n - 1)]))
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
get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65)) %>% length # test the function
