#*********************************************************************************************************
#                      ## Making the procedure a function  ####
#*********************************************************************************************************

rm(list = ls())

load("Data/UCRP.inputs1.RData")
load("Data/UCRP.inputs2.RData")
load( "Data/UCRP.df.noAdj.RData")

source("Functions.R")



get_liab.ben <- function(COLA, i, 
                         gender.R, pct.ca, age.range, age.r.range,
                         mortality = mortality.post,
                         reduction.factor = 1){
  
  
  # COLA   <- 0.02
  # B.init <- 1
  # i      <- 0.0725
  # v      <- 1/(1 + i)
  # 
  # gender.R  <- "M" 
  # pct.ca    <- 0.25
  # age.range <- 53:120
  # age.r.range <- 53:70
  
  
  # Functions
  get_surv.liab <- function(pRxm.R, qxm.R, pxm.S, i,  liab.ben){
    # This function calculates the ALs for a retiree that are attributable to benefits payable to survivors. 
    
    
    # pRxm.M = df2$pRxm.M
    # qxm.M  = df2$qxm.post.M
    # pxm.F  = df2$pxm.post.F
    # liab.ben = df2$liab.ben.indiv
    
    n <- length(liab.ben)
    surv.liab <- numeric(n)
    
    for(j in 1:(n - 1)){
      #j = 1  
      surv.liab[j] <- sum(
        pRxm.R[j:(n - 1)]/pRxm.R[j] * qxm.R[j:(n-1)] * # probability of dying at each period 
          cumprod(pxm.S[j:(n-1)]) *                      # probability of spouse being still alive at each period 
          1/(1 + i)^(1 : (n-j)) * liab.ben[(j + 1):n]    # AL for survivor dis
      )  
    }
    
    surv.liab[n] <- 0  
    surv.liab
    
  }
  
  
  # calculate the liability and benefit cashflow for a male retiree who retirees at age 65 and has
  # an eligible survivor 3 years younger than him.
  
  if("numeric" %in% class(reduction.factor) ) reduction = data.frame(age.r = age.r.range, reduction = reduction.factor)
  
  df <- expand.grid(age = (min(age.range) - 3):max(age.range), age.r = age.r.range) %>% 
    left_join(mortality.post %>% filter(year == 2029) %>%  select(age, qxm.post.M, qxm.post.F)) %>%
    left_join(reduction) %>% 
    group_by(age.r) %>%
    mutate(gender.R = gender.R,
           age.S = ifelse(gender.R == "M", age - 3, age + 3),
           qxm.R = ifelse(gender.R == "M", qxm.post.M, qxm.post.F),
           qxm.S = ifelse(gender.R == "M", lag(qxm.post.F, 3), lead(qxm.post.M, 3)),
           qxm.S = ifelse(gender.R == "M",
                          ifelse(age == max(age), 1, qxm.S), # For convenience, it is assumed that the max age for the female spouses is 117.)
                          ifelse(age.S > max(age.range), 1, qxm.S)
           ))%>% 
    filter(age >= age.r) %>%  
    mutate(
      COLA.scale = (1 + COLA)^(row_number() - 1 ),
      B =  COLA.scale * reduction, #* reduction,
      pxm.R = 1 - qxm.R,
      pxm.S = 1 - qxm.S,
      
      # AL of an individual retiree attributable to the life annuity at each age. 
      pRxm.R     = ifelse(age == min(age), 1, lag(cumprod(pxm.R))),
      ax.r.R     =  get_tla(pxm.R, i, COLA.scale),
      liab.ret.indiv.la = B * ax.r.R,    # "la" for life annuity
      
      # AL of an individual survivor at each age. 
      # (Note that the AL is only the function of the age of the survivor, and not affected by the the year of the retiree's death.)
      ax.b.S     = get_tla(pxm.S, i, COLA.scale),
      liab.ben.indiv = pct.ca * B * ax.b.S,
      
      # AL of an individual retriee with an living survivor atributable to the contingent annuity(survivor) at each age.
      # For each age, this is the sum of all future individual survivor liability weighed by the discount factor and 
      # the prob of the contingent annuity starting from that year. 
      
      liab.ret.indiv.ca = get_surv.liab(pRxm.R, qxm.R, pxm.S, i, liab.ben.indiv) # "ca" for contingent annuity
      
    ) %>% 
    
    ## Demographic dynamics  ##
    
    mutate(
      
      pxm.R1S1 = pxm.R * pxm.S,
      
      qxm.R1S0 = pxm.R * (1 - pxm.S),
      qxm.R0S1 = (1 - pxm.R) * pxm.S, 
      qxm.R0S0 = (1 - pxm.R) * (1 - pxm.S), 
      
      n.R1S1 =  ifelse(age == min(age), 1, lag(cumprod(pxm.R1S1))),
      
      n.newR1S0 = n.R1S1 * qxm.R1S0,
      n.newR0S1 = n.R1S1 * qxm.R0S1,
      
      n.R1S0 = 0,
      n.R0S1 = 0,
      n.R0   = 0
    ) 
  
  
  
  fn_demo <- function(df.demo){      
    for (j in 2:nrow(df.demo)){
      df.demo$n.R1S0[j] <- with(df.demo, n.R1S0[j-1] * pxm.R[j-1] + n.newR1S0[j-1])
      df.demo$n.R0S1[j] <- with(df.demo, n.R0S1[j-1] * pxm.S[j-1] + n.newR0S1[j-1])
      
      df.demo$n.R0[j] <- with(df.demo, n.R0[j - 1] + 
                                n.R1S1[j-1] * (1 - pxm.R[j-1]) +  
                                n.R1S0[j-1] * (1 - pxm.R[j-1]) )
    }
    return(df.demo)
  }
  
  df <- split(df, df$age.r) %>% lapply(fn_demo) %>% rbind_all
  
  # check demographic
  df.check <- df %>% mutate(tot.check = n.R1S1 + n.R1S0 + n.R0) %>% select(age, age.S, n.R1S1, n.R1S0, n.R0S1, tot.check) 
  
  #                      ## Aggregate liability and benefit payment  ##
  
  df %<>% select(age.r,age, age.S, B, liab.ret.indiv.la, liab.ret.indiv.ca, liab.ben.indiv,
                 n.R1S1, n.R1S0, n.R0S1) %>% 
    group_by(age.r) %>% 
    mutate(
      B.R = (n.R1S1 + n.R1S0) * B,   # total annuity payment for retirees.
      B.S =  n.R0S1 * pct.ca * B,    # total contingent annuity payment for survivors.
      B.tot = B.R + B.S,             # total benefit payment
      
      liab.tot.R1S1 = (liab.ret.indiv.ca + liab.ret.indiv.la) * n.R1S1, # total liability for retirees whose spouses are alive.
      liab.tot.R1S0 = liab.ret.indiv.la * n.R1S0,                       # total liability for retirees whose spouses are dead.
      liab.tot.R0S1 = liab.ben.indiv * n.R0S1,                          # total liability for survivors. 
      liab.tot      = liab.tot.R1S1 + liab.tot.R1S0 + liab.tot.R0S1,    # total liability for all. 
      
      liab.tot.la   = liab.ret.indiv.la * (n.R1S1 + n.R1S0) , # only look at the first row
      reduction     = liab.tot.la / liab.tot,     # only look at the first row
      
      MA = ifelse(age == min(age), liab.tot, 0),
      B_R = liab.tot.R0S1 / (liab.tot.R1S1 + liab.tot.R1S0),
      B.S_B.R = B.S / B.R)
  
  
  # Check internal consistency 
  
  fn_MA <- function(df){
    
    for (j in 2:nrow(df)){
      df$MA[j] <- with(df, (MA[j-1] - B.tot[j-1]) * (1 + i) )
    }
    return(df)
  }
  
  df <- split(df, df$age.r) %>% lapply(fn_MA) %>% rbind_all %>% 
    mutate(FR = MA / liab.tot) 
  
  return(df)
  
}




# Run the function

# For male retirees
# w/o benefit reduction
df1.M <- get_liab.ben(COLA = 0.02, i = 0.0725,
                      gender.R = "M", pct.ca = 0.25, age.range = 53:120, age.r.range = 53:70)

# save the benefit reductin factor for contingent annuity. 
reduction.M <- df1.M %>% group_by(age.r) %>% filter(age == min(age)) %>% select(reduction) 

# w/ benefit reduction
df2.M <- get_liab.ben(COLA = 0.02, i = 0.0725,
                      gender.R = "M", pct.ca = 0.25, age.range = 53:120, age.r.range = 53:70,
                      reduction = reduction)


# For female retirees
# w/o benefit reduction
df1.F <- get_liab.ben(COLA = 0.02, i = 0.0725,
                      gender.R = "F", pct.ca = 0.25, age.range = 53:120, age.r.range = 53:70)

# save the benefit reductin factor for contingent annuity. 
reduction.F <- df1.F %>% group_by(age.r) %>% filter(age == min(age)) %>% select(reduction) 

# w/ benefit reduction
df2.F <- get_liab.ben(COLA = 0.02, i = 0.0725,
                      gender.R = "F", pct.ca = 0.25, age.range = 53:120, age.r.range = 53:70,
                      reduction = reduction)

