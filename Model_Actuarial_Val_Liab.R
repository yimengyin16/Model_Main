# This program calculates actuarial liabilities, normal costs, benefits, and other values that will be used
# in the simulation model

start_time_liab <- proc.time()

# 1. Decrement table ####

# Notes
 # 1) For now, we assume all decrement rates do not change over time.  
 # 2) Use decrement rates from winklevoss.  
 # 3) Now assume the decrement tables contain multiple decrement rates(probabilities) rather than single decrement rates.
      # If the decrement tables provide single decrement rates, we need to convert them to multiple decrement rates in a consistent way.   
      # At least for TPAF, the multiple decrement rates (probabilities) are provided in AV.  

# Timing of decrements
 # Time period t is defined as the time interval [t, t+1), closed at the beginning and open at the end. 
 # Assume retirement is independent of all other risks and occurs at the beginning of time period t, with the probability qxr(t).
   # Individual's status at t becomes "retired" immediately after the risk of retirement is realized at the beginning of t.    
 # The occurence of death, disability and termination follow UUD over period t. 
 # payment of retirement benefit occurs at the beginning of t. Hence all retirees will recieve benefit at least once, at the very moment when
   # they become retirees. 
 # Given the assumptions above, it follows that (' indicates single decrement rates)
   # qe = qe'
   # qt = qt'(1 - 0.5qm')(1 - 0.5 qd')(1 - qe'), (qd, qm are similar), note that qd=qm=qt=0 at max retirement age, when qe' = 1
   # p  = 1 - qe - qt - qm - qd
 # We assume qe, qt, qd, qm are directly available from data.     


load("Data/winklevossdata.RData")

# Create decrement table and calculate probability of survival
decrement <- expand.grid(age = range_age, ea = range_ea) %>% 
  left_join(filter(gam1971, age>=20)) %>%    # mortality 
  left_join(term2) %>%                       # termination
  left_join(disb)  %>%                       # disability
  left_join(dbl)   %>%                       # mortality for disabled
  left_join(er)    %>%                       # early retirement
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>% 
  filter(age >= ea) %>%
  group_by(ea) 

decrement$qxe.p <- na2zero(decrement$qxe.p)
decrement$qxe.p <- ifelse(decrement$age == r.max, 1, 0) # Single retirement age. 

# Timing of decrements
  
decrement %<>% 
  # For active(".a"). 
  mutate(qxt.a   = ifelse(age >= r.max, 0, qxt.p),   # qxt.p         * (1 - qxd.p/2) * (1 - qxm.p/2),
         qxd.a   = ifelse(age >= r.max, 0, qxd.p),   # (1 - qxt.p/2) * qxd.p         * (1 - qxm.p/2),
         qxm.a   = ifelse(age >= r.max, 0, qxm.p),   # (1 - qxt.p/2) * (1 - qxd.p/2) * qxm.p, 
         qxr.a   = qxe.p                             # ifelse(age == 64, (1 - qxt.p)*(1 - qxd.p)*(1 - qxm.p), 0)
  ) %>%
  
  # For terminated(".t"), target status are dead and retired.
  # Terminated workers will never enter the status of "retired". Rather, they will begin to receive pension benefits 
  # when reaching age r.max, but still with the status "terminated". So now we do not need qxr.t
  mutate(qxm.t   = qxm.p) %>%
  
  # For disabled(".d"), target status are dead. Note that we need to use the mortality for disabled 
  # Note the difference from the flows 3Darray.R. Disabled can not become retired here. 
  mutate(qxm.d = qxmd.p ) %>%
  
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  mutate(qxm.r   = qxm.p) %>% 
  
  # Calculate various survival probabilities
  mutate( pxm = 1 - qxm.p,
          pxT = 1 - qxt.p - qxd.p - qxm.p - qxe.p) #(1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          # px65m = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm))), # prob of surviving up to r.max, mortality only
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only

# 2. Salary scale #### 
# We start out with the case where 
# (1) the starting salary at each entry age increases at the rate of productivity growth plus inflation.
# (2) The starting salary at each entry age are obtained by scaling up the the salary at entry age 20,
#     hence at any given period, the age-30 entrants at age 30 have the same salary as the age-20 entrants at age 30. 

# Notes:
# At time 1, in order to determine the liability for the age 20 entrants who are at age 110, we need to trace back 
# to the year when they are 20, which is -89. 

# scale for starting salary 
growth <- data.frame(start.year = -89:nyear) %>%
  mutate(growth = (1 + infl + prod)^(start.year - 1 ))

# Salary scale for all starting year
salary <- expand.grid(start.year = -89:nyear, ea = range_ea, age = 20:(r.max - 1)) %>% 
  filter(age >= ea, start.year + 110 - ea >= 1 ) %>%
  arrange(start.year, ea, age) %>%
  left_join(merit) %>% left_join(growth) %>%
  group_by(start.year, ea) %>%
  mutate( sx = growth*scale*(1 + infl + prod)^(age - min(age)))


# 3. Individual AL and NC by age and entry age ####

# variables relevant to COLA: B, ax, ALx.r

liab <- expand.grid(start.year = -89:nyear, ea = range_ea, age = range_age) %>%
  filter(start.year + 110 - ea >= 1) %>% # drop redundant combinations of start.year and ea. 
  left_join(salary) %>% 
  right_join(decrement) %>%
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate salary and benefits
  mutate(
    year = start.year + age - ea,                      # year index in the simulation
    # vrx = v^(r.max-age),                                # discount factor
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale.
    Bx = benfactor * yos * fas,                        # accrued benefits payable at age r.max
    B  = ifelse(age>=r.max, Bx[age == r.max] * COLA.scale/COLA.scale[age == r.max], 0), # annual benefit # NOT COMPATIBLE WITH MULTIPLE RETIREMENT AGES!!!
    bx = lead(Bx) - Bx,                                # benefit accrual at age x
    ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at 110 for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age 110. 
    axR = c(get_tla(pxT[age<r.max],i), rep(0, 110 - r.max + 1)),                      # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age<r.max],i, sx[age<r.max]), rep(0, 110 - r.max + 1)),          # ^s_aT..{x:r.max-x-|}
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, 110 - r.max)),                  # need to make up the length of the vector up to age 110
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, 110 - r.max))   # need to make up the length of the vector up to age 110
   
  ) %>%
  
  #(liab %>% filter(start.year == -89, ea == 20, age >= r.max) %>% select(pxm))$pxm %>% get_tla(i)
  #(liab %>% filter(start.year == -89, ea == 20, age <= r.max) %>% select(pxT))$pxT %>% get_tla2a(i)
  
  
  # Calculate normal costs with multiple retirement ages
  mutate(gx.r  = ifelse(age %in% r.min:r.max, 1 - 12 * (r.max - age) * 0.0025 , 0), # reduction factor for early retirement benefits
         TCx.r = gx.r * Bx * qxr.a * ax,  # term cost of retirement
         PVFBx.r = c(get_PVFB(pxT[age <= r.max], v, TCx.r[age <= r.max]), rep(0, 110 - r.max)),
         
         ## NC and AL of UC
         # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
         # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         
         # NC and AL of PUC
         TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax)), # Note that this is not really term cost 
         NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, 110 - r.max)),
         ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, 110 - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD = ifelse(age < r.max, PVFBx.r[age == min(age)]/ayx[age == r.max], 0),
         ALx.EAN.CD = PVFBx.r - NCx.EAN.CD * axR,
         # NC and AL of EAN.CP
         NCx.EAN.CP = ifelse(age < r.max, sx * PVFBx.r[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
         ALx.EAN.CP = PVFBx.r - NCx.EAN.CP * axRs,
         ALx.r      = ifelse(age < r.max, 0, ax * B)  # Remaining liability(PV of unpaid benefit) for retirees, identical for all methods  # NOT COMPATIBLE WITH MULTIPLE RETIREMENT AGES!!!
  ) %>% 
  
  
  ungroup %>% 
  select(start.year, year, ea, age, everything()) 

# 4. Calculate Total Actuarial liabilities and Normal costs 

# Define a function to extract the variables in a single time period
extract_slice <- function(Var, Year,  data = liab){
  # This function extract information for a specific year.
  # inputs:
  # Year: numeric
  # Var : character, variable name 
  # data: name of the data frame
  # outputs:
  # Slice: data frame. A data frame with the same structure as the workforce data.
  Slice <- data %>% ungroup %>% filter(year == Year) %>% 
    select_("ea", "age", Var) %>% arrange(ea, age) %>% spread_("age", Var, fill = 0)
  rownames(Slice) = Slice$ea
  Slice %<>% select(-ea) %>% as.matrix
  return(Slice)
}

# Extract variables in liab that will be used in the simulation, and put them in a list.
# Storing the variables this way can significantly boost the speed of the loop. 

# var.names <- liab %>% select(B:ALx.EAN.CP) %>% colnames()

# 
# #cl <- makeCluster(ncore)
# #registerDoParallel(cl)
# liab_list <- alply(var.names, 1, function(var){
#   alply(1:100,1, function(n) extract_slice(var, n))
# },
# #.parallel = TRUE,
# #.paropts = list(.packages = c("dplyr", "tidyr"))
# # Parellel does not work here b/c it does not recognize objects outside the function. 
# .progress = "text"
# )
# # stopCluster(cl)
# 
# names(liab_list) <- var.names



# a faster way to create liab_list instead of extract_slice and using aply twice
# the speed comes from processing the entire df first and setting the data frame up in matrix format before extracting the matrices into the list
var.names <- colnames(liab)[grep("^sx$|^B$|^ALx|^NCx", colnames(liab))]
a <- proc.time()
lldf <- liab %>% ungroup %>% # I don't think liab is grouped, but just in case...
  filter(year %in% 1:100) %>%
  select(year, ea, age, one_of(var.names)) %>% 
  right_join(expand.grid(year=1:100, ea=range_ea, age=range_age)) %>% # make sure we have all possible combos
  gather(variable, value, -year, -ea, -age) %>%
  spread(age, value, fill=0) %>%
  select(variable, year, ea, everything()) %>%
  arrange(variable, year, ea) # this df is in the same form and order, within each var, as the liab_list of matrices (vars may be in a different order)


# microbenchmark(
# ll2[["NCx.PUC"]]$`3` + ll2[["NCx.PUC"]]$`3`,
# ll2$NCx.PUC$`3` + ll2$NCx.PUC$`3`,
# )
  
# might be possible to stop here, but continue and create an equivalent list
f.inner <- function(df.inner) as.matrix(df.inner[-c(1:3)])
f.outer <- function(df.outer) lapply(split(df.outer, df.outer$year), f.inner) # process each year
ll2 <- lapply(split(lldf, lldf$variable), f.outer) # process each variable
proc.time() - a

# compare year 3 for variable NCx.PUC
# ll2$NCx.PUC$`3`
# liab_list$NCx.PUC$`3`

end_time_liab <- proc.time()

Time_liab <- end_time_liab - start_time_liab





#eg. extract "B" for year 1, a matrix is returned
#liab_list[["B"]][[1]]


# a <- proc.time()
# extract_slice("NCx.EAN.CP",1)
# extract_slice("NCx.EAN.CD",1)
# extract_slice("NCx.PUC", 1)
# b <- proc.time()
# b-a
# 
# 
# extract_slice("ALx.EAN.CP",1)
# extract_slice("ALx.EAN.CD",1)
# extract_slice("ALx.PUC", 1)
# extract_slice("ALx.r", 1)
# 
# 
# extract_slice("B", 1) # note that in the absence of COLA, within a time period older retirees receive less benefit than younger retirees do.
# b <- proc.time()
# b-a
# 
# 
# # Total AL for Active participants
# sum(wf_active[, , 1] * extract_slice("ALx.EAN.CP",1))
# sum(wf_active[, , 1] * extract_slice("ALx.EAN.CD",1))
# sum(wf_active[, , 1] * extract_slice("ALx.PUC",1))
# 
# 
# # Total Normal Costs
# sum(wf_active[, , 1] * extract_slice("NCx.EAN.CP",1))
# sum(wf_active[, , 1] * extract_slice("NCx.EAN.CD",1))
# sum(wf_active[, , 1] * extract_slice("NCx.PUC",1))
# 
# # Total AL for retirees
# sum(wf_retired[, , 1] * extract_slice("ALx.r",1))
# 
# # Total benefit payment
# sum(wf_retired[, , 1] * extract_slice("B",1))

