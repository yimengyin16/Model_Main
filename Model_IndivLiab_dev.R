#*************************************************************************************************************
#                        Individual AL and NC by age and entry age ####
#*************************************************************************************************************
# This program calculates actuarial liabilities, normal costs, benefits, and other values that will be used
# in the simulation model.


get_IndivLiab <- function(.salary    = salary, 
                          .benefit   = benefit, 
                          .decrement = decrement,
                          .paramlist = paramlist,
                          .Global_paramlist = Global_paramlist){

# Inputs:
  # Data frames:
  #  - salary:       Salary  table.  History and prospect salary for all start.year, age and ea combos
  #  - benefit:      Benefit table.  Benefit payments for all age and ea combos at period 1
  #  - decrement:    Decrement table.
  #  - Parameters(in paramlist and Gloabal_paramlist)
  #   - max.age, min.age
  #   - nyear
  #   - range_ea, range_age
  #   - fasyears
  #   - benfactor
  #   - cola
  #   - r.max, r.min
  #   - v.yos
  #   - i
  #   - actuarial_method     
# Output
  # liab: a list containing following data frames: 
    # - active: individual liabilities of actives by year, ea, and age
    # - term:   individual liabilities of terms   by year, year.term, ea and age 

# Notes:
  # variables relevant to COLA: B, ax, ALx.r


# Run the section below when developing new features.   
#   .salary    <-  salary 
#   .benefit   <-  benefit 
#   .decrement <-  decrement
#   .paramlist <-  paramlist
#   .Global_paramlist <-  Global_paramlist  
  
  
assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())


#*************************************************************************************************************
#                     Preparation                        #####                  
#*************************************************************************************************************

min.year <- min(1 - (max.age - (r.max - 1)), 1 - (r.max - 1 - min.ea))
## Track down to the year that is the smaller one of the two below: 
 # the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
 # the year a r.max year old active in year 1 enter the workforce at age min.ea 

liab.active <- expand.grid(start.year = min.year:nyear, ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= 1)   %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  left_join(.salary)  %>%
  left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  right_join(.decrement) %>%
  arrange(start.year, ea, age) %>%
  group_by(start.year, ea) %>%
  # Calculate salary and benefits
  mutate(
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = na2zero(benfactor * yos * fas),                        # accrued benefits
    bx = lead(Bx) - Bx,                                # benefit accrual at age x
    
    ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    ax.r = get_tla(pxm.r, i, COLA.scale),              # ax calculated with mortality table for retirees. 
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
  
#   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
#   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i), rep(0, max.age - r.full + 1))),                 # Similar to axR, but based on r.full.  For calculation of term benefits when costs are spread up to r.min.        
    axrs= ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i, sx[age<r.full]), rep(0, max.age - r.full + 1))),  # Similar to axRs, but based on r.full. For calculation of term benefits when costs are spread up to r.min.
     
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )
  
# c1 <- !is.na(liab$B) & liab$B!=0
# c2 <- !is.na(liab$B.init) & liab$B.init!=0
# cbind(c1, c2)
# range(c1+c2) # Good if only 0 and 1

# liab.active <- colwise(na2zero)(liab.active)


#*************************************************************************************************************
#                          AL and NC of service retirement for actives                        #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.r = ifelse(yos < r.yos, 0,  
                       ifelse(age > r.full, 1, ifelse(age %in% r.min:r.full, 1 - (r.full - age) * 0.03, 0) ) # reduction factor for early retirement benefits. Early retirement has a penalty factor on benefit. 
                       ),
#          
#           gx.r = ifelse(age > r.full, 1, ifelse(age %in% r.min:r.full, 1 - (r.full - age) * 0.03, 0)), # reduction factor for early retirement benefits. Early retirement has a penalty factor on benefit. 

         
         Bx.r  = gx.r * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
         TCx.r = lead(Bx.r) * qxr.a * lead(ax.r) * v,  # term cost of retirement at the internal retirement age x (start to claim benefit at age x + 1)
         # TCx.r = Bx.r * qxr.a * ax,
         PVFBx.r  = c(get_PVFB(pxT[age <= r.max], v, TCx.r[age <= r.max]), rep(0, max.age - r.max)),

         ## NC and AL of UC
         # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
         # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
         
         # NC and AL of PUC
         TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
         NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
         ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD = ifelse(age < r.max, PVFBx.r[age == min(age)]/ayx[age == r.max], 0),
         ALx.EAN.CD = PVFBx.r - NCx.EAN.CD * axR,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP = ifelse(age < r.max, sx * PVFBx.r[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
         PVFNC.EAN.CP = NCx.EAN.CP * axRs,
         ALx.EAN.CP = PVFBx.r - PVFNC.EAN.CP
         
  ) 

#   x <- liab.active %>% filter(start.year == 1, ea == 21)


#*************************************************************************************************************
#                          AL for retirees                        #####                  
#*************************************************************************************************************

# Calculate AL and benefit payment for retirees having retired at different ages. 
liab.retiree <- rbind(
  # grids for initial retirees in year 1
  expand.grid(ea         = r.min - 1,
              age.retire = .benefit$age, # This ensures that year of retirement is year 1. 
              #age.retire = r.min,
              start.year = 1 - (.benefit$age - (r.min - 1)),
              age        = range_age[range_age >= r.min]) %>%
    # mutate(year.retire = start.year + age.retire - ea) %>%  
    filter(age >= ea + 1 - start.year),  
  
  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < r.max],
              age.retire = r.min:r.max,
              start.year = (2 - (r.max - min(range_ea))):nyear,
              age        = range_age[range_age >=r.min]) %>%
    # mutate(year.retire = start.year + age.retire - ea) %>%   
    filter(age >= ea, 
           age.retire >= ea,
           age >= age.retire,
           start.year + (age.retire - ea) >= 2, # retire after year 2
           start.year + age - ea >= 2) 
) %>%
  data.table(key = "start.year,ea,age.retire,age") 

liab.retiree <- liab.retiree[!duplicated(liab.retiree %>% select(start.year, ea, age, age.retire )),]


liab.retiree <- merge(liab.retiree,
                      select(liab.active, start.year, year, ea, age, Bx, ax, ax.r, COLA.scale, benefit, gx.r) %>% data.table(key = "ea,age,start.year"), 
                      all.x = TRUE, by = c("ea", "age","start.year")) %>% 
  arrange(start.year, ea, age.retire)


liab.retiree %<>% as.data.frame  %>% # filter(start.year == -41, ea == 21, age.retire == 65) %>% 
  # filter(ea == 54) %>% 
  group_by(start.year, ea, age.retire) %>%  
  mutate(
    year.retire = start.year + age.retire - ea,
    Bx = ifelse(is.na(Bx), 0, Bx),
    B.r   = ifelse(year.retire < 2,
                   benefit[year == 1] * COLA.scale / COLA.scale[year == 1],                          # Benefits for initial retirees
                   (gx.r * Bx)[age == age.retire] * COLA.scale / COLA.scale[age == age.retire]),
    ALx.r = B.r * ax.r  # Liability for remaining retirement benefits.
    
  ) %>% ungroup %>% 
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  select(year, ea, age, year.retire,  B.r, ALx.r)



# young plan 
#  liab.retiree %>% filter( ea %in% 21, age.retire == 65, year.retire == 3)
#  
#  liab.retiree %>% data.frame %>% mutate(x = Bx * gx.r, y = cumsum(gx.r)) %>% filter(ea == 21)
#  liab.retiree %>% data.frame %>% filter(ea == 21) %>% mutate(B.r = (gx.r * Bx)[age == age.retire] * COLA.scale / COLA.scale[age == age.retire]) %>% filter(ea == 21)
#  
#  liab.retiree %>% mutate(x = Bx * gx.r, y = cumsum(gx.r)) %>% filter(ea == 21)
#  liab.retiree %>% filter(ea == 21) %>% mutate(x = Bx * gx.r, y = cumsum(gx.r))
#  
#  liab.retiree %>% na2zero %>% group_by(start.year, ea, age.retire) %>% mutate(x = Bx * gx.r, y = cumsum(gx.r)) %>% filter(ea == 21) 
#  liab.retiree %>% group_by(ea) %>% na2zero %>% mutate(x = Bx * gx.r, y = cumsum(gx.r)) %>% filter(ea == 21)
#  
# 
#  data.frame()


# underfunded plan
# liab.retiree %>% filter( ea %in% 20, age.retire == 64, year.retire == 3)



######################################

## young plan

# start.year, ea, age.retire
# [41, 20:21, 65]: 0 
# [.,  21,    . ]: 0 


# [41, 20:22, 65]:OK
# [. , 20:21, 65]:OK
# [. , 20:22, 65]:OK


## underfunded plan


#*************************************





#*************************************************************************************************************
#                          AL and NC of deferred benefits for actives                        #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begin to receive deferred retirement benefit at r.full.
# Notes on deferred retirement benefits for vested terms. 
  # 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
  #    at age r.max
  # 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
  #    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the 
  #    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balance. 
  #    
  # WARNING: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min   

liab.active %<>% 
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0), # actives become vested after reaching v.yos years of yos
         Bx.v  = gx.v * Bx,
 
         TCx.v   = ifelse(ea < r.full, Bx.v * qxt.a * lead(px_r.full_m) * v^(r.full - age) * ax[age == r.full], 0), # term cost of vested termination benefits. We assume term rates are 0 after r.full. 
         PVFBx.v = ifelse(ea < r.full, c(get_PVFB(pxT[age < r.full], v, TCx.v[age < r.full]), rep(0, max.age - r.full + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).         
         
#        TCx.v = Bx.v * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max],  # term cost of vested termination benefits
#        PVFBx.v = c(get_PVFB(pxT[age < r.max], v, TCx.v[age < r.max]), rep(0, max.age - r.max + 1)),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).         

         # NC and AL of PUC
         TCx.vPUC = TCx.v / (age - min(age)),
         NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
          
         # NC and AL of EAN.CD
         #NCx.EAN.CD.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/ayx[age == r.min], 0), # Note that NC is 0 after age r.min - 1
         NCx.EAN.CD.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/ayx[age == r.full], 0), # for testing spreading NC.v up to r.full
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,
         
         # NC and AL of EAN.CP
         #NCx.EAN.CP.v = ifelse(age < r.min, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.min]) * sx, 0),  # Note that NC is 0 after age r.min - 1
         NCx.EAN.CP.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.full]) * sx, 0),  # for testing spreading NC.v up to r.full
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
         ) %>% 
  ungroup %>% select(start.year, year, ea, age, everything()) 

# x <- liab.active %>% filter(start.year == 1, ea == 20)



#*************************************************************************************************************
#                          AL for vested terminatede members                        #####                  
#*************************************************************************************************************

# Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(start.year = (1 - (r.max - 1 - min.age)):nyear, ea = range_ea[range_ea < r.full], age = range_age, age.term = range_age[range_age < r.full]) %>% # start year no longer needs to start from -89 if we import initial benefit data.
  filter(start.year + max.age - ea >= 1, age >= ea, age.term >= ea) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 
liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, ax, COLA.scale, pxRm, px_r.full_m) %>% data.table(key = "ea,age,start.year"), 
                   all.x = TRUE, by = c("ea", "age","start.year"))

liab.term %<>% as.data.frame %>% 
  # arrange(start.year, ea, age.term, age) %>% # Very slow. Uncomment it only when we want to examine liab.term.
  group_by(start.year, ea, age.term) %>% 
  mutate(year.term = year[age == age.term],
         #year.term = year - (age - age.term),
#          B.v   = ifelse(age >= r.max, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.max], 0),  # Benefit payment after r.max  
#          ALx.v = ifelse(age <  r.max, Bx.v[age == unique(age.term)] * ax[age == r.max] * pxRm * v^(r.max - age),
#                         B.v * ax)  
                         
         B.v   = ifelse(age >= r.full, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.full], 0),  # Benefit payment after r.full  
         ALx.v = ifelse(age <  r.full, Bx.v[age == unique(age.term)] * ax[age == r.full] * px_r.full_m * v^(r.full - age),
                        B.v * ax)  
         
  ) %>% 
  ungroup  %>% 
  select(-start.year, -age.term, -Bx.v, -ax, -COLA.scale, -pxRm) %>%
  # select(-age.term, -Bx.v, -ax, -COLA.scale, -pxRm) %>% 
  filter(year %in% 1:nyear)
# liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])






# Choosing AL and NC variables corresponding to the chosen acturial methed
ALx.method   <- paste0("ALx.", actuarial_method)
NCx.method   <- paste0("NCx.", actuarial_method)
ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")

var.names <- c("sx", ALx.method, NCx.method, ALx.v.method, NCx.v.method, "PVFBx.r")
liab.active %<>% 
  filter(year %in% 1:nyear) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx" = ALx.method, "NCx" = NCx.method, "ALx.v" = ALx.v.method, "NCx.v" = NCx.v.method) # Note that dplyr::rename_ is used. 
  # liab[-(1:3)] <- colwise(na2zero)(liab[-(1:3)])


return(list(active = liab.active, retiree = liab.retiree, term = liab.term))
}



start_time_liab <- proc.time()

liab <- get_IndivLiab()

Time_liab <- proc.time() - start_time_liab
Time_liab


# Some comparisons

# microbenchmark(
# ll2[["NCx.PUC"]]$`3` + ll2[["NCx.PUC"]]$`3`,
# ll2$NCx.PUC$`3` + ll2$NCx.PUC$`3`, times = 1000
# )      
# Result: appox. the same

# microbenchmark( lldf %>% filter(variable == "sx", year == 1),
#                 ll2[["sx"]][[1]] # this is much f
#                 , times = 100)  
# Result: latter is much faster. 

# compare year 3 for variable NCx.PUC
# ll2$NCx.PUC$`3`


# x <- data.frame(a = 1:2, b = 3:4)
# y <- "a"
# rename_(x, "A" = y )

# liab$active %>% filter(year == 46, ea == 20, age %in% 64:65 )
# 
# liab$retiree %>% filter(year %in% 43:44, ea == 20, age %in% 62:63, year.retire == 43)
# 
# pop$retired[,,43,43] %>% sum
# pop$retired[,,44,43] %>% sum
# 
# 
# 35635*0.8487
# 35991*0.8431
# 
# 35635*11.832
# 
# 421634 * 0.8487 # ALx 357841 vs 355318
# 
# (357841-355318)*1.075
# 
# # Now look at how the liability is calculated in liab$active
# liab$active %>% filter(year %in% 43:44, ea == 20, age %in% 62:63)
# # AL = 418657
# 418657*0.8487
#  # AL is calculated as PVFB - PVFNC. 
#  # The liability would match if AL is only calculated as PVFB:
#    # 421634 * 0.8487 = 357841
#  # But there is non-negative normal cost 
# 
# x <- liab$active
#    

