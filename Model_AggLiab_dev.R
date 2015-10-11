
get_AggLiab <- function(  .liab   = liab, 
                          .pop   = pop, 
                          .paramlist = paramlist,
                          .Global_paramlist = Global_paramlist){

# This function calculates total AL, NC and benefits.

  
# Run the section below when developing new features.  
  .liab   = liab
  .pop   = pop
  .paramlist = paramlist
  .Global_paramlist = Global_paramlist

  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())
  

  
#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************
  
## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

.pop$active <- adply(.pop$active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, number.a, -ea, -year) %>% 
  mutate(year = f2n(year), age = f2n(age))


.pop$retired <- data.frame(expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.retire = 1:nyear),
                           number.r = as.vector(.pop$retired))


.pop$term <- data.frame(expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.term = 1:nyear),
                       number.v = as.vector(.pop$term))


# summarize term across termination year. Resulting data frame will join .Liab$active as part of the output. 
 term_reduced <- .pop$term %>% group_by(year, age) %>% summarise(number.v = sum(number.v, na.rm = TRUE))
 

 
#*************************************************************************************************************
#                                     Calculate Demographic Summary Statistics   ####
#*************************************************************************************************************
# This is a digress from the main purpose of this function. Demographic statistics are useful in analyzing 
# the plan characteristics, and the most convenient way to construct these statistics is by using data frames. 

demo_summary <- 
Reduce(merge, list( 
 # Average age of workforce
 # Average year of service of workforce
 # Average entry age of workforce
 .pop$active %>% group_by(year) %>% summarise(actives_age.avg = weighted.mean(age, number.a)),
 .pop$active %>% group_by(year) %>% summarise(actives_ea.avg  = weighted.mean(ea, number.a)),
 .pop$active %>% group_by(year) %>% mutate(yos = age - ea) %>% summarise(actives_yos.avg  = weighted.mean(yos, number.a)),
 
 # Average age of retirees
 .pop$retired %>% group_by(year) %>% summarise(retirees_age.avg = weighted.mean(age, number.r)),
 
 # Total actives, retirees and terminated members. (terms in all status, including not vested.) 
 .pop$active  %>% group_by(year) %>% summarise(tot_actives  = sum(number.a)),
 .pop$retired %>% group_by(year) %>% summarise(tot_retirees = sum(number.r)),
 .pop$term    %>% group_by(year) %>% summarise(tot_terms    = sum(number.v)),
 
 # Total vested terms in benefit status and total vested terms not in benefit status. Note: year - (age - ea) gives the year of entrance. 
 .pop$term %>% filter(age >= r.full & year.term - (year - (age - ea)) >= v.yos) %>% group_by(year) %>% summarise(tot_termsBen    = sum(number.v)),  
 .pop$term %>% filter(age <  r.full & year.term - (year - (age - ea)) >= v.yos) %>% group_by(year) %>% summarise(tot_termsInact  = sum(number.v)),
 
 
 # New entrants
 .pop$active %>% filter(age == ea) %>% group_by(year) %>% summarise(tot_newEntrants = sum(number.a)), 
 
 # New retirees
 .pop$retired %>% filter(year == year.retire) %>% group_by(year) %>% summarise(tot_newRetirees = sum(number.r)),
 
 # New terms (in all status, including not vested.)
 .pop$term %>% filter(year == 1 | year == year.term + 1) %>% group_by(year) %>% summarise(tot_newTerms = sum(number.v)),
 
 # New terms in benefit status.
 .pop$term %>% filter(age == r.full & year.term - (year - (age - ea)) >= v.yos) %>% group_by(year) %>% summarise(tot_newTermsBen = sum(number.v)),
 
 # New terms in not in benefit status  Note: year - (age - ea) gives the year of entrance. 
 .pop$term %>% filter(year == 1 | (year == year.term + 1 & age < r.full & year.term - (year - (age - ea)) >= v.yos)) %>% group_by(year) %>% summarise(tot_newTermsInact = sum(number.v)),
 
 # Number of new death in actives and retirees
 data.frame(year = 1:nyear, newDeath.act = .pop$newDeath.act),
 data.frame(year = 1:nyear, newDeath.ret = .pop$newDeath.ret)

 
 )) %>% 
   mutate(# Ratios
          newEnt_actives  = 100 * tot_newEntrants / tot_actives,
          newRet_actives  = 100 * tot_newRetirees / tot_actives,
          
          newTerm_actives = 100 * tot_newTerms / tot_actives,
          newTermsInact_actives  = 100 * tot_newTermsInact / tot_actives,
          newTermsBen_termsInact = 100 * tot_newTermsBen / tot_termsInact,
          
          newDeath.act_actives  = 100 * newDeath.act / tot_actives,
          newDeath.ret_retirees = 100 * newDeath.ret / tot_retirees, 
          
          ar.ratio = tot_actives / tot_retirees,                  # Active-to-service retiree ratio
          ab.ratio = tot_actives / (tot_retirees + tot_termsBen), # Active-to-beneficiary ratio
          runname = runname) %>% 
   select(runname, everything())


 demo_summary

 
#*************************************************************************************************************
#                                     ## Liabilities and NCs for actives   ####
#*************************************************************************************************************
 
# Join population data frames and liability data frames. 
.liab$active <- left_join(.pop$active, .liab$active) # %>% left_join(new_retirees)
.liab$active[-(1:3)] <- colwise(na2zero)(.liab$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 

active.agg <- .liab$active %>%  
              mutate(ALx.a.tot = ALx * number.a,  # New retirees should be included when calculating liabilities
                     ALx.v.tot = ALx.v * number.a,
                     ALx.av.tot  = ALx.a.tot + ALx.v.tot,
                     
                     NCx.a.tot = NCx * number.a,
                     NCx.v.tot = NCx.v * number.a,
                     NCx.av.tot = NCx.a.tot + NCx.v.tot,
                     
                     PR.tot  = sx * number.a

                     ) %>% 
              group_by(year) %>% 
              summarise(
                        ALx.a.tot = sum(ALx.a.tot, na.rm = TRUE),
                        ALx.v.tot = sum(ALx.v.tot, na.rm = TRUE),
                        ALx.tot.active = sum(ALx.av.tot, na.rm = TRUE), 
                        
                        NCx.a.tot = sum(NCx.a.tot, na.rm = TRUE),
                        NCx.v.tot = sum(NCx.v.tot, na.rm = TRUE),
                        NCx.tot   = sum(NCx.av.tot, na.rm = TRUE),
                        
                        PR.tot    = sum(PR.tot,  na.rm = TRUE),
                        
                        nactives  = sum(number.a,  na.rm = TRUE)
                        ) %>% 
              as.matrix # extracting elements from matrices is much faster than from data.frame



#*************************************************************************************************************
#                                     ## Liabilities and benefits for retirees   ####
#*************************************************************************************************************

.liab$retiree  <- data.table(.liab$retiree, key = "ea,age,year,year.retire")
.pop$retired   <- data.table(.pop$retired,  key = "ea,age,year,year.retire")
.liab$retiree  <- merge(.pop$retired, .liab$retiree, by = c("ea", "age","year", "year.retire"), all.x = TRUE)
.liab$retiree  <- as.data.frame(.liab$retiree)


retiree.agg <- .liab$retiree %>% 
  mutate(ALx.tot.r = ALx.r * number.r,
         B.tot.r   = B.r   * number.r) %>% 
  group_by(year) %>% 
  summarise(ALx.tot.r   = sum(ALx.tot.r, na.rm = TRUE),
            B.tot.r     = sum(B.tot.r  , na.rm = TRUE),
            nretirees   = sum(number.r , na.rm = TRUE))  %>% as.matrix





#*************************************************************************************************************
#                                 ## Liabilities and benefits for vested terms.   ####
#*************************************************************************************************************

# Save 10 seconds by using data.table to merge
.liab$term  <- data.table(.liab$term, key = "ea,age,year,year.term")
.pop$term   <- data.table(.pop$term,  key = "ea,age,year,year.term")
.liab$term  <- merge(.pop$term, .liab$term, by = c("ea", "age","year", "year.term"), all.x = TRUE)
.liab$term  <- as.data.frame(.liab$term)

# liab.term <-  left_join(df_wf_term, liab.term)
# liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])  

term.agg <- .liab$term %>% 
            mutate(ALx.tot.v = ALx.v * number.v,
                   B.tot.v   = B.v  * number.v) %>% 
            group_by(year) %>% 
            summarise(ALx.tot.v   = sum(ALx.tot.v, na.rm = TRUE),
                      B.tot.v     = sum(B.tot.v  , na.rm = TRUE),
                      nterms      = sum(number.v  , na.rm = TRUE)) %>% 
            as.matrix



return(list(active = active.agg, retiree = retiree.agg,  term = term.agg, ind_act_ret = .liab$active, ind_term = term_reduced, 
            demo_summary = demo_summary))

}

start_time_prep_loop <-  proc.time()

AggLiab <- get_AggLiab()

end_time_prep_loop <-  proc.time()
Time_prep_loop <- end_time_prep_loop - start_time_prep_loop

