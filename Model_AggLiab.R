
get_AggLiab <- function(  .liab   = liab, 
                          .pop   = pop, 
                          .paramlist = paramlist,
                          .Global_paramlist = Global_paramlist){

# This function calculates total AL, NC and benefits.

  
# Run the section below when developing new features.  
  # .liab   = liab
  # .pop   = pop
  # .paramlist = paramlist
  # .Global_paramlist = Global_paramlist

  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())
  


#*************************************************************************************************************
#                                     ## Liabilities and NCs for actives   ####
#*************************************************************************************************************
 
# Join population data frames and liability data frames. 
.liab$active <- left_join(.pop$active, .liab$active) # %>% left_join(new_retirees)
.liab$active[-(1:3)] <- colwise(na2zero)(.liab$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 

.liab$active %<>%  
              mutate(ALx.a.tot = ALx * number.a,  # New retirees should be included when calculating liabilities
                     ALx.v.tot = ALx.v * number.a,
                     ALx.av.tot  = ALx.a.tot + ALx.v.tot,
                     
                     NCx.a.tot = NCx * number.a,
                     NCx.v.tot = NCx.v * number.a,
                     NCx.av.tot = NCx.a.tot + NCx.v.tot,
                     
                     PR.tot  = sx * number.a,
                     
                     runname = runname)

active.agg <- .liab$active %>%  
               group_by(year) %>% 
               summarise(
                        ALx.a.sum = sum(ALx.a.tot, na.rm = TRUE),
                        ALx.v.sum = sum(ALx.v.tot, na.rm = TRUE),
                        ALx.av.sum = sum(ALx.av.tot, na.rm = TRUE), 
                        
                        NCx.a.sum = sum(NCx.a.tot, na.rm = TRUE),
                        NCx.v.sum = sum(NCx.v.tot, na.rm = TRUE),
                        NCx.av.sum   = sum(NCx.av.tot, na.rm = TRUE),
                        
                        PR.sum    = sum(PR.tot,  na.rm = TRUE),
                        
                        nactives  = sum(number.a,  na.rm = TRUE)) %>% 
               # mutate(runname  = runname) %>% 
               as.matrix # extracting elements from matrices is much faster than from data.frame


#*************************************************************************************************************
#                                     ## Liabilities and benefits for retirees   ####
#*************************************************************************************************************

.liab$retiree  <- data.table(.liab$retiree, key = "ea,age,year,year.retire")
.pop$retired   <- data.table(.pop$retired,  key = "ea,age,year,year.retire")
.liab$retiree  <- merge(.pop$retired, .liab$retiree, by = c("ea", "age","year", "year.retire"), all.x = TRUE)
.liab$retiree  <- as.data.frame(.liab$retiree)


.liab$retiree %<>% 
  mutate(ALx.r.tot = ALx.r * number.r,
         B.r.tot   = B.r   * number.r,
         runname = runname)
  
retiree.agg <- .liab$retiree %>% 
  group_by(year) %>% 
  summarise(ALx.r.sum   = sum(ALx.r.tot, na.rm = TRUE),
            B.r.sum     = sum(B.r.tot  , na.rm = TRUE),
            nretirees   = sum(number.r , na.rm = TRUE)) %>% 
            # mutate(runname = runname) %>% 
            as.matrix





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

.liab$term %<>% 
            mutate(ALx.v.tot = ALx.v * number.v,
                   B.v.tot   = B.v  * number.v,
                   runname = runname)
            
  
term.agg <- .liab$term %>% 
             group_by(year) %>% 
            summarise(ALx.v.sum   = sum(ALx.v.tot, na.rm = TRUE),
                      B.v.sum     = sum(B.v.tot  , na.rm = TRUE),
                      nterms      = sum(number.v  , na.rm = TRUE)) %>% 
            # mutate(runname = runname) %>% 
            as.matrix


return(list(active = active.agg, 
            retiree = retiree.agg,  
            term = term.agg, 
            
            ind_active  = if(paramlist$save.indiv) .liab$active  else "Not saved", 
            ind_retiree = if(paramlist$save.indiv) .liab$retiree else "Not saved",
            ind_term    = if(paramlist$save.indiv) .liab$term    else "Not saved"))
}

start_time_prep_loop <-  proc.time()

AggLiab <- get_AggLiab()

end_time_prep_loop <-  proc.time()
Time_prep_loop <- end_time_prep_loop - start_time_prep_loop

