#*************************************************************************************************************
#                                    Calculating total AL, NC and benefits ####
#*************************************************************************************************************

get_AggLiab <- function(  .liab   = liab, 
                          .pop   = pop, 
                          .paramlist = paramlist,
                          .Global_paramlist = Global_paramlist){

#   .liab   = liab 
#   .pop   = pop
#   .paramlist = paramlist
#   .Global_paramlist = Global_paramlist
#   
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())
  
  
## Calculate total liabilities, NCs and benefits
#  Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

.pop$active <- adply(.pop$active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, number.a, -ea, -year) %>% 
  mutate(year = f2n(year), age = f2n(age))

.pop$retired <- adply(.pop$retired, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>% 
  gather(age, number.r, -ea, -year) %>% 
  mutate(year = f2n(year), age = f2n(age))

#df_wf_term <- expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.term = 1:nyear)
#df_wf_term$number.v <- as.vector(wf_term)

.pop$term <- data.frame(expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.term = 1:nyear),
                       number.v = as.vector(.pop$term))

# summarize term across termination year. Resulting data frame will join .Liab$active as part of the output. 
 
term_reduced <- .pop$term %>% group_by(year, age) %>% summarise(number.v = sum(number.v, na.rm = TRUE))


# Join population data frames and liability data frames. 
.liab$active <- left_join(.pop$active, .pop$retired) %>% 
                left_join(.liab$active) #  %>% 
                # left_join(term_simple)
.liab$active[-(1:3)] <- colwise(na2zero)(.liab$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 
active.agg <- .liab$active %>%  
              mutate(ALx.a.tot = ALx * number.a,
                     ALx.v.tot = ALx.v * number.a,
                     ALx.r.tot = ALx.r * number.r,
                     ALx.tot   = (ALx + ALx.v) * number.a + ALx.r * number.r, 
                     
                     NCx.a.tot = NCx * number.a,
                     NCx.v.tot = NCx.v * number.a,
                     NCx.tot = (NCx + NCx.v) * number.a,
                     
                     PR.tot  = sx * number.a,
                     B.tot   = B * number.r) %>% 
              group_by(year) %>% 
              summarise(
                        ALx.a.tot = sum(ALx.a.tot, na.rm = TRUE),
                        ALx.v.tot = sum(ALx.v.tot, na.rm = TRUE),
                        ALx.r.tot = sum(ALx.r.tot, na.rm = TRUE),
                        ALx.tot = sum(ALx.tot, na.rm = TRUE), 
                        
                        NCx.a.tot = sum(NCx.a.tot, na.rm = TRUE),
                        NCx.v.tot = sum(NCx.v.tot, na.rm = TRUE),
                        NCx.tot = sum(NCx.tot, na.rm = TRUE),
                        
                        PR.tot  = sum(PR.tot,  na.rm = TRUE),
                        B.tot   = sum(B.tot,   na.rm = TRUE),
                        
                        nactives  = sum(number.a,  na.rm = TRUE),
                        nretirees = sum(number.r, na.rm = TRUE)
                        ) %>% 
              as.matrix # extracting elements from matrices is much faster than from data.frame


# x <- active.agg %>% filter(year==1)
# x$ALx.tot %>% which.max
# x[2070,]


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

return(list(active = active.agg, term = term.agg, ind_act_ret = .liab$active, ind_term = term_reduced))

}

start_time_prep_loop <-  proc.time()

AggLiab <- get_AggLiab()

end_time_prep_loop <-  proc.time()
Time_prep_loop <- end_time_prep_loop - start_time_prep_loop

