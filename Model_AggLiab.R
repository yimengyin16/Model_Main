#*************************************************************************************************************
#                                    Calculating total AL, NC and benefits ####
#*************************************************************************************************************
start_time_prep_loop <-  proc.time()

## Calculate total liabilities, NCs and benefits
#  Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

df_wf_active <- adply(wf_active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, number.a, -ea, -year) %>% 
  mutate(year = f2n(year), age = f2n(age))

df_wf_retired <- adply(wf_retired, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>% 
  gather(age, number.r, -ea, -year) %>% 
  mutate(year = f2n(year), age = f2n(age))

df_wf_term <- expand.grid(ea = range_ea, age = range_age, year = 1:nyear, year.term = 1:nyear)
df_wf_term$number.v <- as.vector(wf_term)



# Join population data frames and liability data frames. 
liab <- left_join(df_wf_active, df_wf_retired) %>% 
  left_join(liab)
liab[-(1:3)] <- colwise(na2zero)(liab[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 
liab %<>%        mutate(ALx.tot =  (ALx + ALx.v) * number.a + ALx.r * number.r, 
                        NCx.tot = (NCx + NCx.v) * number.a,
                        PR.tot  = sx * number.a,
                        B.tot = B * number.r
) %>% 
  group_by(year) %>% 
  summarise(ALx.tot = sum(ALx.tot, na.rm = TRUE), 
            NCx.tot = sum(NCx.tot, na.rm = TRUE),
            PR.tot  = sum(PR.tot,  na.rm = TRUE),
            B.tot   = sum(B.tot, na.rm = TRUE)) %>% 
  as.matrix # extracting elements from matrices is much faster than from data.frame


# Save 10 seconds by using data.table to merge
liab.term  <- data.table(liab.term, key = "ea,age,year,year.term")
df_wf_term <- data.table(df_wf_term, key = "ea,age,year,year.term")
liab.term  <- merge(df_wf_term, liab.term, by = c("ea", "age","year", "year.term"), all.x = TRUE)
liab.term  <- as.data.frame(liab.term)

# liab.term <-  left_join(df_wf_term, liab.term)
# liab.term[c("B.v", "ALx.v")] <- colwise(na2zero)(liab.term[c("B.v", "ALx.v")])  

liab.term %<>%mutate(ALx.tot.v = ALx.v * number.v,
                     B.tot.v   = B.v  * number.v) %>% 
  group_by(year) %>% 
  summarise(ALx.tot.v = sum(ALx.tot.v, na.rm = TRUE),
            B.tot.v   = sum(B.tot.v  , na.rm = TRUE)) %>% 
  as.matrix

end_time_prep_loop <-  proc.time()