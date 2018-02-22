# Run D1F075-average_steady in IO_M2.1_new RunControl_M2.1_new.xlsx with nyear = 105 (previously 60) before running the script below.
# Single retirement age

# Check steady state
d.SS <- pop$active %>% filter(year == 102, age <= 64) %>% select(-year)
d.SS2 <- pop$active %>% filter(year == 103, age <= 64) %>% rename(number.a2 = number.a) %>% select(-year)

d <- left_join(d.SS, d.SS2) %>% 
  mutate(diff = number.a2/number.a)
range(d$diff, na.rm = T)
d

# Look at the distributions 
d.SS %<>% mutate(nactives.pct = number.a / sum(number.a),
                 yos = age - ea) %>% 
  rename(nactives = number.a) %>% 
  ungroup() %>% 
  select(ea, age, yos, everything()) %>% 
  arrange(ea, age)

d.SS_byAge <- d.SS %>% group_by(age) %>% 
  summarise(nactives.pct = sum(nactives.pct))
d.SS_byAge

d.SS_byEA <- d.SS %>% group_by(ea) %>% 
  summarise(nactives.pct = sum(nactives.pct))

d.SS_byYos <- d.SS %>% 
  group_by(yos) %>% 
  summarise(nactives.pct = sum(nactives.pct))

# Plot distributions
qplot(x = age, y = nactives.pct,  data = d.SS_byAge )
qplot(x = ea,  y = nactives.pct,  data = d.SS_byEA )
qplot(x = yos,  y = nactives.pct, data = d.SS_byYos )

# Distribution of new entrants
d.newHires <- data.frame(age = 20:64, entrants_dist)
d.newHires

# Distribution of retirees


d.SS.ret  <- pop$retired %>% 
  group_by(year, age) %>% 
  summarise(number.r = sum(number.r)) %>% 
  mutate(nretirees.pct = number.r / sum(number.r)) %>% 
  filter(year == 104, age >=50) %>% ungroup %>% select(-year)

d.SS.ret2 <- pop$retired %>% 
  group_by(year, age) %>% 
  summarise(number.r2 = sum(number.r)) %>% 
  filter(year == 105, age >=50) %>% ungroup %>% select(-year)

d.ret <- left_join(d.SS.ret, d.SS.ret2) %>% 
  mutate(diff = number.r2/number.r)
range(d.ret$diff, na.rm = T)

d.SS.ret
d.SS.ret2

d.SS.ret %<>% mutate(nretirees.pct = number.r/sum(number.r)) %>% rename(nretirees =  number.r)

d.SS.ret %>% head

write.xlsx2(d.SS,file = "Data/Demo_Steady.singleRet.xlsx", sheetName = "SS.actives", row.names = F )
write.xlsx2(d.SS_byAge,file = "Data/Demo_Steady.singleRet.xlsx", sheetName = "SS.actives.ByAge", append = T, row.names = T)
write.xlsx2(d.SS_byYos,file = "Data/Demo_Steady.singleRet.xlsx", sheetName = "SS.actives.ByYos", append = T, row.names = T)
write.xlsx2(d.newHires,file = "Data/Demo_Steady.singleRet.xlsx", sheetName = "Dist_newHires", append = T, row.names = T)
write.xlsx2(d.SS.ret  ,file = "Data/Demo_Steady.singleRet.xlsx", sheetName = "SS.retirees", append = T, row.names = T)




pop$disb[,,101] %>% sum

x <- pop$term_reduced %>% filter(year == 101, age >= 60)

x$number.v %>% sum


outputs_list$results %>% filter(sim == 1) %>% select(sim, year, B.v, B)

x <- 1 + seq(0, 0.5, 0.01) * 2

y <- (1 + seq(0, 0.5, 0.01))^2


x

y

x - y

1 + seq(0, 0.5, 0.01) * 1.2 - (1 + seq(0, 0.5, 0.01))^1.2



 
