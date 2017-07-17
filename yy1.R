
load("IO_M2.1_new/Outputs_D1F075-average_yy1.RData")
yy1.actives <- outputs_list$results %>% filter(sim == -1) %>% mutate(runname = "yy1") %>% select(year, B.actives = B)
yy1.actives

load("IO_M2.1_new/Outputs_D1F075-average_yy1.ret.RData")
yy1.retirees <- outputs_list$results %>% filter(sim == -1) %>% mutate(runname = "yy1") %>% select(year, B.retirees = B)
yy1.retirees

yy1 <- left_join(yy1.actives, yy1.retirees) %>% 
  mutate(runnname = "yy1") %>% 
  select(runnname, year, everything())

save(yy1, file = "Benefit_yy1.RData")
