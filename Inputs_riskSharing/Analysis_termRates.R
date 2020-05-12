

# Decrements
load("Data/2015-10-07/retrates.rda");  retrates %<>% dplyr::rename(qxr = retrate)
# load("Data/retrates_AZ.RData"); retrates <- retrate_AZ
load("Data/2015-10-07/termrates.rda"); termrates %<>% dplyr::rename(qxt = termrate) # %>% mutate(qxt = 0.5*qxt)
load("Data/2015-10-07/mortality.rda")
load("Data/winklevossdata.rdata") # disability, disability mortaity and early retirement

# Salary scale
load("Data/2015-10-07/salgrowth.rda"); salgrowth %<>% mutate(age = NULL)


# termrates %<>% mutate(qxt = 1.2 * qxt)
# 
# mortality %<>% mutate(qxm = 0.6 * qxm) %>%
#   mutate(qxm.r = qxm)
# 
# retrates %<>% mutate(qxr = qxr * 0.7)
# 
# 
# 
# termrates
# 
# mortality$tablename %>% unique




termrates2 <- 
  bind_rows(
    termrates,
    termrates %>% 
      filter(planname != "term.average") %>% 
      group_by(yos) %>% 
      summarise(qxt = mean(qxt)) %>% 
      mutate(planname = "term.average2")
  )

termrates2_wide <- 
  termrates2 %>% 
  spread(planname, qxt) %>% 
  mutate(term.average3 = term.average * 1.2)

