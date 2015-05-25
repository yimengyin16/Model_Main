# This script import salary and retirement benefit data.


## Outputs
 # salary:  complete historical and prospect salary by age and ea.
 # benefit: average benefit payment in year 1, by age and ea. 

load("Data/example_Salary_benefit.RData")

#*************************************************************************************************************
#                                        Create complete salary scale                                    #####                  
#*************************************************************************************************************


# Salary scale for all starting year
SS.all <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = min.age:(r.max - 1)) %>% 
  filter(age >= ea, start.year + r.max - 1 - ea >= 1 ) %>%
  arrange(start.year, ea, age) %>%
  left_join(SS) %>% 
  group_by(start.year, ea) %>% 
  mutate(year = start.year + (age - ea),
         growth.start = (1 + infl)^(start.year - 1), # assume starting salary grows at the rate of inflation for all entry ages 
         scale = cumprod(ifelse(age == ea, 1, lag(1 + sscale.hist.rate))),
         scale = ifelse(start.year <= 1, scale/scale[year == 1],
                        scale * growth.start)
  )




#*************************************************************************************************************
#                                        Create complete salary history                                  #####                  
#*************************************************************************************************************

salary <- SS.all %>% left_join(avgpay) %>% 
  group_by(start.year, ea) %>% 
  mutate(sx = ifelse(start.year <= 1, salary[year == 1]* scale, 
                     salary[age == ea]* scale)) %>% 
  select(start.year, ea, age, year, sx)

# Check the growth of starting salary before year 1
fn <- function(x) (x[length(x)]/x[1])^(1/(length(x) - 1)) - 1

# starting salary over time
start.pay <- salary %>% filter(age == ea, start.year <=1) %>% ungroup %>%  arrange(ea, year) %>% 
  select(ea, year, sx)
start.pay %>%  kable
# start.pay %>% ggplot(aes(x = year + 2012, y = sx, colour = factor(ea))) + geom_line(size = 1)


# average growht rates of starting salary 
salary %>% filter(age == ea, start.year <=1) %>% ungroup %>% group_by(ea) %>%  arrange(ea, year) %>% 
  select(ea, year, sx) %>% filter(year <= -12 ) %>% summarize(g = fn(sx) )%>% kable


# 1. Starting salary generally has in rising trend, while drops after the Great Recession.(The rising trend in the end may be due to the imputation)
# 2. The growth rates before 40 are around 3% ~ 4%, which is approximately the average inflation rate
# 3. The growth rates of starting salary are much lower for ages greater 40, and even become negative in 50s.
#    Even after taking into account that starting salary at higher entry ages have shorter history and hencea are
#    affected by the recent recession more, their growth curves are still flatter than those of lower entry ages.
# 4. Need to be carefull about the observations above since the imputed salary table is used.
# 5. If the observations above are real, we need to make different assumptions about the growth rate of starting
#    salary for different entry ages. The current assumption of a flat growth rate over all entry ages may be inappropriate.


#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************

benefit <- avgben %>% filter(age>=r.max) %>% 
           mutate(year = 1)
# benefit %>% select(-year) %>% spread(age, benefit)


