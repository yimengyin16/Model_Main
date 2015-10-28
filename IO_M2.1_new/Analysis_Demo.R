# Analysis of Plan Demographics
# Yimeng Yin
# 10/27/2015

rm(list = ls())

library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(magrittr) # to use %<>%
library(zoo)
library(grid)
library(gridExtra)
library(stringr)
library("readr")
library("readxl")
library(xlsx)

source("Functions.R") 



#****************************************************************************************************
#               ## Loading Data  ####
#****************************************************************************************************

IO_folder   <- "IO_M2.1_new"
runName     <- "D1F075-average"
file_select <- dir(IO_folder, pattern = paste0(runName,".RData"))

load(paste0(IO_folder, "/", file_select))


#****************************************************************************************************
#               ## Computing remaining working life and remaining lifetime.   ####
#****************************************************************************************************


get_expLife <- function(p, age){
  # get expected remaining work life/life time.
  # p: probability of survival at age x
  len <- length(age)
  exp.life <- numeric(len)
  
  for(i in seq_along(age)){
    p.i   <- p[i:len]
    age.i <- age[i:len]  
    
    exp.life[i] <- sum(cumprod(p.i) * c( 1 - p.i[-1], 0) * seq_along(age.i)) # prob of quitting the workforce at age x * age x
  }
  
  return(exp.life)
}



df_workLife <- expand.grid(ea = 20:74, age = 20:74) %>% 
               filter(age >= ea) %>% 
               left_join(outputs_list$decrement) %>% 
               select( ea, age, pxT) %>% 
               group_by(ea) %>% 
               mutate(workLife = get_expLife(pxT, age))

df_workLife.sum <- outputs_list$ind_active %>% 
                   left_join(df_workLife) %>% group_by(year) %>% 
                   summarise(workLife.avg = weighted.mean(workLife, number.a, na.rm = TRUE))

df_workLife.sum %>% print(n = 60)                   



df_lifeTime <- expand.grid(ea = 20, age = 50:120) %>% 
  left_join(outputs_list$decrement) %>% 
  select(age, pxm) %>% 
  mutate(lifeTime = get_expLife(pxm, age))

df_lifeTime.sum <- outputs_list$ind_retiree %>% 
  left_join(df_lifeTime) %>% group_by(year) %>% 
  summarise(lifeTime.avg = weighted.mean(lifeTime, number.r, na.rm = TRUE))

df_lifeTime.sum %>% print(n = 60)                   



#****************************************************************************************************
#               ## Summary measures   ####
#****************************************************************************************************
# To be done:
# - average remaining working life of actives
# - average remaining lifetime of serive retirees/beneficiaries
# - more flow measures for terms. 

outputs_list$ind_active %>% head
outputs_list$decrement
outputs_list$demo_summary %>% select(starts_with("new"))

# average ages/yos/ea
outputs_list$demo_summary %>% left_join(df_workLife.sum) %>% 
                              left_join(df_lifeTime.sum) %>%
                              select(ends_with("avg"))
outputs_list$demo_summary  %>% select(ar.ratio, ab.ratio)

# Flows for actives
outputs_list$demo_summary  %>% select(runname,year,
                                      tot_actives,
            
                                     new_entrants,
                                     new_retirees,
                                     new_terms,  # Including not vested.
                                     newDeath.act,
                                     newDisb.act,
                                    
                                     newEnt_actives,
                                     newRet_actives,
                                     newTerm_actives,
                                     newDeath.act_actives,
                                     newDisb.act_actives)

# Flows for retirees
outputs_list$demo_summary %>% select(tot_retirees,
  
                                     new_retirees,
                                     newDeath.ret,
                                     newDeath.ret_retirees)

# Flows for terms
outputs_list$demo_summary %>% select(tot_terms,
                                     tot_termsInact,
                                     tot_termsBen,
                                     
                                     new_terms,
                                     new_termsInact,
                                     new_termsBen)



#****************************************************************************************************
#               ## AL by age and ea ####
#****************************************************************************************************

df_AL.NC_act <- outputs_list$ind_active %>% 
                select(year, ea, age, number.a, sx, ALx, NCx, ALx.v, NCx.v) %>% 
                mutate(AL_sx = 100 * (ALx + ALx.v) / sx,
                       NC_sx = 100 * (NCx + NCx.v) / sx)

## AL_sx rate
 # Does it change a lot over time?
df_AL.NC_act %>% filter(year %in% c(1, 10, 30, 60), age < 75) %>% select(year, ea, age, AL_sx) %>% spread(year,AL_sx) 
 # Almost(exactly?) the same across years. Use year 30 only from here on 
df_AL_sx <- df_AL.NC_act %>% filter(year == 30, age < 75) %>% select(year, ea, age, AL_sx) 

# Notes:
#   What leads to high AL_payroll ratio?
#   - large proportion of young entrants. 
#   - high survival rates, especially for young workers. 
#   - What is the impact of salary scale? flat, steep, growh
#   - What is the impact of growth of starting salary?


# heat map 
df_AL_sx %>%  
  ggplot(aes(x = age, y = ea ,fill = AL_sx)) + geom_tile( colour = "white") + 
  scale_fill_gradient(low = "green", high = "red")
# line plot
df_AL_sx %>% filter(ea %in% seq(0,70,5)) %>% 
             ggplot(aes(x = age, y = AL_sx ,color = factor(ea))) + 
             geom_line() + geom_point()

## NC_sx rate
df_NC_sx <- df_AL_act %>% filter(year == 30, age ==74, ea %in% 20:74) %>% select(year, ea, age, NC_sx)
df_NC_sx %>% ggplot(aes(x = ea, y = NC_sx)) + geom_bar(stat = "identity", size)




#****************************************************************************************************
#               ## AL-payroll ratio ####
#****************************************************************************************************

df_AL.ratios <- outputs_list$results %>% filter(sim == 1) %>% 
                  select(runname, sim, year, AL.act, AL.ret, AL.term, AL.Ben, AL_PR, AL.act_PR, AL.ret_PR, AL.term_PR, PR)
                  
df_AL.ratios %>% kable(digits = 3)

# Notes:
#   - The difference between AL.Ben_PR and AL.ret_PR becomes larger over time(~100% after year 40). This means AL of vested
#     terms - recieving and not recieving benefits - accounts for a very large proportion of total liability. I wonder whether 
#     this is a normal case in real plans, and whether this is due to no EEC refunds in the model. 



