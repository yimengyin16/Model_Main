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
#               ## Loading data ####
#****************************************************************************************************

IO_folder <- "IO_M2.1_new"
load(paste0(IO_folder, "/Analysis_Demo/Demo_results.RData"))
load(paste0(IO_folder, "/Analysis_Demo/Demo_metrics.RData"))
load(paste0(IO_folder, "/Analysis_Demo/DemoSum_all.RData"))


#****************************************************************************************************
#               ## Table of descriptive statistics for the report ####
#****************************************************************************************************

tab.vars <- c("runname", "year", 
              "actives_age.avg",
              "actives_yos.avg",
              "actives_ea.avg",
              "retirees_age.avg",
              #"workLife.avg",
              #"lifeTime.avg",
              "ab.ratio")  

tab.runname <- c("average_gn2",
                 "average_gn1",
                 "average",
                 "average_g1",
                 "average_g2",
                 "mature1_gn1",
                 "mature2_gn1",
                 "immature_g1")
tab.runname <- paste0("D1F075-", tab.runname)


tab_demoSum_y1 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
  filter(runname %in% tab.runname, year %in% c(1))
  

tab_demoSum_y30 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
  filter(runname %in% tab.runname, year %in% c(30))

tab_demoSum_y60 <- df_demo_summary_all %>% select(one_of(tab.vars)) %>% 
  filter(runname %in% tab.runname, year %in% c(60)) 




tab_demoSum_y1 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_MA) %>% 
                                filter(runname %in% tab.runname, year == 1) %>% 
                                group_by(runname) %>% 
                                summarise(NC_PR = median(NC_PR),
                                          MA_PR = median(MA_PR)/100,
                                          ExF_MA = median(ExF_MA)/100))
tab_demoSum_y1 %<>% #select(-sim) %>% 
                    mutate(runname = factor(runname, levels = tab.runname)) %>% 
                    arrange(runname)
tab_demoSum_y1 %>% kable(digits = 2)



tab_demoSum_y30 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_MA) %>% 
                                 filter(runname %in% tab.runname, year == 30) %>% 
                                 group_by(runname) %>% 
                                 summarise(NC_PR = median(NC_PR),
                                           MA_PR = median(MA_PR)/100,
                                           ExF_MA = median(ExF_MA)/100))
tab_demoSum_y30 %<>% # select(-sim) %>% 
                     mutate(runname = factor(runname, levels = tab.runname)) %>% 
                     arrange(runname)
tab_demoSum_y30 %>% kable(digits = 2)



tab_demoSum_y60 %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_MA) %>% 
                                 filter(runname %in% tab.runname, year == 60) %>% 
                                 group_by(runname) %>% 
                                 summarise(NC_PR = median(NC_PR),
                                           MA_PR = median(MA_PR)/100,
                                           ExF_MA = median(ExF_MA)/100))
tab_demoSum_y60 %<>% select(-sim) %>% 
                     mutate(runname = factor(runname, levels = tab.runname)) %>% 
                     arrange(runname)
tab_demoSum_y60 %>% kable(digits = 1)



write.xlsx2(tab_demoSum_y1, file = paste0(IO_folder,"/", "Analysis_Demo/tab_demoSum.xlsx"), sheetName = "year1_new1", append = T)
write.xlsx2(tab_demoSum_y30, file = paste0(IO_folder,"/", "Analysis_Demo/tab_demoSum.xlsx"), sheetName = "year30_new1", append = T)




#****************************************************************************************************
#               ## Graphs of Descriptive statistics of plans for the report ####
#****************************************************************************************************

runName <- "D1F075-mature2"
runName_list <- c("D1F075-average",
                  "D1F075-mature1_gn1", 
                  "D1F075-mature2_gn1", 
                  "D1F075-immature_g1")

runName_labels <- c("Average Plan",
                  "Mature Plan 1", 
                  "Mature Plan 2", 
                  "Immature Plan")


p_ageDist <- 
df_ageDist_all %>% filter(runname %in% runName_list, year %in% c(1) , age <= 74) %>%
                   mutate(runname = factor(runname, levels = runName_list, label = runName_labels)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + theme_bw() + 
  geom_bar(stat = "identity", fill = "skyblue3", color = "skyblue3") + 
  scale_x_discrete(breaks = seq(20, 75, 10)) + 
  scale_y_continuous(breaks = seq(0, 4, 0.5)) + 
  coord_cartesian(xlim = c(20, 75)) + 
  labs(title = "Age Distribution of Actives", y = "Percent", x = "Age") + facet_grid(.~runname)
p_ageDist

p_eaDist <- 
  df_eaDist_all %>% filter(runname %in% runName_list, year %in% c(1) , age <= 74) %>%
  mutate(runname = factor(runname, levels = runName_list, label = runName_labels)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + theme_bw() + 
  geom_bar(stat = "identity", fill = "skyblue3", color = "skyblue3") + 
  scale_x_discrete(breaks = seq(20, 75, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 0.5)) + 
  coord_cartesian(xlim = c(20, 75)) + 
  labs(title = "Entry Distribution of Actives", y = "Percent", x = "Age") + facet_grid(.~runname)
p_eaDist



p_yosDist <- 
  df_yosDist_all %>% filter(runname %in% runName_list, year %in% c(1) , yos <= 55) %>%
  mutate(runname = factor(runname, levels = runName_list, label = runName_labels)) %>% 
  ggplot(aes(x = yos,  y = pct_age)) + theme_bw() + 
  geom_bar(stat = "identity", fill = "skyblue3", color = "skyblue3") + 
  scale_x_discrete(breaks = seq(0, 55, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  coord_cartesian(xlim = c(-2, 52)) + 
  labs(title = "Year of Service Distribution of Actives", y = "Percent", x = "Year") + facet_grid(.~runname)
p_yosDist




p_entDist <- 
  df_entDist_all %>% filter(runname %in% runName_list) %>%
  mutate(runname = factor(runname, levels = runName_list, label = runName_labels)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + theme_bw() + 
  geom_bar(stat = "identity", fill = "skyblue3", color = "skyblue3") + 
  scale_x_discrete(breaks = seq(20, 75, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  coord_cartesian(xlim = c(20, 75)) + 
  labs(title = "Age Distribution of New Entrants", y = "Percent", x = "Age") + facet_grid(.~runname)
p_entDist




ggsave(p_ageDist, file = paste0(IO_folder, "/", "Analysis_Demo/p_ageDist.png"), width = 12, height = 4)
ggsave(p_eaDist, file = paste0(IO_folder, "/", "Analysis_Demo/p_eaDist.png"), width = 12, height = 4)
ggsave(p_yosDist, file = paste0(IO_folder, "/", "Analysis_Demo/p_yosDist.png"), width = 12, height = 4)
ggsave(p_entDist, file = paste0(IO_folder, "/", "Analysis_Demo/p_entDist.png"), width = 12, height = 4)



df_ageDist.r_all %>% filter(runname %in% runName_list, year %in% c(1)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity") + theme_bw() + 
  labs(title = "Age Distribution of Retirees", y = "%") + facet_grid( runname ~ year)







#****************************************************************************************************
#              ## Tables of Measures of FR risk and contribution volatility for the report   ####
#****************************************************************************************************

df_metrics_y30 %<>% select(runname, FR40_y30, ERC_PR.5yMaxChg, PV.ERC_PR) 


runname_risk <- c("D1F075-average_gn2", "D1F075-average_gn1",
                     "D1F075-average", 
                     "D1F075-average_g1", "D1F075-average_g2",
                     "D1F075-mature1_gn1", "D1F075-mature2_gn1",
                     "D1F075-immature_g1")


tab.risk <- df_metrics_y30 %>% filter(runname %in% runname_risk) 

tab.risk %<>% left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_MA) %>% 
                          filter(runname %in% tab.runname, year %in% c(1, 30)) %>% 
                          group_by(runname, year) %>% 
                          summarise(NC_PR = median(NC_PR),
                                    MA_PR = median(MA_PR)/100,
                                    ExF_MA = median(ExF_MA)/100) %>% 
                          gather(variable, value, - runname, -year) %>% 
                          mutate(variable = paste0(variable,"_y",year), year = NULL) %>%
                          spread(variable, value))

tab.risk %<>%  left_join(df_demo_summary_all %>% select(runname, year, ab.ratio) %>% 
                         filter(runname %in% runname_risk, year %in% c(1)))
 



tab.risk %<>% mutate(runname = factor(runname, levels = runname_average)) %>% 
  arrange(runname) %>%
  select(runname, ab.ratio, NC_PR_y1, MA_PR_y1, MA_PR_y30, ExF_MA_y1, ExF_MA_y30, ERC_PR.5yMaxChg, FR40_y30, PV.ERC_PR) 

tab.risk %>% kable(digits = 2)

write.xlsx2(tab.risk, file = paste0(IO_folder,"/", "Analysis_Demo/tab_demoSum.xlsx"), sheetName = "risks_new1", append = TRUE)



#****************************************************************************************************
#              ## Quantile plosts for contribution rate and funded ratio     ####
#****************************************************************************************************

runName_list <- c("D1F075-average",
                  "D1F075-mature1_gn1", 
                  "D1F075-mature2_gn1", 
                  "D1F075-immature_g1")

runName_list <- c("D1F075-average",
                  "D1F075-average_gn1",
                  "D1F075-average_g1"
)


get_quantiles(runName_list, "FR_MA", year.max = 30, qts = c(0.25, 0.5, 0.75)) %>% 
  gather(quantile, value,-runname, - year) %>% 
  ggplot(aes(x = year, y = value, colour = quantile)) + facet_grid(.~runname) + 
  geom_line() + 
  geom_point()
  
  
get_quantiles(runName_list, "ERC_PR", year.max = 30, qts = c(0.25, 0.5, 0.75)) %>% 
  gather(quantile, value,-runname, - year) %>% 
  ggplot(aes(x = year, y = value, colour = quantile)) + facet_grid(.~runname) + 
  geom_line() + 
  geom_point() 



#****************************************************************************************************
#               ## Checking Simulation results  ####
#****************************************************************************************************

# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.
var.display <- c("runname",
                 "year",  
                 "AL",    "AA",   "FR", "NC",    "SC", # "UAAL",
                 "AL_PR", "AL.act_PR", "AL.ret_PR",# "AL.term_PR", "AL.Ben_PR",
                 "NC_PR", #" NC.act_PR", "NC.term_PR", "MA_PR", 
                 #"AL_PR", "NC_PR", "SC_PR", "C_PR", "ERC_PR", 
                 "ExF_MA",   
                 "C", "B", "B.v", "B_PR","PR"    
)

runName_list <- c("D1F075-average", "D1F075-mature1", # "D1F075-mature1_lowB", 
                  "D1F075-mature2", "D1F075-immature")

runName_list <- c("D1F075-average", "D1F075-mature1_gn1", # "D1F075-mature1_lowB", 
                  "D1F075-mature2", "D1F075-immature_g1")

runName_list <- c("D1F075-average", "D1F075-average_g2", "D1F075-average_gn2")

results_all %>% filter(runname %in% runName_list, sim == 0, year %in% c(1,15,30,60)) %>% 
                select(one_of(var.display))


results_all %>% filter(runname %in% runName_list, sim == 0, year %in% c(1, 15, 30,60)) %>% 
  select(one_of(var.display))




#****************************************************************************************************
#               ## Individual AL and NC ####
#****************************************************************************************************

df_AL_sx_all %>% filter(runname == "D1F075-average", ea %in% seq(0,65, 10)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea), shape = factor(ea))) + 
  geom_line() + geom_point(size = 2 ) +
  scale_color_discrete(name = "Entry Age") + 
  scale_shape_discrete(name = "Entry Age") + 
  labs(x = "Age", y = "Accrued Liability as % of Salary",
       title = 'Accrued liability as a percentage of salary \nby age and entry age \nfor workers in an "average plan"') + 
  theme_bw()
  
  

df_NC_sx_all %>% filter(runname == "D1F075-average", ea <=65) %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + 
  geom_bar(stat = "identity", color = "black", size = 0.5, fill = "skyblue2") + 
  coord_cartesian(ylim = c(0, 20), xlim = c(18, 67)) + 
  scale_y_continuous(breaks = seq(0,30, 2), name = "Normal cost as % of salary") + 
  scale_x_discrete(breaks = seq(20, 65, 5), name = "Entry Age") + 
  labs(title = 'Normal cost under Entry Age Normal method \nas a percentage of salary by entry age \n for workers in an "average plan"') + 
  theme_bw()





#****************************************************************************************************
#               ## Detective work on LA-CERA: Initial Benefit.  ####
#****************************************************************************************************
load("Data/2015-10-07/retirees.rda")

retirees  %>% filter(grepl(".fill", planname)) %>% 
              ggplot(aes(x = age, y = benefit, color = planname)) +
              geom_line() + 
              geom_point()

retirees %>% filter(planname == "WA-PERS2-119.fillin")


#****************************************************************************************************
#               ## Detective work on WA-PERS2: new entrants  ####
#****************************************************************************************************

load("Data/2015-10-07/actives.rda")

actives %>% filter(planname == "OH-PERS-85", age - ea <= 4)

actives %>% filter(planname == "OH-PERS-85.fillin.yos", age - ea <= 0, ea >=20) %>% arrange(ea) %>% 
            group_by(ea) %>% 
            summarise(nactives = sum(nactives)) %>% 
            qplot(x = ea, y = nactives, data =., geom = c("line","point"))

actives %>% mutate(year.start = 1 - (age - ea)) %>% filter(planname == "OH-PERS-85.fillin.yos") %>%  
            filter(ea >=20, age - ea <= 4 ) %>% arrange(ea) %>% 
  group_by(ea) %>% 
  summarise(nactives = sum(nactives)) %>% 
  qplot(x = ea, y = nactives, data =., geom = c("line","point"))







#****************************************************************************************************
#               ## Detective work Salary scale  ####
#****************************************************************************************************

load("Data/2015-10-07/salgrowth.rda"); salgrowth %<>% mutate(age = NULL)

salgrowth %>% filter(grepl("yos", planname)) %>% 
  qplot(x = yos, y = salgrowth, color = planname, data = ., geom = c("line","point"))

# Verified: high salary growth lead to high normal cost 






#****************************************************************************************************
#              ## Tables of Measures of FR risk and contribution volatility for the report   ####
#****************************************************************************************************

df_metrics_y30 %<>% select(runname, FR40_y30, ERC_PR.5yMaxChg, PV.ERC_PR) 


runname_average <- c("D1F075-average_gn1", "D1F075-average", "D1F075-average_g1",
                     "D1F075-mature1_gn1", "D1F075-mature2_gn1",
                     "D1F075-immature_g1")


tab.risk <- df_metrics_y30 %>% filter(runname %in% runname_average) 

tab.risk %<>% left_join(results_all %>% filter(runname %in% runname_average, sim == 0, year %in% c(1,30)) %>%
                          select(runname, year, MA_PR, ExF_PR) %>% 
                          gather(variable, value, - runname, -year) %>% 
                          mutate(variable = paste0(variable,"_y",year), year = NULL) %>%
                          spread(variable, value))




tab.risk %>% mutate(runname = factor(runname, levels = runname_average)) %>% 
             arrange(runname) %>%
             select(runname, ERC_PR.5yMaxChg, MA_PR_y1, MA_PR_y30, FR40_y30, ExF_PR_y1, ExF_PR_y30, PV.ERC_PR) %>% 
             kable(digits = 1)


