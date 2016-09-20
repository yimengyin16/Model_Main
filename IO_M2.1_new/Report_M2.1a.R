## Figures and tables for Milestone Report 2.1a Demographics
## Yimeng Yin
## Sep 2016


#*****************************************************
##  Packages and Functions  ####
#*****************************************************


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

source("Functions.R")
source("Functions_Measures.R")

#*****************************************************
##  Combine selected files into a single list.  ####
#*****************************************************
IO_folder <- "IO_M2.1_new"
g.folder  <- "/M2.1a_graphs/"

get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder, "^Outputs_D")

#save(results_all, file = "IO_M2.1_new/results_all.Demo.RData")

#*****************************************************
##  RIG color and theme ####
#*****************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"


RIG.theme <- function(){
             theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
}


#*****************************************************
## Graphs of risk measures ####
#*****************************************************

runs_demo <- c("D1F075-average_gn2",
                  "D1F075-average",
                  "D1F075-average_g2",
                  "D1F075-mature1_gn1",
                  "D1F075-mature2_gn1",
                  "D1F075-immature_g1")

runs_demo_labels <- c("Average, 2% decline",
                      "Average, constant workforce",
                      "Average, 2% growth",
                      "Mature plan 1 (high asset-payroll ratio)",
                      "Mature plan 2 (high normal cost)",
                      "Immature plan")


df_demo <- results_all  %>% 
           filter(runname %in% runs_demo, sim >= 0, year <= 30)

df_demo %<>%   
  select(runname, sim, year, AL, MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR90more  = cumany(FR_MA >= 90),
         ERC_high  = cumany(ERC_PR >= 30), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR90more = 100 * sum(FR90more, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1),
            FR.q25   = quantile(FR_MA, 0.25),
            FR.q50   = quantile(FR_MA, 0.5),
            FR.q75   = quantile(FR_MA, 0.75),
            FR.q90   = quantile(FR_MA, 0.9),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1),
            ERC_PR.q25 = quantile(ERC_PR, 0.25),
            ERC_PR.q50 = quantile(ERC_PR, 0.5),
            ERC_PR.q75 = quantile(ERC_PR, 0.75),
            ERC_PR.q90 = quantile(ERC_PR, 0.9)
  ) %>% 
  ungroup() %>% 
  mutate(runname = factor(runname, 
                          levels = runs_demo, 
                          labels = runs_demo_labels))


demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


## Funded ratio
g.FR40less <- 
  ggplot(df_demo, aes(x = year, y = FR40less, color = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 32)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = demo.color6) +
  labs(x = "Year",
       y = "Probability of falling below 40%",
       title = "Probability that funded ratio falls below 40% \nin any year up to the given year") + 
  guides(col = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.FR40less


g.FR.med <- 
  ggplot(df_demo, aes(x = year, y = FR.q50, color = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2, color = "black") + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,200, 25)) + 
  scale_color_manual(values = demo.color6) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = "Median funded ratio") + 
  guides(col = guide_legend(title = "Plan type")) 
  # theme(# legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.FR.med


## ERC graphs
g.ERC_PR.med <- 
  ggplot(df_demo, aes(x = year, y = ERC_PR.q50, color = runname)) + theme_bw() + RIG.theme() +
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 25)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,60, 5)) + 
  scale_color_manual(values = demo.color6) +
  labs(x = "Year",
       y = "Employer contribution rate (%)",
       title = "Median employer contribution as a percentage of payroll") + 
  guides(col = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.ERC_PR.med


g.ERC_high <- 
  ggplot(df_demo, aes(x = year, y = ERC_high, color = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 50)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,100, 10)) + 
  scale_color_manual(values = demo.color6) +
  labs(x = "Year",
       y = "Probability that employer contribution exceeds 30%",
       title = "Probability that employer contribution exceeds 30% of payroll \nin any year up to the given year ") + 
  guides(col = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.ERC_high


g.ERC_hike <- 
  ggplot(df_demo, aes(x = year, y = ERC_hike, color = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 60)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,100, 10)) + 
  scale_color_manual(values = demo.color6) +
  labs(x = "Year",
       y = "Probability (%)",
       title = "Probability that employer contribution rises by more than \n10% of payroll in any year up to the given year ") + 
  guides(col = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.ERC_hike


g.width = 9
g.height = 5.5

ggsave(paste0(IO_folder, g.folder, "g.demo.FR.med.png"), g.FR.med, width=g.width, height=g.height, units="in")
ggsave(paste0(IO_folder, g.folder, "g.demo.FR40less.png"), g.FR40less, width=g.width, height=g.height, units="in")
ggsave(paste0(IO_folder, g.folder, "g.demo.ERC_PR.med.png"), g.ERC_PR.med, width=g.width, height=g.height, units="in")
ggsave(paste0(IO_folder, g.folder, "g.demo.ERC_high.png"), g.ERC_high, width=g.width, height=g.height, units="in")
ggsave(paste0(IO_folder, g.folder, "g.demo.ERC_hike.png"), g.ERC_hike, width=g.width, height=g.height, units="in")




