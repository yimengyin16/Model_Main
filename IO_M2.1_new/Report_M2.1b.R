## Figures and tables for Milestone Report 2.1b Investment Policy
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
library(xlsx)

library("grid")
library("gridExtra") # to create objects to place on graphs
library("apitools")
library(readr)
library(readxl)
library(lubridate)

#devtools::install_github("donboyd5/fof")
library(fof)
#devtools::install_github("donboyd5/pdata")
library("pdata") # ppd 

source("Functions.R")
source("Functions_Measures.R")

#*****************************************************
##  Combine selected files into a single list.  ####
#*****************************************************
IO_folder <- "IO_M2.1_new"
outputs.folder  <- "/M2.1b_outputs/"

get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder, "^Outputs_I")


#*****************************************************
##  RIG color and theme ####
#*****************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"

RIG.3colors <- c(RIG.red, RIG.green, RIG.blue)

RIG.theme <- function(){
             theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
                   panel.background = element_rect(fill = "white", color = "grey"),
                   legend.key = element_rect(fill = "white", color = "grey"))
}


#*****************************************************
##  Selecting runs ####
#*****************************************************

runs_inv1 <- c("I8F075-2", # SD = 8%
               "I8F075-3", # SD = 12%
               "I8F075-4") # SD = 16%

runs_inv2 <- c("I7F075-1", # the good old days 
               "I1F075-6", # Invest in riskier assets
               "I6F075-6") # Lower assumed return

runs_inv3 <- c("I1F075-6",  # True expected return = 7.5%          
               "I1F075-6b") # True expected return = 6%

runs_inv.all <- Reduce(union, list(runs_inv1, runs_inv2, runs_inv3))


runs_inv_labels1 <- c("Standard deviation = 8%",
                      "Standard deviation = 12%",
                      "Standard deviation = 16%")

runs_inv_labels2 <- c("The good old days",
                      "Invest in riskier assets",
                      "Lower assumed return")

# runs_inv_labels3 <- c("True expected compound return = 7.5%",
#                       "True expected compound return = 6%")


runs_inv_labels3.1line  <- c("Expected compound return: assumed = 7.5%, true = 7.5%",
                      "Expected compound return: assumed = 7.5%, true = 6%")

runs_inv_labels3.2lines <- c("Expected compound return: \nassumed = 7.5%, true = 7.5%",
                       "Expected compound return: \nassumed = 7.5%, true = 6%")


#*****************************************************
## Calculating risk measures ####
#*****************************************************

df_AL.7p5 <- results_all %>% filter(runname == "I7F075-1", sim == 1) %>% select(runname, year, AL) %>% 
  select(year, AL.7p5 = AL)

df_inv.all <- results_all  %>% 
        filter(runname %in% runs_inv.all, sim >= 0, year <= 30) %>% 
  left_join(df_AL.7p5) %>% 
  select(runname, sim, year, AL, AL.7p5,  MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL.7p5,
         FR40less  = cumany(FR_MA <= 40),
         ERC_high  = cumany(ERC_PR >= 30), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
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
  ungroup()



#*****************************************************
## Figures: Pure impact of volatility            ####
#*****************************************************

df_inv1 <- df_inv.all %>% filter(runname %in% runs_inv1) %>% 
  mutate(runname = factor(runname, levels = runs_inv1, labels = runs_inv_labels1))

## Risk of low funded ratio
fig.title <- "Probability of funded ratio falling below 40% \nat any time prior to and including the given year"
fig_pureVol.FR40less <- 
  df_inv1 %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_pureVol.FR40less



## risk of ERC sharp increases
fig.title <- "Probability of employer contribution rising by more than \n10% of payroll in any 5-year period up to the given year"
fig_pureVol.ERC_hike <- 
  df_inv1 %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 40)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_pureVol.ERC_hike


## risk of very high ERC 
fig.title <- "Probability of employer contribution rising above \n30% of payroll at any time prior to and including the given year"
fig_pureVol.ERC_high <- 
  df_inv1 %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 10)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 2)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_pureVol.ERC_high




# Distribution of funded ratios
pctile.labels <- c("75th percentile", "50th percentile", "25th percentile")
fig.title <- "Distribution of funded ratios across simulations under different return volatility"
fig_pureVol.FRDist <- 
  df_inv1 %>%  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(pctile, value, -runname, -year) %>%
  mutate(pctile = factor(pctile, levels = c("FR.q75", "FR.q50", "FR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname) + 
  geom_point(size = 2) + geom_line() + 
  geom_hline(yintercept = 100, linetype = 2, color = "black") +
  #coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 25)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))


fig.title <- "Distribution of employer contribution rates across simulations under different return volatility"
pctile.labels <- c("75th percentile", "50th percentile", "25th percentile")
fig_pureVol.ERCDist <- 
  df_inv1 %>%  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(pctile, value, -runname, -year) %>%
  mutate(pctile = factor(pctile, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname) + 
  geom_point(size = 2) + geom_line() +
  #coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green)) +
  labs(x = "Year",
       y = "Employer contribution rate (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))
fig_pureVol.ERCDist




#****************************************************************************
## Figures: Policy response to deline in risk-free rate           ####
#****************************************************************************

df_inv2 <- df_inv.all %>% filter(runname %in% runs_inv2) %>% 
  mutate(runname = factor(runname, levels = runs_inv2, labels = runs_inv_labels2))

## Risk of low funded ratio
fig.title <- "Probability of funded ratio falling below 40% \nat any time prior to and including the given year"
fig_response.FR40less <- 
  df_inv2 %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 20)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_response.FR40less



## risk of ERC sharp increases
fig.title <- "Probability of employer contribution rising by more than \n10% of payroll in any 5-year period up to the given year"
fig_response.ERC_hike <- 
  df_inv2 %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 20)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_response.ERC_hike


## risk of very high ERC 
fig.title <- "Probability of employer contribution rising above \n30% of payroll at any time prior to and including the given year"
fig_response.ERC_high <- 
  df_inv2 %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 4)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 2)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_response.ERC_high



# Distribution of funded ratios
pctile.labels <- c("75th percentile", "50th percentile", "25th percentile")
fig.title <- "Distribution of funded ratios across simulations under different scenarios"
fig_response.FRDist <- 
  df_inv2 %>%  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(pctile, value, -runname, -year) %>%
  mutate(pctile = factor(pctile, levels = c("FR.q75", "FR.q50", "FR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname) + 
  geom_point(size = 2) + geom_line() + 
  geom_hline(yintercept = 100, linetype = 2, color = "black") +
  coord_cartesian(ylim = c(0, 130)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 25)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))
fig_response.FRDist


# Distribution of ERC rate
fig.title <- "Distribution of employer contribution rates across simulations under different scenarios"
fig_response.ERCDist <- 
  df_inv2 %>%  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(pctile, value, -runname, -year) %>%
  mutate(pctile = factor(pctile, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname) + 
  geom_point(size = 2) + geom_line() +
  #coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green)) +
  labs(x = "Year",
       y = "Employer contribution rate (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))
fig_response.ERCDist


# Median funded ratio
fig.title <- "Median funded ratio"
fig_response.FR.med <- 
  df_inv2 %>% 
  ggplot(aes(x = year, y = FR.q50, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2, color = "black") +
  coord_cartesian(ylim = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,150, 25)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_response.FR.med


# Median ERC rate
fig.title <- "Median employer contribution as a percentage of payroll"
fig_response.ERC_PR.med <- 
  df_inv2 %>% 
  ggplot(aes(x = year, y = ERC_PR.q50, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 50)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,150, 5)) + 
  scale_color_manual(values = RIG.3colors) +
  labs(x = "Year",
       y = "Employer contribution rate(%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_response.ERC_PR.med 


#****************************************************************************
## Table: Policy response to deline in risk-free rate            ####
#****************************************************************************
df_inv2 %>% filter(year %in% c(1, 30)) %>% select(runname, year, FR40less, FR.q50, ERC_hike, ERC_PR.q50) %>% 
  gather(variable, value, -runname, -year) %>% 
  mutate(var_year = paste0(variable, "_y", year)) %>% 
  select(-variable, -year) %>% 
  spread(var_year, value) %>% 
  select(-ERC_hike_y1, -FR40less_y1)



#****************************************************************************
## Figures: True expected return falls short of assumed return           ####
#****************************************************************************


df_inv3 <- df_inv.all %>% filter(runname %in% runs_inv3) %>% 
  mutate(runname.2lines = factor(runname, levels = runs_inv3, labels = runs_inv_labels3.2lines),
         runname = factor(runname, levels = runs_inv3, labels = runs_inv_labels3.1line))

## Risk of low funded ratio
fig.title <- "Probability of funded ratio falling below 40% \nat any time prior to and including the given year"
fig_shortfall.FR40less <- 
  df_inv3 %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 40)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red) ) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_shortfall.FR40less



## risk of ERC sharp increases
fig.title <- "Probability of employer contribution rising by more than \n10% of payroll in any 5-year period up to the given year"
fig_shortfall.ERC_hike <- 
  df_inv3 %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 25)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_shortfall.ERC_hike


## risk of very high ERC 
fig.title <- "Probability of employer contribution rising above \n30% of payroll at any time prior to and including the given year"
fig_shortfall.ERC_high <- 
  df_inv3 %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 10)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 2)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_shortfall.ERC_high



# Distribution of funded ratios
pctile.labels <- c("75th percentile", "50th percentile", "25th percentile")
fig.title <- "Distribution of funded ratios across simulations under true expected compound returns"
fig_shortfall.FRDist <- 
  df_inv3 %>%  select(runname.2lines, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(pctile, value, -runname.2lines, -year) %>%
  mutate(pctile = factor(pctile, levels = c("FR.q75", "FR.q50", "FR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname.2lines) + 
  geom_point(size = 2) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2) + 
  coord_cartesian(ylim = c(0, 130)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 25)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))
fig_shortfall.FRDist

ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.FRDist.png"),fig_shortfall.FRDist, width=11*.9, height=5*.9, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.FRDist.pdf"),fig_shortfall.FRDist, width=11*.9, height=5*.9, units="in")



# Distribution of ERC rate
fig.title <- "Distribution of employer contribution rates across simulations \nunder different true expected compound returns"
fig_shortfall.ERCDist <- 
  df_inv3 %>%  select(runname.2lines, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(pctile, value, -runname.2lines, -year) %>%
  mutate(pctile = factor(pctile, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"), labels = pctile.labels)) %>% 
  ggplot(aes(x = year, y = value, color = pctile, shape = pctile)) + theme_bw() +  RIG.theme() + facet_grid(.~runname.2lines) + 
  geom_point(size = 2) + geom_line() +
  #coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,250, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green)) +
  labs(x = "Year",
       y = "Employer contribution rate (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL))
fig_shortfall.ERCDist


# Median funded ratio
fig.title <- "Median funded ratio"
fig_shortfall.FR.med <- 
  df_inv3 %>% 
  ggplot(aes(x = year, y = FR.q50, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2, color = "black") + 
  coord_cartesian(ylim = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,150, 25)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_shortfall.FR.med


# Median ERC rate
fig.title <- "Median employer contribution as a percentage of payroll"
fig_shortfall.ERC_PR.med <- 
  df_inv3 %>% 
  ggplot(aes(x = year, y = ERC_PR.q50, color = runname, shape = runname)) +  RIG.theme() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 25)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,150, 5)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red)) +
  labs(x = "Year",
       y = "Employer contribution rate(%)",
       title = fig.title) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig_shortfall.ERC_PR.med 



#*****************************************************
## Figure 1 Equity share ####
#*****************************************************

# # fofu %>% filter(variable=="LM653064100") %>% count(freq)
# # fof %>% filter(variable=="FL225035043") %>% count(freq)
# 
# # prepare data
# penvars <- c("FL223073045", "FL224190043", "FL224190005", "FA086902005", "FL214190005", "FL213170003")
# penvarf <- c("claims.db", "entitlement.db", "liabs.all", "gdp", "slgliabtot", "slgtradepayables")
# 
# taxvars <- c("FA206210001", "FA206240001", "FA206231001")
# taxvarf <- c("slgcurtax", "slgprodtax", "slgcorptax")
# 
# vars <- c(penvars, taxvars)
# varf <- c(penvarf, taxvarf)
# 
# fofu %>% filter(variable=="FL223073045") %>% count(freq, description)
# 
# fofbase <- fofu %>% filter(freq=="Q", variable %in% vars) %>% 
#   mutate(varf=factor(variable, levels=vars, labels=varf)) %>%
#   select(varf, date, value) %>%
#   spread(varf, value) %>%
#   mutate(slgtax=slgcurtax + slgprodtax + slgcorptax,
#          slgdebt=slgliabtot - slgtradepayables - claims.db)
# 
# fofcalcs <- function(df) {
#   df <- df %>% mutate(claimpct=claims.db / entitlement.db * 100,
#                       fr=100 - claimpct,
#                       liab.gdp=entitlement.db / gdp * 100,
#                       ufl.gdp=claims.db / gdp * 100,
#                       ufl.tax=claims.db / slgtax * 100)
#   return(df)
# }
# 
# fofdat.q <- fofbase %>% do(fofcalcs(.))
# 
# # glimpse(fofbase)
# 
# fofdat.a <- fofbase %>%
#   mutate(year=year(date)) %>%
#   select(-date) %>%
#   group_by(year) %>%
#   summarise_all(mean) %>%
#   do(fofcalcs(.))
# 
# fofdat.fy <- fofbase %>%
#   mutate(fyear=year(date) + (month(date)>=7)) %>%
#   select(-date) %>%
#   group_by(fyear) %>%
#   summarise_all(mean) %>%
#   do(fofcalcs(.))
# 
# 
# 
# 
# 
# vnames.vec <- "snamex, shortname
# 
# # general variables
# FA206210001, other.slgperscurtax
# FA206240001, other.slgprodimptax
# FA086902005, other.gdp
# FL213162005, other.munisec
# FL213162400, other.stdebt
# FL214090005, other.slgfinass
# FL214190005, other.slgfinliab
# FL653064100, other.mfcorpequity_old
# LM653064100, other.mfcorpequity
# FL654090005, other.mfassets_old
# LM654090000, other.mfassets
# 
# # private DB pension funds
# FL574090045, ppfdb.finassets
# FL573065043, ppfdb.mortgages
# LM573064143, ppfdb.corpequity
# LM573064243, ppfdb.mfshares
# FL573073005, ppfdb.claims
# FL573093043, ppfdb.otherassets
# FL574190043, ppfdb.entitlement
# FL575035005, ppfdb.redirect
# 
# # SLG DB funds
# FL224090045, slgdb.finassets
# FL223065043, slgdb.mortgages
# LM223064145, slgdb.corpequity
# LM223064243, slgdb.mfshares
# FL223073045, slgdb.claims
# FL223093043, slgdb.otherassets
# FL224190043, slgdb.entitlement
# FL225035043, slgdb.redirect
# "
# 
# vnames <- read_csv(vnames.vec) %>% 
#   setNames(str_trim(names(.))) %>%
#   mutate_each(funs(str_trim)) %>%
#   filter(!is.na(shortname), !str_detect(snamex, "#"))
# # vnames
# # anyDuplicated(vnames)
# # vnames[duplicated2(vnames)]
# # glimpse(fof)
# 
# df <- fofu %>% filter(variable %in% vnames$snamex, freq=="Q") %>%
#   group_by(year, variable, description, units) %>%
#   summarise(value=mean(value, na.rm=TRUE)) %>%
#   mutate(vname=vnames$shortname[match(variable, vnames$snamex)]) %>%
#   ungroup %>%
#   select(vname, year, value) %>%
#   separate(vname, c("slgppf", "vname"), sep="\\.", 
#            extra="merge", remove=TRUE) %>% # so we can track public plans, private plans, other data
#   select(slgppf, year, vname, value)
# # ht(df)
# # count(df, vname)
# # df %>% filter(str_detect(vname, "mfcorpequity")) %>% spread(vname, value)
# 
# dfothr <- filter(df, slgppf=="other") %>%
#   spread(vname, value) %>%
#   mutate(mfstockshare=mfcorpequity / mfassets) # economywide share of mutual fund assets in corp equities
# 
# # isolate slgtax for analysis
# 
# # redirect seems to have disappeared from the data, so compute
# # equityshare3 is prob best 6/13/2016
# dfshares <- df %>% filter(slgppf!="other") %>%
#   spread(vname, value) %>%
#   left_join(select(dfothr, year, gdp, mfstockshare)) %>%
#   left_join(select(fofdat.a, year, slgtax)) %>%
#   mutate(invassets=entitlement - claims,
#          redirect=invassets - (finassets - claims), # no longer a reported variable
#          fr.mv=invassets / entitlement * 100,
#          equity1=corpequity + mortgages + (mfshares + otherassets) * mfstockshare, # +otherassets
#          equity2=corpequity + mfshares * mfstockshare / 100,
#          equity3=corpequity + (mfshares + otherassets) * mfstockshare / 100 + redirect, 
#          equityshare1=equity1 / invassets * 100,
#          equityshare2=equity2 / invassets * 100,
#          equityshare3=equity3 / invassets * 100,
#          equity3slgtax=equity3 / slgtax * 100)
# #equitygdpshare=equity1/gdp *100)
# 
# # dfshares %>% select(year, slgppf, starts_with("equity")) %>% 
# #   qplot(year, equityshare1, data=., colour=slgppf, geom=c("point", "line"))
# 
# 
# 
# 
# ## Public and private plans risk assets
# 
# pdata <- dfshares %>% filter(year>=1963)
# 
# linesize <- 1.1
# pointsize <- 1.1
# legend <- guides(colour=guide_legend(title=NULL))
# 
# main <- "Equity-like investments as percentage of invested assets \nof defined benefit plans
# State and local government and private sector plans"
# xlab <- "Calendar year"
# fig.equityShare <- pdata %>%
#   mutate(slgppf=factor(slgppf, levels=c("slgdb", "ppfdb"), c("State & local", "Private"))) %>%
#   ggplot(aes(x=year, y=equityshare3, colour=slgppf)) +
#   theme_bw() + # RIG.theme() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
#   geom_line(size=linesize) +
#   geom_point(size=pointsize) +
#   scale_y_continuous(name="Percent (%)", breaks=seq(0, 80, 5), limits=c(0, 70)) +
#   scale_x_continuous(name=xlab, breaks=seq(1940, 2020, 5)) +
#   scale_colour_manual(values=c(RIG.blue, RIG.green)) +
#   legend +
#   ggtitle(main)
# # fig.equityShare
# # ggsave("./results/invest_equitysharesdb.png", p, width=11, height=6.8, units="in")
# 
# # do this when we need both an axis label (centered) and a source note (left justified, not bold)
# # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
# grid.newpage()
# note <- "\n\nSource: Authors' analysis of Financial Accounts of the United States, Federal Reserve Board"
# fig.equityShare <- arrangeGrob(fig.equityShare, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
#                                       gp = gpar(fontface = "plain", fontsize = 10)))
# grid.draw(fig.equityShare)
# 
# # ggsave("./results/invest_equitysharesdb.png", g, width=10, height=8, units="in")
# 
# # write_csv(pdata, paste0("./results/invest_equitysharesdb_data.csv"))





# get and save flow of funds data
# fofu %>% filter(variable=="LM653064100") %>% count(freq)
# fof %>% filter(variable=="FL225035043") %>% count(freq)

vnames <- read_csv("snamex, shortname
                   # general variables
                   FA206210001, other.slgperscurtax
                   FA206240001, other.slgprodimptax
                   FA206231001, other.slgcorptax
                   FA086902005, other.gdp
                   FL213162005, other.munisec
                   FL213162400, other.stdebt
                   FL214090005, other.slgfinass
                   FL214190005, other.slgfinliab
                   LM653064100, other.mfcorpequity
                   LM654090000, other.mfassets
                   
                   # private DB pension funds
                   FL574090045, ppfdb.finassets
                   FL573065043, ppfdb.mortgages
                   LM573064143, ppfdb.corpequity
                   LM573064243, ppfdb.mfshares
                   FL573073005, ppfdb.claims
                   FL573093043, ppfdb.otherassets
                   FL574190043, ppfdb.entitlement
                   FL575035005, ppfdb.redirect
                   
                   # SLG DB funds
                   FL224090045, slgdb.finassets
                   FL223065043, slgdb.mortgages
                   LM223064145, slgdb.corpequity
                   LM223064243, slgdb.mfshares
                   FL223073045, slgdb.claims
                   FL223093043, slgdb.otherassets
                   FL224190043, slgdb.entitlement
                   FL225035043, slgdb.redirect")
vnames

# get quarterly FOF data from unique-variables file (fofu) ####
df <- fofu %>% filter(variable %in% vnames$snamex, freq=="Q") %>%
  mutate(vname=vnames$shortname[match(variable, vnames$snamex)]) %>%
  select(vname, date, value) %>%
  separate(vname, c("slgppf", "vname"), sep="\\.", 
           extra="merge", remove=TRUE) %>% # so we can track public plans, private plans, other data
  select(slgppf, date, vname, value)
ht(df)
count(df, vname)
# which vars don't we have?
vnames[!vnames$shortname %in% unique(paste(df$slgppf, df$vname, sep=".")), ]
# the data no longer include redirect and we must calculate it


# quarterly data calculations ####
df.q <- df
dfother.q <- filter(df.q, slgppf=="other") %>%
  spread(vname, value) %>%
  mutate(mfstockshare=mfcorpequity / mfassets, # economywide share of mutual fund assets in corp equities
         slgtax=slgperscurtax + slgprodimptax + slgcorptax)

# redirect has disappeared from the data, so compute
fof.q <- df.q %>% filter(slgppf!="other") %>%
  spread(vname, value) %>%
  left_join(select(dfother.q, date, gdp, mfstockshare, slgtax)) %>%
  mutate(invassets=entitlement - claims,
         redirect=invassets - (finassets - claims), # no longer a reported variable
         fr.mv=invassets / entitlement * 100,
         # equity3_old=corpequity + (mfshares + otherassets) * mfstockshare / 100 + redirect, # old approach, error
         equity = corpequity + (mfshares + otherassets) * mfstockshare + redirect, 
         equityshare = equity / invassets * 100,
         equityslgtax = equity / slgtax * 100,
         equitygdpshare = equity / gdp *100)
saveRDS(fof.q, "./Data/fof.q.rds")


# annual calculations ####
df.a <- df %>% mutate(date=floor_date(date, "year")) %>%
  group_by(slgppf, vname, date) %>%
  summarise(value=sum(value)) %>%
  ungroup

dfother.a <- filter(df.a, slgppf=="other") %>%
  spread(vname, value) %>%
  mutate(mfstockshare=mfcorpequity / mfassets, # economywide share of mutual fund assets in corp equities
         slgtax=slgperscurtax + slgprodimptax + slgcorptax)

# redirect has disappeared from the data, so compute
# equityshare3 is prob best 6/13/2016
fof.a <- df.a %>% filter(slgppf!="other") %>%
  spread(vname, value) %>%
  left_join(select(dfother.a, date, gdp, mfstockshare, slgtax)) %>%
  mutate(invassets=entitlement - claims,
         redirect=invassets - (finassets - claims), # no longer a reported variable
         fr.mv=invassets / entitlement * 100,
         # equity3_old=corpequity + (mfshares + otherassets) * mfstockshare / 100 + redirect, # old approach, error
         equity = corpequity + (mfshares + otherassets) * mfstockshare + redirect, 
         equityshare = equity / invassets * 100,
         equityslgtax = equity / slgtax * 100,
         equitygdpshare = equity / gdp *100)
saveRDS(fof.a, "./Data/fof.a.rds")



fof.a <- readRDS("./data/fof.a.rds")

pdata <- fof.a %>% filter(year(date) >= 1990) %>% 
  mutate(slgppf=factor(slgppf, levels=c("slgdb", "ppfdb"), c("State & local", "Private")))

gtitle1 <- "Equity-like investments as percentage of invested assets"
gtitle2 <- "State and local government and private sector defined benefit pension plans"
xlab <- "Calendar year"
xlab <- NULL
#srcnote <- "\nSource: Authors' analysis of Financial Accounts of the United States, Federal Reserve Board"
srcnote <- "\nSource: Authors' analysis of Z.1 Financial Accounts of the United States, Federal Reserve Board, Tables L.118.b, L.120.b, and L.122"
fig.equityShare <- pdata %>%
  ggplot(aes(x=date, y=equityshare, colour=slgppf)) +
  geom_line(size=rel(1)) +
  geom_point(size=rel(1.4)) +
  scale_y_continuous(name="Percent (%)", breaks=seq(0, 100, 5), limits=c(0, 75)) +
  scale_x_date(breaks=seq.Date(as.Date("1900-01-01"), as.Date("2020-01-01"), "5 years"), date_labels = "%Y") +
  scale_colour_manual(values=c(RIG.blue, RIG.green), name = "") +
  # legend +
  labs(x=xlab, caption=srcnote) +
  ggtitle(label=gtitle1, subtitle=gtitle2) +
  theme_bw() +
  #RIG.theme() + 
  theme(plot.title = element_text(size=rel(1.3), face="bold", hjust = 0.5)) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold", hjust = 0.5)) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.7))) + 
  theme(panel.grid.minor.x = element_blank()) + 
  theme(panel.grid.minor.y = element_blank())
 
fig.equityShare




# #*****************************************************
# ## Figure 2 Risk-free rate and assumed return ####
# #*****************************************************



# FRED
# DGS10 10-Year Treasury Constant Maturity Rate
# DGS30 30-Year Treasury Constant Maturity Rate
# http://www.federalreserve.gov/releases/h15/current/h15.pdf
# http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/yieldmethod.aspx.
# 30 is missing 1960s to ~1975, and also mid 2000s

# some historical earnings assumptions
src94 <- "7.83 mean Survey of State and Local Gov ERS June 1994.pdf Zorn p.40"
src89 <- "http://www.uh.edu/~bsorense/Mitchell%26SmithPensions.pdf"
# https://www.osc.state.ny.us/retire/word_and_pdf_documents/reports/actuarial_assumption/aa_2015.pdf

hist.ir.s <- "fyear, value, source
1975, 5, 1978 Pension Task Force Report On Public Employee Retirement Systems House Committee on Education and Labor p. 161
1989, 7.6, Pension Funding in the Public Sector Olivia S. Mitchell and Robert S. Smith The Review of Economics and Statistics p.281
1990, 7.76, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
1991, 7.81, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
1992, 7.83, Zorn
1994, 7.84, Zorn
1996, 7.84, Zorn p.12
1998, 7.88, Zorn p.12
2000, 7.91, Zorn p.12"

# 1990-1992, 1994, 1996, 1998, and 2000 are generally available from

irhist <- read_csv(hist.ir.s) %>% mutate(series="survey")
irhist

fn <- "NY_CRF_InvestReturnAssumption.xlsx"
nycrf <- read_excel(paste0("./Data/", fn)) %>% select(fyear, value=crfassumed) %>% mutate(value=value*100, series="nycrf")
glimpse(nycrf)

t10 <- FRED("DGS10")
t30 <- FRED("DGS30")
t30 %>% group_by(year) %>% summarise(value=mean(value, na.rm=TRUE))
glimpse(t10)
ht(t10)

rates <- bind_rows(t10, t30)

# qplot(date, value, data=rates, colour=series, geom=c("point", "line"))
#
# rates %>% group_by(series, year) %>%
#   summarise(value=mean(value, na.rm=TRUE)) %>%
#   qplot(year, value, data=., colour=series, geom=c("point", "line"))
#
rates.fy <- rates %>% mutate(fyear=ifelse(month(date)>=7, year+1, year)) %>%
  group_by(series, fyear) %>%
  summarise(value=mean(value, na.rm=TRUE))
#
# rates.fy %>% group_by(series) %>%
#   mutate(diff=value - value[match(2016, fyear)]) %>%
#   select(series, fyear, diff) %>%
#   spread(series, diff)

#glimpse(ppd)
#names(ppd)[str_detect(names(ppd), "sset")]
ppdir.fy <- ppd %>% group_by(fyear=fy) %>%
  summarise(value=mean(InvestmentReturnAssumption_GASB, na.rm=TRUE)*100,
            wvalue=sum(InvestmentReturnAssumption_GASB * MktAssets_net, na.rm=TRUE) /
              sum(MktAssets_net * !is.na(InvestmentReturnAssumption_GASB), na.rm=TRUE) * 100) %>%
  mutate(series="survey")

rates.all <- bind_rows(ppdir.fy, rates.fy, irhist, nycrf) %>% arrange(series, fyear)
saveRDS(rates.all, "./Data/rates.all.rds")

rates.all <- readRDS("./Data/rates.all.rds")


# 
# # rates.all %>% qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # rates.all %>% filter(!series %in% c("DGS10")) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # rates.all %>% filter(!series %in% c("DGS30")) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # rates.all %>% filter(!series %in% c("DGS30", "nycrf"), fyear>=1973) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # rates.all %>% filter(!series %in% c("DGS10", "nycrf"), fyear>=1973) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # 
# # rates.all %>% filter(!series %in% c("DGS30", "nycrf"), fyear>=1983) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # rates.all %>% filter(!series %in% c("DGS10", "nycrf"), fyear>=1983) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# # 
# # 
# # rates.all %>% filter(!series %in% c("nycrf"), fyear>=1983) %>% 
# #   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# 
# rates.all %>% filter(series %in% "survey", fyear %in% 1973:1989)
# 
# levs <- c("DGS10", "survey")
# labs <- c("10-year Treasury yield", "Average assumed return")
# pdat <- rates.all %>% filter(series %in% levs, fyear>=1973) %>%
#   mutate(value=ifelse(fyear<1989 & series=="survey", NA, value),
#          seriesf=factor(series, levels=levs, labels=labs))
# xlab1 <- "Pension fund fiscal year"
# xlab2 <- "Assumed returns not available for 1976-1988 but likely were near dashed line"
# #xlab <- paste0(xlab1, "\n\n", xlab2)
# gtitle <- "Assumed investment returns of state and local retirement systems\nand risk-free returns"
# p <- ggplot(data=pdat, aes(x=fyear, y=value, colour=seriesf)) +
#   theme_bw() + RIG.theme() + 
#   geom_line() +
#   geom_point() +
#   scale_colour_manual(values = c(RIG.red,RIG.blue)) +
#   geom_segment(x = 1975, y = 5.0, xend = 1989, yend = 7.6, colour = "blue", linetype="dashed", size=.1) +
#   geom_point(x=1975, y=5.0, colour="blue") +
#   scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
#   scale_x_continuous(name=xlab1, breaks=seq(1975, 2020, 5)) +
#   guides(colour=guide_legend(title=NULL)) +
#   ggtitle(gtitle) + 
#   theme(plot.margin = unit(c(0,0,3,0), "lines")) # Top, righ, bottom, left
# p
# 
# 
# noteLine1 <- "Note: Assumed returns not available for 1976-1988 but likely were near dashed line"
# noteLine2 <- "See text for sources"
# # "Sources: Public Plans Database (2001-2014), Public Pension Coordinating Council surveys (1990-2000), Congressional Pension Task Force Report - 1978 (1975"
# footNote <- paste0(noteLine1, noteLine2)
# 
# 
# p <-  p + geom_text(aes(label = noteLine1, x = 1975, y = -Inf), hjust = 0.125, vjust = 4.7 + 1, size = 3.5, color = "black")
# p <-  p + geom_text(aes(label = noteLine2, x = 1975, y = -Inf), hjust = 0.52,   vjust = 6.2 + 1, size = 3.5, color = "black")
# 
# 
# # Turning off clipping
# gt <- ggplot_gtable(ggplot_build(p))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"
# grid.draw(gt)
# 
# ggsave(paste0(IO_folder, outputs.folder, "fig2.png"),gt, width=10, height=6, units="in")
# 




# Public pension funds' earnings assumptions

linesize <- 1.1
pointsize <- 1.1
legend <- guides(colour=guide_legend(title=NULL))


## Longer time period

rates.all <- readRDS("./data/rates.all.rds")

# rates.all %>% filter(series %in% "survey", fyear %in% 1973:1989)

levs <- c("DGS10", "survey")
labs <- c("10-year Treasury yield", "Average assumed return")
pdata <- rates.all %>% filter(series %in% levs, fyear>=1973) %>%
  mutate(value=ifelse(fyear<1989 & series=="survey", NA, value),
         seriesf=factor(series, levels=levs, labels=labs))
# xlab1 <- "Pension fund fiscal year"
# xlab2 <- "Assumed returns not available for 1976-1988 but likely were near dashed blue line"
# xlab <- paste0(xlab1, "\n\n", xlab2)
xlab <- "Pension fund fiscal year"
gtitle <- "Assumed investment returns of state and local retirement systems\nand risk-free returns"
p <- ggplot(data=pdata, aes(x=fyear, y=value, colour=seriesf)) +
  theme_bw() +
  # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
  theme(legend.justification=c(0, 1), legend.position=c(.75, 1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) +
  geom_line(size=linesize) +
  geom_point(size=pointsize) +
  scale_colour_manual(values = c("darkgreen", "blue")) +
  geom_segment(x = 1975, y = 5.0, xend = 1989, yend = 7.6, colour = "blue", linetype="dashed", size=.1) +
  geom_point(x=1975, y=5.0, colour="blue", size=pointsize) +
  scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
  scale_x_continuous(name=xlab, breaks=seq(1975, 2020, 5)) +
  legend +
  ggtitle(gtitle)
q <- p + annotate("text", label="...no data...", x=1982, y=6.5, angle=15, size=3)

# do this when we need both an axis label (centered) and a source note (left justified, not bold)
# theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
grid.newpage()
note <- "\nNote: Assumed returns not available for 1976-1988 but likely were near dashed blue line"
g <- arrangeGrob(q, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
                                      gp = gpar(fontface = "plain", fontsize = 12)))
grid.draw(g)

# ggsave("./results/invest_erorvsriskfree.png", g, width=gwidth, height=gheight, units="in")
# write_csv(pdata, paste0("./results/invest_erorvsriskfree_data.csv"))


## Shorter time period

rates.all <- readRDS("./data/rates.all.rds")

# rates.all %>% filter(series %in% "survey", fyear %in% 1973:1989)

levs <- c("DGS10", "survey")
labs <- c("10-year Treasury yield", "Average assumed return")
pdata <- rates.all %>% filter(series %in% levs, fyear>=1990) %>%
  mutate(value=ifelse(fyear<1989 & series=="survey", NA, value),
         seriesf=factor(series, levels=levs, labels=labs))
# xlab1 <- "Pension fund fiscal year"
# xlab2 <- "Assumed returns not available for 1976-1988 but likely were near dashed blue line"
# xlab <- paste0(xlab1, "\n\n", xlab2)
xlab <- "Pension fund fiscal year"
gtitle <- "Assumed investment returns of state and local retirement systems\nand risk-free returns"
p <- ggplot(data=pdata, aes(x=fyear, y=value, colour=seriesf)) +
  theme_bw() +
  # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
  theme(legend.justification=c(0, 1), legend.position=c(.75, 1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) +
  geom_line(size=linesize) +
  geom_point(size=pointsize) +
  scale_colour_manual(values = c("darkgreen", "blue")) +
  scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
  scale_x_continuous(name=xlab, breaks=seq(1975, 2020, 5)) +
  legend +
  ggtitle(gtitle)

# do this when we need both an axis label (centered) and a source note (left justified, not bold)
# theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
grid.newpage()
note <- "\nNote: Assumed returns not available for 1976-1988 but likely were near dashed blue line"
g <- arrangeGrob(p, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
                                      gp = gpar(fontface = "plain", fontsize = 12)))
grid.draw(g)
#ggsave("./results/invest_erorvsriskfree_short.png", p, width=gwidth, height=gheight, units="in")




## Shorter time period with private assumptions
rates.all <- readRDS("./data/rates.all.rds")

# private rates are from Andonov, Bauer, Cremers 2016
private <- read_csv(
  "fyear, value
  1993, 8.214
  1994, 8.145
  1995, 7.987
  1996, 7.972
  1997, 7.71
  1998, 7.495
  1999, 7.961
  2000, 8.052
  2001, 7.761
  2002, 7.469
  2003, 7.267
  2004, 6.816
  2005, 6.643
  2006, 6.472
  2007, 6.307
  2008, 6.512
  2009, 6.181
  2010, 5.721
  2011, 5.112
  2012, 4.36") %>% mutate(series="privmean")
private

# rates.all %>% filter(series %in% "survey", fyear %in% 1973:1989)

levs <- c("survey", "privmean", "DGS10")
labs <- c("State-local average assumed return", "Private average assumed return", "10-year Treasury yield")
pdata <- rates.all %>% filter(series %in% levs, fyear>=1990) %>%
  bind_rows(private) %>%
  mutate(seriesf=factor(series, levels=levs, labels=labs))
# xlab1 <- "Pension fund fiscal year"
# xlab2 <- "Assumed returns not available for 1976-1988 but likely were near dashed blue line"
# xlab <- paste0(xlab1, "\n\n", xlab2)
xlab <- "Pension fund fiscal year"
gtitle <- "Assumed investment returns of public and private retirement systems\nand risk-free returns"
fig.rates_short_priv <- ggplot(data=pdata, aes(x=fyear, y=value, colour=seriesf)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
  theme(legend.justification=c(1, 1), legend.position=c(0.99, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) +
  geom_line(size=rel(1)) +
  geom_point(size=rel(1.5)) +
  scale_colour_manual(values = c(RIG.blue, RIG.green, RIG.purple)) +
  scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
  scale_x_continuous(name=xlab, breaks=seq(1975, 2020, 5)) +
  legend +
  ggtitle(gtitle) + 
  theme(panel.grid.minor.x = element_blank()) 
  # theme(panel.grid.minor.y = element_blank())
fig.rates_short_priv




# # do this when we need both an axis label (centered) and a source note (left justified, not bold)
# # theme(axis.title.x = element_text(hjust=0.5, face="bold", size=rel(1))) +
# grid.newpage()
# note <- "\nNote: Assumed returns not available for 1976-1988 but likely were near dashed blue line"
# g <- arrangeGrob(q, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
#                                       gp = gpar(fontface = "plain", fontsize = 12)))
# grid.draw(g)

# ggsave("./results/invest_eror_pubpriv_vsriskfree_short_wide.png", p, width=11, height=6.8, units="in")

grid.newpage()
n0 <- "\n\n  Sources:"
n1 <- "\n  State-local assumed return from Public Plans Database"
n2 <- "\n Private assumed returns provided by Andonov, Bauer, and Cremers"
n3 <- "\n10-Year Treasury yield from Federal Reserve Bank of St. Louis (FRED)"
note <- paste0(n0, n1, n2, n3)
fig.rates_short_priv <- arrangeGrob(fig.rates_short_priv, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
                                      gp = gpar(fontface = "plain", fontsize = 9)))
grid.draw(fig.rates_short_priv)

#ggsave("./results/invest_eror_pubpriv_vsriskfree_short.png", g, width=gwidth, height=gheight, units="in")
#write_csv(pdata, paste0("./results/invest_eror_pubpriv_vsriskfree_short_data.csv"))


ggsave(paste0(IO_folder, outputs.folder, "fig2_rates_short_priv.png"), fig.rates_short_priv, width=10*0.8, height=8*0.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig2_rates_short_priv.pdf"), fig.rates_short_priv, width=10*0.8, height=8*0.8, units="in")

# pdata %>% select(fyear, value, seriesf) %>% spread(seriesf, value) %>% 
#   write.csv(file = paste0(IO_folder, outputs.folder, "fig2_rates_short_priv.csv"))
# 

#*****************************************************
## Saving outputs  ####
#*****************************************************

 fig.width = 6
 fig.height = 5.5

# Fig 1
ggsave(paste0(IO_folder, outputs.folder, "fig1_equityShare.png"), fig.equityShare, width=10*0.8, height=6*0.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig1_equityShare.pdf"), fig.equityShare, width=10*0.8, height=6*0.8, units="in")


# Fig 2
ggsave(paste0(IO_folder, outputs.folder, "fig2_rates_short_priv.png"), fig.rates_short_priv, width=10*0.75, height=8*0.75, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig2_rates_short_priv.pdf"), fig.rates_short_priv, width=10*0.75, height=8*0.75, units="in")

 
# Fig 3
ggsave(paste0(IO_folder, outputs.folder, "fig3_pureVol.FRDist.png"),fig_pureVol.FRDist, width=15*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig3_pureVol.FRDist.pdf"),fig_pureVol.FRDist, width=15*.8, height=5*.8, units="in")


# Fig 4
ggsave(paste0(IO_folder, outputs.folder, "fig4_pureVol.FR40less.png"),fig_pureVol.FR40less, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig4_pureVol.FR40less.pdf"),fig_pureVol.FR40less, width=fig.width, height=fig.height, units="in")


# Fig 5
ggsave(paste0(IO_folder, outputs.folder, "fig5_pureVol.ERCDist.png"),fig_pureVol.ERCDist, width=15*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig5_pureVol.ERCDist.pdf"),fig_pureVol.ERCDist, width=15*.8, height=5*.8, units="in")


# Fig 6
ggsave(paste0(IO_folder, outputs.folder, "fig6_pureVol.ERC_hike.png"),fig_pureVol.ERC_hike, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig6_pureVol.ERC_hike.pdf"),fig_pureVol.ERC_hike, width=fig.width, height=fig.height, units="in")


# Fig 7
ggsave(paste0(IO_folder, outputs.folder, "fig7_response.FRDist.png"),fig_response.FRDist, width=15*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig7_response.FRDist.pdf"),fig_response.FRDist, width=15*.8, height=5*.8, units="in")


# Fig 8
ggsave(paste0(IO_folder, outputs.folder, "fig8_response.FR40less.png"),fig_response.FR40less, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig8_response.FR40less.pdf"),fig_response.FR40less, width=fig.width, height=fig.height, units="in")


# Fig 9
ggsave(paste0(IO_folder, outputs.folder, "fig9_response.ERCDist.png"),fig_response.ERCDist, width=15*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig9_response.ERCDist.pdf"),fig_response.ERCDist, width=15*.8, height=5*.8, units="in")


# Fig 10
ggsave(paste0(IO_folder, outputs.folder, "fig10_response.ERC_hike.png"),fig_response.ERC_hike, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig10_response.ERC_hike.pdf"),fig_response.ERC_hike, width=fig.width, height=fig.height, units="in")


# Fig 11
ggsave(paste0(IO_folder, outputs.folder, "fig11_shortfall.FRDist.png"),fig_shortfall.FRDist, width=11*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig11_shortfall.FRDist.pdf"),fig_shortfall.FRDist, width=11*.8, height=5*.8, units="in")


# Fig 12
ggsave(paste0(IO_folder, outputs.folder, "fig12_shortfall.FR40less.png"),fig_shortfall.FR40less, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig12_shortfall.FR40less.pdf"),fig_shortfall.FR40less, width=fig.width, height=fig.height, units="in")


# Fig 13
ggsave(paste0(IO_folder, outputs.folder, "fig13_shortfall.ERCDist.png"),fig_shortfall.ERCDist, width=11*.8, height=5*.8, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig13_shortfall.ERCDist.pdf"),fig_shortfall.ERCDist, width=11*.8, height=5*.8, units="in")


# Fig 14
ggsave(paste0(IO_folder, outputs.folder, "fig14_shortfall.ERC_hike.png"),fig_shortfall.ERC_hike, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig14_shortfall.ERC_hike.pdf"),fig_shortfall.ERC_hike, width=fig.width, height=fig.height, units="in")




## Figures not shown in the report

# Pure volatility
ggsave(paste0(IO_folder, outputs.folder, "figX_pureVol.ERC_high.png"),fig_pureVol.ERC_high, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_pureVol.ERC_high.pdf"),fig_pureVol.ERC_high, width=fig.width, height=fig.height, units="in")


# Policy response
ggsave(paste0(IO_folder, outputs.folder, "figX_response.ERC_high.png"),fig_response.ERC_high, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_response.ERC_high.pdf"),fig_response.ERC_high, width=fig.width, height=fig.height, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figX_response.ERC_PR.med.png"),fig_response.ERC_PR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_response.ERC_PR.med.pdf"),fig_response.ERC_PR.med, width=fig.width, height=fig.height, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figX_response.FR.med.png"),fig_response.FR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_response.FR.med.pdf"),fig_response.FR.med, width=fig.width, height=fig.height, units="in")



# True return falls short of the assumption

ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.ERC_high.png"),fig_shortfall.ERC_high, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.ERC_high.pdf"),fig_shortfall.ERC_high, width=fig.width, height=fig.height, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.ERC_PR.med.png"),fig_shortfall.ERC_PR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.ERC_PR.med.pdf"),fig_shortfall.ERC_PR.med, width=fig.width, height=fig.height, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.FR.med.png"),fig_shortfall.FR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX_shortfall.FR.med.pdf"),fig_shortfall.FR.med, width=fig.width, height=fig.height, units="in")











