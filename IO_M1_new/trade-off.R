# Examining trade-off between contribution volatility and funded status. 
# Yimeng Yin
# 10/19/2015

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
#               ## Combine selected files into a single list. ####
#****************************************************************************************************

IO_folder <- "IO_M1_new"

get_results <- function(IO_folder){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}

  file_select <- dir(IO_folder, pattern = "^Outputs")
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder)



#****************************************************************************************************
#               ## Display selected runs ####
#****************************************************************************************************

var.display <- c( "runname", "year",
                  "FR","AL","AA",
                  #"AL_PR", "AL.act_PR", "AL.ret_PR","AL.term_PR", 
                  "NC_PR", "NC.act_PR", "NC.term_PR", 
                  "SC_PR", "C_PR", "ERC_PR",
                  "MA_PR",   
                  "PR.growth", 
                  "ExF_PR",
                  "AL", "C","B","PR",
                  "ExF",
                  "i.r"
)

results_all %>% filter(runname =="A1F100_O30pA5", sim == 0) %>% select(one_of(var.display))


#****************************************************************************************************
#               ## Select runs  ####
#****************************************************************************************************

runs_all <- c(
  "O15d",
  "O15p",
  "O30d",
  "O30p",  
  "O30pA5",
  "O30pA10",

  "C15d",
  "C15p",
  "C30d",
  "C30p",  
  "C30pA5",
  "C30pA10",

  "O30pA5_cap", 
  "soa3_cap",

  "soa3",
  "soa4", 
  "soa4.1"
)


runs_exclude <- c("O30pA10", "C30pA10", "soa3_cap")

runs_all_labels <- c('runname, run.label, key.feature
O15d,         15-year open dollar,                              15-year level dollar - open
O15p,         15-year open percent,                             15-year level percent - open
O30d,         30-year open dollar,                              30-year level dollar - open
O30p,         30-year open percent,                             30-year level percent - open
O30pA5,       "30-year open percent \n 5-year assets",          30-year level percent - open; 5-year asset smoothing
C15d,         15-year closed dollar,                            15-year level dollar - closed                       
C15p,         15-year closed percent,                           15-year level percent - closed
C30d,         30-year closed dollar,                            30-year level dollar - closed
C30p,         30-year closed percent,                           30-year level percent - closed
C30pA5,       "30-year closed percent \n       5-year assets",  30-year level perncet - closed; 5-year asset smoothing
O30pA5_cap,   "30-year open percent \n5-year assets;ERC cap",   30-year level perncet - closed; 5-year asset smoothing; 20% ERC cap
soa3,         "SOA Blue Ribbon\n Benchmark",                    SOA Blue Ribbon Panel Benchmark
soa4,         "SOA Blue Ribbon\n Benchmark;ir=6.62%",                    SOA Blue Ribbon Panel Benchmark;ir6.62%
soa4.1,       "SOA Blue Ribbon\n Benchmark;ir=5.9%",                  SOA Blue Ribbon Panel Benchmark;ir5.9%
')
 
runs_all_labels <- read.table(text = runs_all_labels, header = TRUE,sep = ",", stringsAsFactors = F) %>% 
                   mutate_each(funs(str_trim)) %>% 
                   mutate(runname = paste0("A1F075_", runname)) 

#****************************************************************************************************
#               ## Measures of FR risk and contribution volatility   ####
#****************************************************************************************************

get_metrics <- function(runs, year.max, include.maxChg = FALSE){


  
runs = runs_all
year.max = 30
# include.maxChg = TRUE
  
df_TO <- results_all %>% filter(runname %in% paste0("A1F075_", runs), year <= year.max, sim > 0) %>% 
                         select(runname, year, sim, FR_MA, ERC_PR, C_PR, C, ERC, PR)  
  
## Measures of funded status *********************************************

# 1. Probability of funded ratio falling below X% in 5, 15, 30 years.  
# 2. VaR-like measure:  5th percentile of funded ratio in year 5, 15, and 30.
# 3. CVaR-like measure: weighted average of funded ratio below 5th percentile in year 5, 15, and 30. 
#                       question: how to calculate the weight?  



df_ruin <- 
df_TO %>% group_by(runname, sim) %>% 
           mutate(FR50 = cumany(FR_MA  <= 50),
                  FR40 = cumany(FR_MA  <= 40)) %>% 
           filter(year %in% c(15,30,40)) %>% 
           ungroup %>% group_by(runname, year) %>% 
           summarise(FR50 = 100 * sum(FR50)/n(),
                     FR40 = 100 * sum(FR40)/n()) %>% 
           gather(variable, value, -runname, -year) %>% 
           mutate(variable = paste0(variable, "_y", year),
                  year = NULL) %>% 
           spread(variable, value)

df_ruin


## Measures of contribution volatility *************************************

df_sd <- df_TO %>%
  select(runname, year, sim, C_PR, ERC_PR) %>% 
  group_by(sim, runname) %>% 
  summarise(C_PR.sd = sd(C_PR, na.rm = TRUE), ERC_PR.sd = sd(ERC_PR, na.rm = TRUE)) %>% 
  group_by(runname) %>% 
  summarise_each(funs(median)) %>% select(-sim)
df_sd


df_dsd <- df_TO %>%
  select(runname, year, sim, C_PR, ERC_PR) %>% 
  group_by(sim, runname) %>% 
  summarise(C_PR.dsd = sd(diff(C_PR), na.rm = TRUE), ERC_PR.dsd = sd(diff(ERC_PR), na.rm = TRUE)) %>% 
  group_by(runname) %>% 
  summarise_each(funs(median)) %>% select(-sim)
df_dsd


df_worst <- df_TO %>%
  select(runname, year, sim, C_PR, ERC_PR) %>%  
  group_by(sim, runname) %>% 
  summarise(C_PR.max = max(C_PR), ERC_PR.max = max(ERC_PR)) %>% 
  group_by(runname) %>% 
  summarise_each(funs(median)) %>% select(-sim)
df_worst


df_final <- df_TO %>%
  select(runname, year, sim, C_PR, ERC_PR) %>%   
  group_by(sim, runname)    %>% 
  filter(year == max(year)) %>% 
  group_by(runname) %>% 
  summarise(C_PR.final   = median(C_PR), 
            ERC_PR.final = median(ERC_PR))
df_final


df_5yearChg <- df_TO %>%
  select(runname, year, sim, C_PR, ERC_PR) %>%   
  group_by(sim, runname)  %>% 
  mutate_each(funs(. - lag(., 5)), one_of(c("C_PR", "ERC_PR")) ) %>% 
  summarise(C_PR.5yChg   = max(C_PR,   na.rm = TRUE),
            ERC_PR.5yChg = max(ERC_PR, na.rm = TRUE)) %>% 
  group_by(runname) %>% 
  summarise_each(funs(median)) %>% select(-sim)

# with(df_5yearChg, df_5yearChg[runname == "A1F075_O30pA5","C_PR.5yChg"] %>% unlist) %>% hist(breaks = 50)

if(include.maxChg){
## Create functions to calculate max changes in 5-year intervals. 
maxChgWithin <- function(y, fn, ...){
  # max/min change within a single interval.
  zoo::rollapply(y, rev(seq_along(y)), function(x) fn(x - x[1], ...), fill = NA, align = "left") %>% fn(., ...)
  #y <- outer(x, x, "-")
  #y[lower.tri(y)] %>% fn(., ...)  
}

roll_maxChg <- function(x, fun, width,  ... ){
  # For a given vector x, calculate the max/min change WITHIN each interval of the width "width" 
  zoo::rollapply(x, width, maxChgWithin, fn = fun, ...,  fill = NA, align = "right")
}


system.time(
  df_5yearMaxChg <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%
    group_by(sim, runname)  %>% 
    mutate(C_PR  = roll_maxChg(C_PR,  max, 5),
           ERC_PR= roll_maxChg(ERC_PR,max, 5)) %>% 
    summarise(C_PR.5yMaxChg  = max(C_PR,   na.rm = TRUE),
              ERC_PR.5yMaxChg= max(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)
)
}



## Present Value of contribution *************************************

df_PVC <- df_TO %>% 
  select(runname, year, sim, C, ERC, PR) %>% 
  group_by(sim, runname) %>% 
  mutate(discount = 1/(1 + 0.075)^(year - 1),
         discount_L10 = ifelse(max(year) - year >= 10, 0, discount)) %>% 
  summarise(PV.C   = sum(C * discount),
            PV.ERC = sum(ERC * discount),
            PV.PR  = sum(PR * discount),
            PV.C_L10   = sum(C * discount_L10),
            PV.ERC_L10 = sum(ERC * discount_L10),
            PV.PR_L10  = sum(PR * discount_L10)) %>% 
  group_by(runname) %>% 
  summarise_each(funs(median), -sim) %>% 
  mutate(PV.C_PR   = 100 * PV.C / PV.PR,
         PV.ERC_PR = 100 * PV.ERC / PV.PR,
         PV.C_PR_L10   = 100 * PV.C_L10 / PV.PR_L10,
         PV.ERC_PR_L10 = 100 * PV.ERC_L10 / PV.PR_L10 )



df_metrics <- join_all(list(df_ruin, 
                            df_PVC,
                            df_sd,
                            df_worst,
                            df_final,
                            df_5yearChg))

if(include.maxChg) df_metrics %<>% left_join(df_5yearMaxChg)

return(df_metrics)

}

#****************************************************************************************************
#                  creating plots  ####
#****************************************************************************************************

 df_metrics_y30 <- get_metrics(runs_all, 30, TRUE) # Takes 10+ minutes if include.maxChg = TRUE. Possible reason is the repeated use of zoo::rollapply
 df_metrics_y40 <- get_metrics(runs_all, 40, TRUE)
# save(df_metrics_y30, df_metrics_y40, file = paste0(IO_folder, "/Data_trade_off/df_metrics.RData"))

load(paste0(IO_folder, "/Data_trade_off/df_metrics.RData"))

df_metrics_y40 %<>% left_join(runs_all_labels) %>% filter(!runname %in% paste0("A1F075_",runs_exclude)) 
df_metrics_y30 %<>% left_join(runs_all_labels) %>% filter(!runname %in% paste0("A1F075_",runs_exclude)) 


df_metrics_y40

# Plotting functions
plot_tradeOff <- function(x, y, data, reg.line = TRUE){
  p <- 
    data %>% ggplot(aes_string(x = x , y = y, label = "run.label")) + 
    geom_point(size = 2.5) +
    geom_text(color = "black", hjust = -0.1, size = 3) + 
    # coord_cartesian(xlim = c(0, 0.45)) + 
    theme_bw() + theme(legend.position = "none")
  
    if(reg.line) p <- p + stat_smooth(data = data %>% filter(!grepl("soa", runname)),
                                       method = "lm", se = F, fullrange = TRUE,  
                                       color = "darkgray", linetype = 2, size = 0.8)
    return(p)
}

lab_x     <- function(x, y){paste0("Probability of funded ratio falling below ", x, "% during first ", y, " years (%)")}
lab_5yChg <- "Contribution Volatility:\nMaximum increase in any 5-year period of employer contributions as % of payroll \n(median of 1,000 runs)"
lab_ERC       <- "Ratio of PV employer contribution to PV payroll"
lab_ERC_L10   <- "Ratio of PV employer contribution to PV payroll: last 10 years only"
lab_dsd   <- "Standard deviation of the annual change of employer contributions" 




# 30-year trade-off
pFR40_5yMaxChg_y30 <- plot_tradeOff("FR40_y30", "ERC_PR.5yMaxChg", df_metrics_y30) + labs(x = lab_x(40, 30), y = lab_5yChg)
pFR50_5yMaxChg_y30 <- plot_tradeOff("FR50_y30", "ERC_PR.5yMaxChg", df_metrics_y30) + labs(x = lab_x(50, 30), y = lab_5yChg)

# 30-year volatility vs level of contribution
pERC_5yMaxChg_y30 <- plot_tradeOff("PV.ERC_PR", "ERC_PR.5yMaxChg", df_metrics_y30) + labs(x = lab_ERC, y = lab_5yChg)
# PV of last 10 years
pERC_L10_5yMaxChg_y30 <- plot_tradeOff("PV.ERC_PR_L10", "ERC_PR.5yMaxChg", df_metrics_y30) + labs(x = lab_ERC_L10, y = lab_5yChg)

# 30-year FR vs level of contribution
pFR40_ERC_y30 <- plot_tradeOff("FR40_y30", "PV.ERC_PR", df_metrics_y30) + labs(x = lab_x(40, 30), y = lab_ERC)
pFR50_ERC_y30 <- plot_tradeOff("FR50_y30", "PV.ERC_PR", df_metrics_y30) + labs(x = lab_x(50, 30), y = lab_ERC)

# 30-year FR vs level of contribution: PV of last 10 years
pFR40_ERC_L10_y30 <- plot_tradeOff("FR40_y30", "PV.ERC_PR_L10", df_metrics_y30) + labs(x = lab_x(40, 30), y = lab_ERC_L10)
pFR50_ERC_L10_y30 <- plot_tradeOff("FR50_y30", "PV.ERC_PR_L10", df_metrics_y30) + labs(x = lab_x(50, 30), y = lab_ERC_L10)





# 40-year trade-off
pFR40_5yMaxChg_y40 <- plot_tradeOff("FR40_y40", "ERC_PR.5yMaxChg", df_metrics_y40) + labs(x = lab_x(40, 40), y = lab_5yChg)
pFR50_5yMaxChg_y40 <- plot_tradeOff("FR50_y40", "ERC_PR.5yMaxChg", df_metrics_y40) + labs(x = lab_x(50, 40), y = lab_5yChg)

# 40-year trade off: contribution volatility measured by sd of first difference
pFR40_dsd_y40 <- plot_tradeOff("FR40_y40", "ERC_PR.dsd", df_metrics_y40) + labs(x = lab_x(40, 40), y = lab_dsd)
pFR50_dsd_y40 <- plot_tradeOff("FR50_y40", "ERC_PR.dsd", df_metrics_y40) + labs(x = lab_x(50, 40), y = lab_dsd)


# 40-year volatility vs level of contribution
pERC_5yMaxChg_y40 <- plot_tradeOff("PV.ERC_PR", "ERC_PR.5yMaxChg", df_metrics_y40) + labs(x = lab_ERC, y = lab_5yChg)
  # PV of last 10 years
pERC_L10_5yMaxChg_y40 <- plot_tradeOff("PV.ERC_PR_L10", "ERC_PR.5yMaxChg", df_metrics_y40) + labs(x = lab_ERC_L10, y = lab_5yChg)


# 40-year FR vs level of contribution
pFR40_ERC_y40 <- plot_tradeOff("FR40_y40", "PV.ERC_PR", df_metrics_y40) + labs(x = lab_x(40, 40), y = lab_ERC)
pFR50_ERC_y40 <- plot_tradeOff("FR50_y40", "PV.ERC_PR", df_metrics_y40) + labs(x = lab_x(50, 40), y = lab_ERC)


# 40-year FR vs level of contribution: PV of last 10 years
pFR40_ERC_L10_y40 <- plot_tradeOff("FR40_y40", "PV.ERC_PR_L10", df_metrics_y40) + labs(x = lab_x(40, 40), y = lab_ERC_L10)
pFR50_ERC_L10_y40 <- plot_tradeOff("FR50_y40", "PV.ERC_PR_L10", df_metrics_y40) + labs(x = lab_x(50, 40), y = lab_ERC_L10)



## Display Graphs

# 30-year period
# FR risk and cont volatility graphs 
pFR40_5yMaxChg_y30 + coord_cartesian(xlim = c(0, 38), ylim = c(0, 25)) 
pFR50_5yMaxChg_y30 + coord_cartesian(xlim = c(0, 25), ylim = c(0, 25)) 


# contribution PV and cont volatility 
pERC_5yMaxChg_y30 + coord_cartesian(xlim = c(12, 20), ylim = c(0, 25))
pERC_L10_5yMaxChg_y30 + coord_cartesian(xlim = c(2, 19), ylim = c(0, 20))  # PV of last 10 years


# FR risk and contribution PV
pFR40_ERC_y30 + coord_cartesian(xlim = c(0, 50), ylim = c(12, 20))
pFR40_ERC_L10_y30 + coord_cartesian(xlim = c(0, 50), ylim = c(2, 17))  # PV of last 10 years

pFR50_ERC_y30 + coord_cartesian(xlim = c(0, 50), ylim = c(12, 20))
pFR50_ERC_L10_y30 + coord_cartesian(xlim = c(0, 50), ylim = c(2, 17))  # PV of last 10 years






# 40-year period
#  FR risk and cont volatility 
pFR40_5yMaxChg_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(0, 25)) 
pFR40_dsd_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(0, 7))
# quite similar 

pFR50_5yMaxChg_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(0, 25)) 

# contribution PV and cont volatility 
pERC_5yMaxChg_y40 + coord_cartesian(xlim = c(12, 20), ylim = c(0, 25))
pERC_L10_5yMaxChg_y40 + coord_cartesian(xlim = c(2, 19), ylim = c(0, 20)) # PV of last 10 years


# FR risk and contribution PV
pFR40_ERC_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(12, 18))
pFR40_ERC_L10_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(2, 12)) # PV of last 10 years. 

pFR50_ERC_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(12, 20))
pFR50_ERC_L10_y40 + coord_cartesian(xlim = c(0, 50), ylim = c(2, 17)) # PV of last 10 years


# Notes: 
#  points move toward upper-right when using 40-year perid. 
#  SOA/BRP w/o cap runs have lower FR risk and lower contribtuion volatility
#  SOA/BRP w/  cap run has 0 5-year max change? Need to investigate. 

# Notes:
# Graphs change very little when using C_PR. 
# The difference between 5yChg and 5yMaxChg is small
# Standard deviation of contribution rate may not be a good measure because of the time trend. 
# sd of the CHANGES (first differce) in contribution rate might be a better measure.  







#****************************************************************************************************
#                   Graphs and tables for M1 report  ####
#****************************************************************************************************

# Plotting for the report
p <- 
df_metrics_y30 %>% ggplot(aes_string(x = "FR40_y30" , y = "ERC_PR.5yMaxChg", label = "run.label")) + 
    geom_point(size = 2.5) +
    coord_cartesian(xlim = c(0, 22), ylim = c(0, 23)) + 
    stat_smooth(data = df_metrics_y30 %>% filter(!grepl("soa", runname)),
                                    method = "lm", se = F, fullrange = TRUE,  
                                    color = "darkgray", linetype = 2, size = 0.8) + 
    theme_bw() + theme(legend.position = "none", plot.title=element_text(size=14)) + 
    labs(x = lab_x(40, 30), y = lab_5yChg,
         title = "Pension Contribution Volatility and the Probability of a Low Funded Ratio \nUnder Selected Funding Policies") + 
    geom_text(color = "black", hjust = -0.1, size = 3.5, 
              data = df_metrics_y30 %>% filter(!runname %in% c("A1F075_C30d","A1F075_O30pA5","A1F075_O30pA5_cap"))) +
    geom_text(color = "black", hjust = 0.8, vjust = 1.5, size = 3.5, 
              data = df_metrics_y30 %>% filter(runname %in% c("A1F075_C30d"))) +
    geom_text(color = "black", hjust = 0.5, vjust = 1.3, size = 3.5, 
            data = df_metrics_y30 %>% filter(runname %in% c("A1F075_O30pA5"))) + 
    geom_text(color = "black", hjust = 0.2, vjust = -0.5, size = 3.5, 
            data = df_metrics_y30 %>% filter(runname %in% c("A1F075_O30pA5_cap"))) 
p
  
ggsave(paste0(IO_folder, "/Data_trade_off/trade_off.png"), p, width=12, height=7.5, units="in")

# Table 
table_tradeOff <- df_metrics_y30 %>% select(key.feature, FR40_y30, ERC_PR.5yMaxChg) %>% 
                  mutate(FR40_y30 = FR40_y30/100)


write.xlsx2(table_tradeOff, file = paste0(IO_folder, "/Data_trade_off/table_trade_off.xlsx"), row.names = FALSE)









