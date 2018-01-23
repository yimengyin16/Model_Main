rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(readxl)
library(xlsx)
library(stringr)
library(extrafont)
library(pdata)
library(apitools)

library("grid")
library("gridExtra")

# devtools::install_github("donboyd5/apitools")
# devtools::install_github("donboyd5/pdata")
source("Functions.R")

#****************************************************************************************************
#                System-specific definitions ####
#****************************************************************************************************

runsd       <- "IO_M1_new/"
outputs.dir <- "IO_M1_new/NYSCRF2018/"



#****************************************************************************************************
#                RIG color palette ####
#****************************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"

# font_import(pattern="[A/a]rial")
# loadfonts(device="win")
# loadfonts(device="pdf")

# RIG.theme <- function(){
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
#         panel.background = element_rect(fill = "white", color = "grey"),
#         legend.key = element_rect(fill = "white", color = "grey"),
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# }


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        panel.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill = "white", color = "grey"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}



#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

# ma4 <- function(x) {rollapply(x, 4, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}
rollsd <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) sd(x, na.rm=TRUE), fill=NA, align="right")}
# note that this is sample standard deviation

rollmean <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}

rollmin <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) min(x, na.rm=TRUE), fill=NA, align="right")}

getrun <- function(runfile, dir) {
  fn <- paste0(dir, runfile)
  load(fn)
  df <- outputs_list$results
  return(df)
}


#****************************************************************************************************
#                2018 Analysis of simulations ####
#****************************************************************************************************

allrunfiles <- list.files(runsd, pattern="RData")

# Desired runs, in the order I would like them for the table
# A1F075_0           75% initial Funding; No amortization; No Smoothing NOT IN TABLE
# A1F075_C15d        15-year level dollar - closed
# A1F075_C15p        15-year level percent - closed
# A1F075_C30d        30-year level dollar - closed
# A1F075_C30p        30-year level percent - closed
# A1F075_C30pA5      30-year level percent - closed;  5-year asset smoothing
# A1F075_O15d        15-year level dollar - open
# A1F075_O15p        15-year level percent - open
# A1F075_O30d        30-year level dollar - open
# A1F075_O30p        30-year level percent - open
# A1F075_O30pA5      30-year level percent - open; 5-year asset smoothing
# A1F075_O30pA5_cap  30-year level percent - closed; 5-year asset smoothing; 20% ERC cap
# A1F075_soa3        SOA Blue Ribbon Panel Benchmark


runs <- c("NYSLRS_1",
          "NYSLRS_2",
          "NYSLRS_1_lowReturn",
          "NYSLRS_2_lowReturn")

runs_labels <- c("NYSLRS_1",
                 "Scenario 1 \n7% expected return",
                 "NYSLRS_1_lowReturn",
                 "Scenario 2 \n15 Years of Low Returns")

files2get <- paste0("Outputs_", runs, ".RData")

system.time(df <- ldply(files2get, getrun, runsd, .progress="text"))
glimpse(df)
names(df)
count(df, runname)




#****************************************************************************************************
#                compute risk measures ####
#****************************************************************************************************


df_all.stch <- df  %>% 
  filter(runname %in% runs, sim > 0, year %in% 1:30)

df_all.stch %<>%   
  select(runname, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR) %>%
  group_by(runname, sim) %>% 
  mutate(rowNumber = row_number(),
         FR40less  = cumany(FR_MA <= 40),
         FR60less  = cumany(FR_MA <= 60),
         FR75less  = cumany(FR_MA <= 75),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 64), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))     # cumany(na2zero(c(ERC_rate_5y, ERC_PR) - lag(c(ERC_rate_5y, ERC_PR), 5) >= 10))[-(1:5)],  # NA for 2016 value: excludes impact of new amort payment in 2017 )
  ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less  = 100 * sum(FR40less, na.rm = T)/n(),
            FR60less  = 100 * sum(FR60less, na.rm = T)/n(),
            FR75less  = 100 * sum(FR75less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            ERC_high  = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike  = 100 * sum(ERC_hike, na.rm = T)/n(),
            #  ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm  = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm  = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm  = T)
            
  ) %>% 
  ungroup() %>% 
  mutate(year = year + 2015,
         runname.lab = factor(runname, levels = runs, 
                                   labels = runs_labels)) 
 
  # mutate(runname.lab = factor(runname, 
  #                             levels = runs_all, 
  #                             labels = runs_all_labels))

df_all.stch %>% filter(runname == "NYSLRS_1")
df_all.stch %>% filter(runname == "NYSLRS_2")



df %>% filter(runname == "NYSLRS_1", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i)
df %>% filter(runname == "NYSLRS_2", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i)



#****************************************************************************************************
#                Plot distributions and risk measures ####
#****************************************************************************************************


# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- NULL
fig_CP.RS1.FRdist <- df_all.stch %>% filter(runname %in% c("NYSLRS_2", "NYSLRS_2_lowReturn")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% "RS1", sim == 0) %>% 
  #             select(runname, year, FR_det = FR_MA)) %>%  
  select(runname,runname.lab, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -runname, -year, - runname.lab) %>% 
  mutate(type = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"), 
                             labels =  c("75th percentile", "50th percentile", "25th percentile"))) %>% 
  ggplot(aes(x = year, y = value,
             color = type,
             shape = type
  )) + theme_bw() + 
  facet_grid(. ~ runname.lab)+
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(40,200)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_CP.RS1.FRdist
fig_CP.RS1.FRdist$data




# Distribution of funded ratio 
fig.title <- "Distribution of employer contribution as a percentage of payroll across simulations"
fig.subtitle <- NULL
fig_CP.RS1.ERCdist  <- df_all.stch %>% filter(runname %in% c("NYSLRS_2", "NYSLRS_2_lowReturn")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% "RS1", sim == 0) %>% 
  #             select(runname, year, FR_det = FR_MA)) %>%  
  select(runname,runname.lab, year,  ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year, - runname.lab) %>% 
  mutate(type = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"), 
                       labels =  c("75th percentile", "50th percentile", "25th percentile"))) %>% 
  ggplot(aes(x = year, y = value,
             color = type,
             shape = type
  )) + theme_bw() + 
  facet_grid(. ~ runname.lab)+
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,40)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_CP.RS1.ERCdist 




# Risk of low funded ratio
fig.title <- "Probabilities of funded ratio below 75%, 60%, and 40% in any year up to the given year"
fig.subtitle <- NULL
fig_CP.RS1.FR40less <- df_all.stch %>% filter(runname %in% c("NYSLRS_2", "NYSLRS_2_lowReturn"), year >= 2016) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, runname.lab, year, FR75less, FR60less, FR40less) %>%
  gather(type, value, -runname, -year, -runname.lab) %>% 
  mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = type, shape = type)) + 
  # color = runname, shape = runname)) + 
  theme_bw() + 
  facet_grid(.~runname.lab) +
  geom_point(size = 2) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,80)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Probability of \nfunded ratio below:") + 
  scale_shape_manual(values = c(17,16, 15),  name = "Probability of \nfunded ratio below:") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RS1.FR40less
fig_CP.RS1.FR40less$data %>% filter(year == 2046)



# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- NULL
fig_CP.RS1.ERChike <- df_all.stch %>% filter(runname %in% c("NYSLRS_2", "NYSLRS_2_lowReturn") , year >= 2016) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, runname.lab, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike)) + theme_bw() + 
  facet_grid(.~runname.lab) +
  geom_point(size = 2, color = RIG.blue) + geom_line(color = RIG.blue) + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RS1.ERChike
fig_CP.RS1.ERChike$data %>% filter(year == 2046)



#*************************************************************************
##                        Saving results                              ####
#*************************************************************************

g.height.1col <- 7*0.8
g.width.1col  <- 10*0.8

g.height.2col <- 6*0.8
g.width.2col  <- 13*0.8

g.height.3col <- 5*0.8
g.width.3col  <- 15*0.8


# 1. Current policy: assumption achieved
ggsave(file = paste0(outputs.dir, "fig1.CP.RS1.FRdist.png"),   fig_CP.RS1.FRdist,  height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(outputs.dir, "fig2.CP.RS1.FR40less.png"), fig_CP.RS1.FR40less,height = g.height.2col, width = g.width.2col*1.1)
ggsave(file = paste0(outputs.dir, "fig3.CP.RS1.ERCdist.png"),  fig_CP.RS1.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(outputs.dir, "fig4.CP.RS1.ERChike.png"),  fig_CP.RS1.ERChike, height = g.height.2col, width = g.width.2col*0.8)











