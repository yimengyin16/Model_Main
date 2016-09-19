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

source("Functions.R")
source("Functions_Measures.R")

#*****************************************************
##  Combine selected files into a single list.  ####
#*****************************************************
IO_folder <- "IO_M2.1_new"

get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder, "^Outputs_I")




#*****************************************************
## Forward looking scenarios ####
#*****************************************************

# I1F075-6
# I1F075-6b

runs_forward <- c("I1F075-6",
                  "I1F075-6b")

runs_forward_labels   <- c("True expected compound return = 7.5%",
                           "True expected compound return = 6%")


df_forward <- results_all  %>% 
              filter(runname %in% runs_forward, sim >= 0, year <= 30)

df_forward %>% filter(sim == 0) %>% select(runname, sim, year, FR_MA, I.r, AL, SC)



x <- df_forward %>% filter(runname == runs_forward[2]) %>% group_by(sim) %>%  
  summarise(mean.i = mean(i.r))

x$mean.i
x$mean.i %>% mean


df_forward %<>%   
  select(runname, sim, year, AL, MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
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
  ungroup() %>% 
  mutate(runname = factor(runname, 
                          levels = runs_forward, 
                          labels = runs_forward_labels))


g.FR40less <- 
  ggplot(df_forward, aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 20)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,20, 5)) + 
  labs(x = "Year",
       y = "Probability (%)",
       title = "Probability of funded ratio falling below 40% \nat any time prior to and including the given year") + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.FR40less


g.FR.med <- 
  ggplot(df_forward, aes(x = year, y = FR.q50, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 3) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2, color = "black") + 
  coord_cartesian(ylim = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,200, 25)) + 
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = "Median funded ratio") + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
g.FR.med








