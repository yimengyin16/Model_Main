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

IO_folder <- "IO_M1_new/"


expand_apply <- function(x, fun, ...){
  # Applying the given function to expanding windows of vector x.
  zoo::rollapply(x, seq_along(x), fun, ...,  fill = NA, align = "right")
}

lab_x     <- function(x, y){paste0("Probability of funded ratio falling below ", x, "% during first ", y, " years (%)")}

#****************************************************************************************************
#               ## 1. Loading simulation results ####
#****************************************************************************************************

load_run <- function(runname, folder){
  load(paste0(folder, runname))
  outputs_list$results
}

run_select <- dir(IO_folder, pattern = "^Outputs_")

results_all <- ldply(run_select, load_run, IO_folder)

results_all %<>% mutate(MA_B   = 100 * MA / B)





#****************************************************************************************
# Graphs for single runs ####
#****************************************************************************************

# Graphs for Contributrion rate and funded status. 

"A1F075_O30pA5"

df <- results_all %>% select(runname, year, sim, FR_MA, ERC_PR, i.r) %>% 
                      filter(runname == "A1F075_O30pA5", sim %in% c(56, 228), year <=30) %>% 
                      mutate(sim = factor(sim, labels = c("Simulation #56", "Simulation #228"))) %>% 
                      group_by(sim) %>% 
                      mutate(geoReturn.cum = expand_apply(i.r, get_geoReturn))



df %>% group_by(runname, sim) %>% 
  mutate(FR40 = cumany(FR_MA  <= 40)) %>% 
  group_by(runname, year) %>% 
  summarise(FR40 = 100 * sum(FR40)/n())



                      
p_ERC <- 
df %>% select(runname, year, sim, ERC_PR) %>% 
       ggplot(aes(x = year, y = ERC_PR, color = factor(sim), shape = factor(sim) )) + theme_bw() + 
       geom_point(size = 2.5) + 
       geom_line(size = 1) + 
       scale_color_discrete(name = "") + 
       scale_shape_discrete(name = "") + 
       scale_y_continuous(breaks = c(seq(-50, 5, 5), seq(10, 50, 5)), name = "Percent") + 
       scale_x_continuous(breaks = seq(0, 30, 5), name = "Year") +
       labs(x = "Year", y = "Percent", title = "Employer contribution as % of payroll") + 
       theme(legend.position = "bottom",
             plot.title=element_text(size=17), legend.text = element_text(size = 14),
             axis.text.x = element_text(size = 13),
             axis.text.y = element_text(size = 13),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14)) 
       
       
p_FR <- 
df %>% select(runname, year, sim, FR_MA) %>% 
  ggplot(aes(x = year, y = FR_MA, color = factor(sim), shape = factor(sim) )) + theme_bw() + 
  geom_point(size = 2.5) + 
  geom_line(size = 1) + 
  scale_color_discrete(name = "") + 
  scale_shape_discrete(name = "") + 
  scale_y_continuous(breaks = c(seq(-50, 150, 10)), name = "Percent") + 
  scale_x_continuous(breaks = seq(0, 30, 5), name = "Year") +
  geom_hline(yintercept = 100, linetype = "dashed", size = 1) + 
  labs(x = "Year", y = "Percent", title = "Funded Ratio (based on market asset value)") + 
  theme(legend.position = "bottom",
        plot.title=element_text(size=17), legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 


p_r <- 
df %>% select(runname, year, sim, i.r, geoReturn.cum) %>% 
       gather(variable, value, -runname, -year, -sim) %>% 
       ggplot(aes(x = year, y = 100 * value, color = variable )) + theme_bw() + 
       geom_point(size = 2.25) + 
       geom_line(size = 1) + 
       facet_grid(. ~ sim ) + 
       scale_color_discrete(name = "",    labels = c("Annual return", "Cumulative Geom Mean Return")) +
       scale_shape_discrete(name = "",    labels = c("Annual return", "Cumulative Geom Mean Return")) +
       scale_y_continuous(breaks = c(seq(-50, 5, 5), 7.5, seq(10, 50, 5)), name = "Percent") + 
       scale_x_continuous(breaks = seq(0, 30, 5), name = "Year") +
       geom_hline(yintercept = 7.5, linetype = "dashed") +
       labs(title = "Annual returns and cumulative compound annual returns \nunder 2 single runs with 30-year compound annual returns close to 7.5% ")+
       theme(legend.position = "bottom",
             plot.title=element_text(size=18), legend.text = element_text(size = 15),
             axis.text.x = element_text(size = 13),
             axis.text.y = element_text(size = 13),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14))
       

ml <- marrangeGrob(list(p_ERC, p_FR), nrow = 1, ncol = 2, top = NULL)
ml

ggsave(paste0("RIG_10-18-15/p2.png"), ml, width=13*0.8, height=7*0.8, units="in") 
ggsave(paste0("RIG_10-18-15/p1.png"), p_r, width=13*0.8, height=7*0.8, units="in") 




#****************************************************************************************
# Trade-offs ####
#****************************************************************************************

load("IO_M1_new/Data_trade_off/df_metrics.RData")

runs_all_labels <- c('runname, run.label
                     O30pA5,       "Common Smoothing Policy \n(30-year open percent \n 5-year assets)"
                     O30pA5_cap,   "Common Smoothing Policy \nwith ERC Cap"                 
                     C15d,         "\nLess Backloaded Policy \n(15-year closed dollar)"                                                   
                     soa3,         "SOA Blue Ribbon Benchmark \n(Less smoothed and low discount rate)"')

runs_all_labels

runs_all_labels <- read.table(text = runs_all_labels, header = TRUE, sep = ",", stringsAsFactors = F) %>% 
  mutate_each(funs(str_trim)) %>% 
  mutate(runname = paste0("A1F075_", runname)) 


df_metrics_y30 %<>% left_join(runs_all_labels) %>% 
                    filter(runname %in% c("A1F075_C15d",
                                          "A1F075_O30pA5",
                                          "A1F075_O30pA5_cap",
                                          "A1F075_soa3"))
df_metrics_y30



lab_5yChg <- "Contribution Volatility:\nMaximum increase in any 5-year period of employer contributions as % of payroll \n(median of 1,000 runs)"

p_tradeOff <- 
  df_metrics_y30 %>% ggplot(aes_string(x = "FR40_y30" , y = "ERC_PR.5yMaxChg", label = "run.label")) + 
  geom_point(size = 5, color = "blue") +
  coord_cartesian(xlim = c(0, 22), ylim = c(0, 23)) + 
  theme_bw() + theme(legend.position = "none", plot.title=element_text(size=20),
                     axis.text.x = element_text(size = 15),
                     axis.text.y = element_text(size = 15),
                     axis.title.x = element_text(size = 15)) + 
  labs(x = lab_x(40, 30), y = lab_5yChg,
       title = "Pension Contribution Volatility and the Probability of a Low Funded Ratio \nUnder Selected Funding Policies") + 
       geom_text(color = "black", hjust = -0.1, size = 4.8, 
                 data = df_metrics_y30 %>% filter(runname %in% c("A1F075_C15d"))) +
       geom_text(color = "black", hjust = 0.25, vjust = -0.5, size = 4.8, 
            data = df_metrics_y30 %>% filter(runname %in% c("A1F075_soa3"))) +
       geom_text(color = "black", hjust = 1, vjust = 1.3, size = 4.8, 
            data = df_metrics_y30 %>% filter(runname %in% c("A1F075_O30pA5"))) + 
       geom_text(color = "black", hjust = 0.4, vjust = -0.5, size = 4.8, 
            data = df_metrics_y30 %>% filter(runname %in% c("A1F075_O30pA5_cap"))) 
p_tradeOff

ggsave(paste0("RIG_10-18-15/p3.png"), p_tradeOff, width=12*0.9, height=7.5*0.9, units="in") 





runs <- c("A1F075_O30pA5",
          "A1F075_O30pA5_cap",
          "A1F075_C15d",
          "A1F075_soa3")

run.labels <- c("Common Policy",
            "Common Policy w/ ERC Cap",
            "Less Backloaded Policy",
            "SOA Blue Ribbon"
)



df2 <- results_all %>% select(runname, year, sim, FR_MA, ERC_PR, i.r) %>% 
       filter(runname %in% runs, year <=30) %>% 
       mutate(runname = factor(runname, levels = runs, labels = run.labels)) %>% 
       group_by(runname, sim) %>% 
       mutate(FR40 = cumany(FR_MA  <= 40)) %>% 
       group_by(runname, year) %>% 
       summarise(FR40 = 100 * sum(FR40)/n(),
            ERC_med = median(ERC_PR, na.rm = TRUE)) 
      
p_FR40 <- 
  df2 %>% ggplot(aes(x = year, y = FR40, color = runname, shape = runname)) + theme_bw() + 
          geom_line(size = 1) + 
          geom_point(size = 3) + 
          scale_color_discrete(name = "") +
          scale_shape_discrete(name = "") +
          scale_y_continuous(breaks = seq(0, 50, 5)) + 
          scale_x_continuous(breaks = seq(0, 30, 5)) +
          labs(x = "Year", y = "Percent",
               title = "Probability of Funded Ratio Falling Below 40% \nunder Alternative Funding Policies \n(Initial Funding Ratio of 75%)") + 
          theme(legend.justification=c(0,1), legend.position=c(0,1),
                legend.background = element_rect(colour = "black", size = 0.2, color = "gray"),
                legend.text = element_text(size = 12),
                plot.title=element_text(size=16),
                axis.text.x = element_text(size = 13),
                axis.text.y = element_text(size = 13),
                axis.title.x = element_text(size = 13))
      
  

p_ERC.med <- 
  df2 %>% ggplot(aes(x = year, y = ERC_med, color = runname, shape = runname)) + theme_bw() + 
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  scale_color_discrete(name = "") +
  scale_shape_discrete(name = "") +
  scale_y_continuous(breaks = seq(0, 50, 5)) + 
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  labs(x = "Year", y = "Employer Contribution as % of payroll",
       title = "Median Employer Contribution Rate \nunder Alternative Funding Policies \n(Initial Funding Ratio of 75%)") + 
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.background = element_rect(colour = "black", size = 0.2, color = "gray"),
        legend.text = element_text(size = 12),
        plot.title=element_text(size=16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))




ml2 <- marrangeGrob(list(p_FR40, p_ERC.med), nrow = 1, ncol = 2, top = NULL)
ml2

ggsave(paste0("RIG_10-18-15/p4.png"), ml2, width=13*0.8, height=7*0.8, units="in") 
















