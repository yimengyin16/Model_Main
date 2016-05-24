
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
library(stringr)



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


runs <- c("A1F075_0", 
          "A1F075_C15d", "A1F075_C15p", "A1F075_C30d", "A1F075_C30p", "A1F075_C30pA5",
          "A1F075_O15d", "A1F075_O15p", "A1F075_O30d", "A1F075_O30p", "A1F075_O30pA5", "A1F075_O30pA5_cap",
          "A1F075_soa3")

files2get <- paste0("Outputs_", runs, ".RData")

runsd <- "IO_M1_new/"

system.time(df <- ldply(files2get, getrun, runsd, .progress="text"))
glimpse(df)
names(df)
count(df, runname)

# sim0 i.r=7.5%, initial FR=75%
# simn1 sim==-1  i.r=7.5%, initial FR=100%

# get AL at 7.5% disc rate to use with SOA runs
AL7p5 <- df %>% filter(runname=="A1F075_C15d", sim==0) %>% select(runname, year, sim, AL)

keyvars <- c("runname", "sim", "year", "ERC", "ADC.ER", "i", "i.r", "ADC_PR", "ERC_PR", "FR_MA", "C_PR", "AL", "MA")
df2 <- df %>% select(one_of(keyvars)) %>%
  mutate(ERC_PR.det=ERC_PR[match(paste(0, year, runname), paste(sim, year, runname))],
         dcpr=ERC_PR - ERC_PR.det, # difference from deterministic value
         AL7p5=AL7p5$AL[match(year, AL7p5$year)],
         FR_MA7p5=MA / AL7p5 * 100,
         runname=factor(runname))
glimpse(df2)
# tmp <- df2 %>% filter(abs(FR_MA7p5 / FR_MA - 1)>.01) %>% select(runname, year, sim, MA, AL, AL7p5, FR_MA, FR_MA7p5)
# count(tmp, runname)

# now get probabilities
# Probability that the funded ratio will fall below 40% at some point in the first 30 years
# Probability that employer contributions will rise above 30% of payroll at some point in the first 30 years
# Probability that employer contributions will rise by more than 10 percentage points of payroll in a 5-year period
probs.base <- df2 %>% filter(sim>0, year<=30) %>%
  group_by(runname, sim) %>%
  arrange(year) %>%
  mutate(FR_MA.min=cummin(FR_MA),
         FR_MA7p5.min=cummin(FR_MA7p5),
         ERC_PR.max=cummax(ERC_PR),
         ERC_PR.diff5=ERC_PR - lag(ERC_PR, 5),
         ERC_PR.diff5=ifelse(is.na(ERC_PR.diff5), 0, ERC_PR.diff5),
         ERC_PR.maxdiff5=cummax(ERC_PR.diff5)) %>%
  ungroup %>%
  arrange(runname, sim, year)

probs <- probs.base %>% mutate(frlt40=FR_MA.min<40,
                               fr7p5lt40=FR_MA7p5.min<40,
                               ercprgt30=ERC_PR.max>30,
                               ercprdiffgt10=ERC_PR.maxdiff5>10) %>%
  group_by(runname, year) %>%
  summarise_each(funs(mean(.)*100), frlt40, fr7p5lt40, ercprgt30, ercprdiffgt10)

probs %>% filter(year==30)


probs.yy <- df2 %>% filter(sim>0, year<=30) %>% 
  group_by(runname, sim) %>% 
  arrange(year) %>% 
  mutate(frlt40 = cumany(FR_MA < 40),
         fr7p5lt40 = cumany(FR_MA7p5 < 40),
         ercprgt30 = cumany(ERC_PR > 30),
         ERC_PR.diff5 = ERC_PR - lag(ERC_PR, 5),
         ERC_PR.diff5 = ifelse(is.na(ERC_PR.diff5), 0, ERC_PR.diff5),
         ercprdiffgt10 = cumany(ERC_PR.diff5 > 10)) %>% 
  group_by(runname, year) %>% 
  summarise_each(funs(mean(.)*100), frlt40, fr7p5lt40, ercprgt30, ercprdiffgt10) %>% 
  filter(year == 30)

probs.yy


# probs %>% filter(year==30, runname!="A1F075_0") %>% write_csv("./Results/probs30.csv")



#****************************************
## Figure 14 (Scatter plots) ####
#*****************************************

runs_graph <- paste0("A1F075_", 
                    c("C15d",
                      "C15p",
                      "C30d",
                      "C30p",
                      "C30pA5",
                      "O15d",
                      "O15p",
                      "O30d",
                      "O30p",
                      "O30pA5",
                      "O30pA5_cap",
                      "soa3"))


runs_graph_labels <- c('runname, run.label, key.feature
O15d,         15-year open dollar,                              15-year level dollar - open
O15p,         15-year open percent,                             15-year level percent - open
O30d,         30-year open dollar,                              30-year level dollar - open
O30p,         30-year open percent,                             30-year level percent - open
O30pA5,       "30-year open percent \n5-year assets",             30-year level percent - open; 5-year asset smoothing
C15d,         15-year closed dollar,                            15-year level dollar - closed                       
C15p,         15-year closed percent,                           15-year level percent - closed
C30d,         30-year closed dollar,                            30-year level dollar - closed
C30p,         30-year closed percent,                           30-year level percent - closed
C30pA5,       "30-year closed percent \n       5-year assets",  30-year level perncet - closed; 5-year asset smoothing
O30pA5_cap,   "30-year open percent \n5-year assets;ERC cap",   30-year level perncet - closed; 5-year asset smoothing; 20% ERC cap
soa3,         "SOA Blue Ribbon Benchmark",                    SOA Blue Ribbon Panel Benchmark
')

runs_graph_labels <- read.table(text = runs_graph_labels, header = TRUE,sep = ",", stringsAsFactors = F) %>% 
  mutate_each(funs(str_trim)) %>% 
  mutate(runname = paste0("A1F075_", runname)) 


df_plot <- runs_graph_labels %>% left_join(probs %>% filter(year == 30))
df_plot

fig14 <- 
  df_plot %>% ggplot(aes_string(x = "ercprdiffgt10" , y = "fr7p5lt40", label = "run.label")) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("black", "blue")) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 23)) + 
  stat_smooth(data = df_plot %>% filter(!grepl("soa", runname)),
              method = "lm", se = F, fullrange = TRUE,  
              color = "darkgray", linetype = 2, size = 0.8) + 
  theme_bw() + theme(legend.position = "none", plot.title=element_text(size=18)) + 
  labs(y = "Risk of low funding: \nProbability of funded ratio falling below 40% during first 30 years (%)", 
       x = "Contribution volatility: \nProbability that employer contribution will rise by more than 10% of payroll in a 5-year period (%)",
       title = "Risk of severe underfunding and contribution volatility\n under selected funding policies ") + 
  geom_text(color = "black", hjust = -0.1, size = 3.5, 
            data = df_plot %>% filter(!runname %in% c("A1F075_O15d", "A1F075_O30pA5", "A1F075_O30pA5_cap",
                                                      "A1F075_O30d", "A1F075_C15d", "A1F075_C15p"))) +
  geom_text(color = "black", hjust = -0.05, vjust = -0.4, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_O15d"))) +
  geom_text(color = "black", hjust = 0.5, vjust = 1.3, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_O30pA5"))) + 
  geom_text(color = "black", hjust = 0.4, vjust = -0.5, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_O30pA5_cap"))) + 
  geom_text(color = "black", hjust = 0.4, vjust = -1, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_O30d"))) + 
  geom_text(color = "black", hjust = 0, vjust = 1.7, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_C15d"))) +
  geom_text(color = "black", hjust = -0.05, vjust = 0, size = 3.5, 
            data = df_plot %>% filter(runname %in% c("A1F075_C15p"))) 

fig14

# p2 <- 
#   df_plot %>% ggplot(aes_string(x = "ercprdiffgt10" , y = "fr7p5lt40", label = "run.label")) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("black", "blue")) + 
#   coord_cartesian(xlim = c(0, 100), ylim = c(0, 23)) + 
#   stat_smooth(data = df_plot %>% filter(!grepl("soa", runname)),
#               method = "lm", se = F, fullrange = TRUE,  
#               color = "darkgray", linetype = 2, size = 0.8) + 
#   theme_bw() + theme(legend.position = "none", plot.title=element_text(size=18)) + 
#   labs(y = "Risk of low funidng: \nProbability of funded ratio falling below 40% during first 30 years (%)", 
#        x = "Contribution Volatility:\nMaximum increase in any 5-year period of employer contributions \nas % of payroll (median of 1,000 runs)",
#        title = "Trade-off between contribution volatility and risk of severe low funding") + 
#   geom_text(color = "black", hjust = -0.1, size = 3.5, 
#             data = df_plot %>% filter(!runname %in% c("A1F075_C30d","A1F075_O30pA5","A1F075_O30pA5_cap"))) +
#   geom_text(color = "black", hjust = 0.8, vjust = 1.5, size = 3.5, 
#             data = df_plot %>% filter(runname %in% c("A1F075_C30d"))) +
#   geom_text(color = "black", hjust = 0.5, vjust = 1.3, size = 3.5, 
#             data = df_plot %>% filter(runname %in% c("A1F075_O30pA5"))) + 
#   geom_text(color = "black", hjust = 0.2, vjust = -0.5, size = 3.5, 
#             data = df_plot %>% filter(runname %in% c("A1F075_O30pA5_cap"))) 

#p

size_fig14 <-  0.9
w_fig14 <- size_fig14 * 12
h_fig14 <- size_fig14 *8

ggsave(paste0("IO_M1_new/M1_figures/", "fig14_trade_off1.png"), fig14, width=w_fig14, height=h_fig14, units="in")



#***************************
## Figure 12 ####
#***************************

df_fig <- df %>% filter(runname == "A1F075_O30pA5", year <= 30) %>% 
  select(runname, sim, year, FR_MA) %>% 
  group_by(year) %>% 
  summarize(fr_q75 = quantile(FR_MA, 0.75),
            fr_q50 = median(FR_MA),
            fr_q25 = quantile(FR_MA, 0.25)) %>% 
  ungroup %>% 
  gather(qtl, value, -year) %>% 
  mutate(qtl = factor(qtl, levels = c("fr_q75", "fr_q50", "fr_q25"),
                           labels = c("75th percentile", "median", "25th percentile")))
  
fig12 <- 
df_fig %>% 
  ggplot(aes(x = year, y = value, color = qtl, shape = qtl)) + theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = c(75, 100), linetype = c(2, 1), size  = 0.5)+
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
  scale_y_continuous(breaks = seq(0, 150, 5)) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  labs(x = "Year", y = "Funded ratio (%)",
       title = "Funded ratio quartiles with common funding policy \n30-year level-percent open, 5-year asset smoothing \n(Initial funded ratio of 75%)")
  
fig12

ggsave(paste0("IO_M1_new/M1_figures/", "fig12_FR.qts.png"), fig12, width=7, height=5, units="in")


  

#***************************
## Figure 13 ####
#***************************


df_fig <- df %>% filter(runname %in% c("A1F075_O30pA5", "A1F075_O30pA5_cap"), year <= 40) %>% 
  select(runname, sim, year, FR_MA) %>% 
  group_by(runname, year) %>% 
  summarize(fr_q50 = median(FR_MA),
            fr_q25 = quantile(FR_MA, 0.25)) %>% 
  ungroup %>% 
  gather(qtl, value, -year, -runname) %>% 
  mutate(runname_qtl = paste(runname, qtl, sep = "_"),
         runname_qtl = factor(runname_qtl, levels = c("A1F075_O30pA5_fr_q50",
                                      "A1F075_O30pA5_cap_fr_q50",
                                      "A1F075_O30pA5_fr_q25",
                                      "A1F075_O30pA5_cap_fr_q25"),
                           labels = c("Median \nNo ERC cap", 
                                      "Median \n20% ERC cap", 
                                      "25th percentile \nNo ERC cap", 
                                      "25th percentile \n20% ERC cap")))

df_fig


fig13 <-
df_fig %>% 
  ggplot(aes(x = year, y = value, color = runname_qtl, shape = runname_qtl)) + theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  scale_y_continuous(breaks = seq(0, 150, 5)) +
  scale_color_manual(values = c("dodgerblue4", "firebrick3", "dodgerblue", "firebrick1")) + 
  guides(col   = guide_legend(title = NULL, label.position = "right", keyheight = 2), 
         shape = guide_legend(title = NULL, label.position = "right", keyheight = 2)) + 
  labs(x = "Year", y = "Funded ratio (%)",
       title = "Median and 25th percentile funded ratio \nof plans with and without contribution cap")
 

fig13

ggsave(paste0("IO_M1_new/M1_figures/", "fig13_FRcap.png"), fig13, width=8, height=5, units="in")










