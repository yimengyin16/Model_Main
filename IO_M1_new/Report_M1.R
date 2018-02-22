
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
outputs.dir <- "IO_M1_new/M1_outputs/"

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

RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        panel.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill = "white", color = "grey"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
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
#                2016 Analysis of simulations ####
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


runs <- c("A1F075_0", 
          "A1F075_C15d", "A1F075_C15p", "A1F075_C30d", "A1F075_C30p", "A1F075_C30pA5",
          "A1F075_O15d", "A1F075_O15p", "A1F075_O30d", "A1F075_O30p", "A1F075_O30pA5", "A1F075_O30pA5_cap",
          "A1F075_soa3")
files2get <- paste0("Outputs_", runs, ".RData")

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


# now get probabilities
# Probability that the funded ratio will fall below 40% during the first 30 years
# Probability that employer contributions will rise above 30% of payroll during the first 30 years
# Probability that employer contributions will rise by more than 10% of payroll in a 5-year period
probs.base <- df2 %>%
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

probs <- probs.base %>% 
  filter(sim>=0) %>%
  mutate(frlt40=FR_MA.min<40,
         fr7p5lt40=FR_MA7p5.min<40,
         fr7p5lt50=FR_MA7p5.min<50,
         fr7p5lt60=FR_MA7p5.min<60,
         fr7p5lt70=FR_MA7p5.min<70,
         ercprgt30=ERC_PR.max>30,
         ercprdiffgt10=ERC_PR.maxdiff5>10) %>%
  group_by(runname, year, deterministic=sim==0) %>%
  summarise_each(funs(mean(.)*100), frlt40, fr7p5lt40, fr7p5lt50, fr7p5lt60, fr7p5lt70, ercprgt30, ercprdiffgt10)

probs %>% filter(year==30, !deterministic)

# probs %>% filter(year==30, !deterministic, runname!="A1F075_0") %>% write_csv("./Results/probs30.csv")

count(probs, year)

df %>% filter(runname == "A1F075_O30pA5", sim == -1)


#****************************************************************************************************
#                2016 Runs of interest ####
#****************************************************************************************************
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

# runs of interest
roi <- c("Deterministic", "A1F075_C15d", "A1F075_C30d", "A1F075_O30pA5", "A1F075_soa3")
roi.labs <- c("Deterministic-30-open-5",
              "15-year closed dollar", 
              "30-year closed percent",
              "30-year open percent asset-5",
              "SOA Blue Ribbon")
roi.labs.2lines <- c("Deterministic-30-open-5",
                     "15-year\nclosed dollar", 
                     "30-year\nclosed percent",
                     "30-year\nopen percent asset-5",
                     "SOA Blue\nRibbon")

# slightly different labels for policy brief
roi.pb <- c("Deterministic", "A1F075_C15d", "A1F075_C30d", "A1F075_O30pA5")
roi.labs.pb <- c("Deterministic",
              "15-year closed dollar", 
              "30-year closed percent",
              "30-year open percent asset-5")

roi.labs.2lines.pb <- c("Deterministic",
                     "15-year\nclosed dollar", 
                     "30-year\nclosed percent",
                     "30-year\nopen percent asset-5")


#****************************************************************************************************
#                2016 Employer contributions ####
#****************************************************************************************************
# http://stackoverflow.com/questions/32275113/ggplot2-increase-space-between-legend-keys

# median ERC ####
glimpse(probs.base)
tmp <- probs.base %>%  mutate(deterministic=(sim==0 & runname=="A1F075_O30pA5")) %>%
  filter(runname %in% roi, sim>0 | deterministic) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname))) %>%
  group_by(runname, year) %>%
  summarise(ERC_PR=median(ERC_PR)) %>% 
  ungroup %>%
  filter(year<=40) %>%
  mutate(runname2=factor(runname, levels=roi, labels=roi.labs, ordered=TRUE),
         runname2lines=factor(runname, levels=roi, labels=roi.labs.2lines, ordered=TRUE))

count(tmp, runname2, runname)
glimpse(tmp)

xtitle <- "Year"
ytitle <- "Employer contribution as % of payroll (%)"
gtitle <- "Median employer contribution as % of payroll, selected funding scenarios"
p <- ggplot(data=tmp, aes(x=year, y=ERC_PR, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 5)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue, RIG.purple)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
p
ggsave(paste0(outputs.dir, "fig6_erc_prmedians.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig6_erc_prmedians.pdf"), p, width=10, height=6, units="in")



# ERC>30% prob ####
count(probs, runname)
tmp <- probs %>%
  filter(runname %in% roi, !deterministic | (deterministic & runname=="A1F075_O30pA5")) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname)),
         runname2=factor(runname, levels=roi, labels=roi.labs, ordered=TRUE),
         runname2lines=factor(runname, levels=roi, labels=roi.labs.2lines, ordered=TRUE))
count(tmp, runname2, runname)
glimpse(tmp)
xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle <- "Probability that employer contributions will rise\nabove 30% of payroll during the first 30 years"
p <- ggplot(data=tmp %>% filter(year<=30), aes(x=year, y=ercprgt30, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue, RIG.purple)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
p
ggsave(paste0(outputs.dir, "fig7_ercprgt30.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig7_ercprgt30.pdf"), p, width=10, height=6, units="in")

# ERC increase > 10% prob ####
tmp <- probs %>%
  filter(runname %in% roi, !deterministic | (deterministic & runname=="A1F075_O30pA5")) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname)),
         runname2=factor(runname, levels=roi, labels=roi.labs, ordered=TRUE),
         runname2lines=factor(runname, levels=roi, labels=roi.labs.2lines, ordered=TRUE))
xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle <- "Probability that employer contributions will rise\nby more than 10% of payroll in a 5-year period"
p <- ggplot(data=tmp %>% filter(year<=30), aes(x=year, y=ercprdiffgt10, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue, RIG.purple)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
   
p
ggsave(paste0(outputs.dir,"fig8_ercprdiffgt10.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir,"fig8_ercprdiffgt10.pdf"), p, width=10, height=6, units="in")




#****************************************************************************************************
#                2016 Funded ratio ####
#****************************************************************************************************
glimpse(probs)

# median FR ####
glimpse(probs.base)
tmp <- probs.base %>%  mutate(deterministic=(sim==0 & runname=="A1F075_O30pA5")) %>%
  filter(runname %in% roi, sim>0 | deterministic) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname))) %>%
  group_by(runname, year) %>%
  summarise(FR_MA=median(FR_MA),
            FR_MA7p5=median(FR_MA7p5)) %>% 
  ungroup %>%
  filter(year<=30) %>%
  mutate(runname2=factor(runname, levels=roi, labels=roi.labs, ordered=TRUE),
         runname2lines=factor(runname, levels=roi, labels=roi.labs.2lines, ordered=TRUE))

count(tmp, runname2, runname)
glimpse(tmp)

xtitle <- "Year"
ytitle <- "Funded ratio (%)"
gtitle <- "Median funded ratio, selected funding scenarios"
p <- ggplot(data=tmp, aes(x=year, y=FR_MA7p5, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  geom_hline(yintercept=75, linetype="dashed") +
  geom_hline(yintercept=100, linetype="solid") +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=c(75, seq(0, 150, 10))) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue, RIG.purple)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
p
ggsave(paste0(outputs.dir,"fig9_frmedians.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir,"fig9_frmedians.pdf"), p, width=10, height=6, units="in")


# Prob FR < 40% ####

tmp <- probs %>%
  filter(runname %in% roi, !deterministic | (deterministic & runname=="A1F075_O30pA5")) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname)),
         runname2=factor(runname, levels=roi, labels=roi.labs, ordered=TRUE),
         runname2lines=factor(runname, levels=roi, labels=roi.labs.2lines, ordered=TRUE))

xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle <- "Probability that the funded ratio will fall\nbelow 40% during the first 30 years"
p <- ggplot(data=tmp %>% filter(year<=30), aes(x=year, y=fr7p5lt40, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 20, 2), limits=c(0, 18)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue, RIG.purple)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
p
ggsave(paste0(outputs.dir,"fig10_fr7p5lt40.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir,"fig10_fr7p5lt40.pdf"), p, width=10, height=6, units="in")


# FR probabilities for selected funded-ratio thresholds ####
# fr7p5lt40, fr7p5lt50, fr7p5lt60, fr7p5lt70
xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle1 <- "Probability that the funded ratio will fall below a given threshold during the first 30 years"
gtitle2 <- "under 30-year open level-percent funding, with 5-year asset smoothing"
gtitle <- paste0(gtitle1, "\n", gtitle2)
frcut <- c("fr7p5lt70", "fr7p5lt60", "fr7p5lt50", "fr7p5lt40")
frcut.labs <- c("70%", "60%", "50%", "40%")
tmp <- probs %>% filter(year<=30, runname=="A1F075_O30pA5", !deterministic) %>%
  ungroup %>%
  select(year, starts_with("fr7p")) %>% 
  gather(variable, value, -year) %>%
  mutate(varf=factor(variable, levels=frcut, labels=frcut.labs, ordered=TRUE))
p <- ggplot(data=filter(tmp, variable!="fr7p5lt70"), aes(x=year, y=value, colour=varf)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  geom_hline(yintercept = 50, linetype="dashed") +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 5)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) +
  ggtitle(gtitle) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80")) +
  guides(colour=guide_legend(title="Funding threshold"), size=guide_legend(title=NULL))
p


ggsave(paste0(outputs.dir, "fig11_frthresholds.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig11_frthresholds.pdf"), p, width=10, height=6, units="in")





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


df_plot <- runs_graph_labels %>% 
  left_join(probs %>% filter(year == 30, deterministic == FALSE))
df_plot %>% print

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

ggsave(paste0(outputs.dir, "fig14_trade_off1.png"), fig14, width=w_fig14, height=h_fig14, units="in")
ggsave(paste0(outputs.dir, "fig14_trade_off1.pdf"), fig14, width=w_fig14, height=h_fig14, units="in")


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
  geom_line(size = 1.2) + 
  geom_point(size = 1.8) + 
  geom_hline(yintercept = c(75, 100), linetype = c(2, 1), size  = 0.5)+
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  labs(x = "Year", y = "Funded ratio (%)",
       title = "Funded ratio quartiles with common funding policy \n30-year level-percent open, 5-year asset smoothing \n(Initial funded ratio of 75%)") + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
        # text = element_text(family = "Arial"))
        # axis.text  = element_text(family = "Arial"),
        # axis.title = element_text(family = "Arial"))  


# fonttable()

fig12

ggsave(paste0(outputs.dir, "fig12_FR.qts.png"), fig12, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig12_FR.qts.pdf"), fig12, width=10, height=6, units="in")



  

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
  geom_line(size = 1.2) + 
  geom_point(size = 1.8) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  scale_y_continuous(breaks = seq(0, 150, 5)) +
  scale_color_manual(values = c(RIG.blue, RIG.red, "dodgerblue", "firebrick1")) + 
  guides(col   = guide_legend(title = NULL, label.position = "right", keyheight = 2), 
         shape = guide_legend(title = NULL, label.position = "right", keyheight = 2)) + 
  labs(x = "Year", y = "Funded ratio (%)",
       title = "Median and 25th percentile funded ratio \nof plans with and without contribution cap") + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))  

fig13

ggsave(paste0(outputs.dir, "fig13_FRcap.png"), fig13, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig13_FRcap.pdf"), fig13, width=10, height=6, units="in")





#****************************************************************************************************
#                Figure 1 Interest rate history ####
#****************************************************************************************************
# # FRED
# # DGS10 10-Year Treasury Constant Maturity Rate
# # DGS30 30-Year Treasury Constant Maturity Rate
# # http://www.federalreserve.gov/releases/h15/current/h15.pdf
# # http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/yieldmethod.aspx.
# # 30 is missing 1960s to ~1975, and also mid 2000s
# 
# # some historical earnings assumptions
# src94 <- "7.83 mean Survey of State and Local Gov ERS June 1994.pdf Zorn p.40"
# src89 <- "http://www.uh.edu/~bsorense/Mitchell%26SmithPensions.pdf"
# # https://www.osc.state.ny.us/retire/word_and_pdf_documents/reports/actuarial_assumption/aa_2015.pdf
# 
# hist.ir.s <- "fyear, value, source
# 1975, 5, 1978 Pension Task Force Report On Public Employee Retirement Systems House Committee on Education and Labor p. 161
# 1989, 7.6, Pension Funding in the Public Sector Olivia S. Mitchell and Robert S. Smith The Review of Economics and Statistics p.281
# 1990, 7.76, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
# 1991, 7.81, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
# 1992, 7.83, Zorn
# 1994, 7.84, Zorn
# 1996, 7.84, Zorn p.12
# 1998, 7.88, Zorn p.12
# 2000, 7.91, Zorn p.12"
# 
# # 1990-1992, 1994, 1996, 1998, and 2000 are generally available from 
# 
# irhist <- read_csv(hist.ir.s) %>% mutate(series="survey")
# irhist
# 
# fn <- "NY_CRF_InvestReturnAssumption.xlsx"
# nycrf <- read_excel(paste0("./Data/", fn)) %>% select(fyear, value=crfassumed) %>% mutate(value=value*100, series="nycrf")
# glimpse(nycrf)
# 
# 
# t10 <- FRED("DGS10")
# t30 <- FRED("DGS30")
# t30 %>% group_by(year) %>% summarise(value=mean(value, na.rm=TRUE))
# glimpse(t10)
# ht(t10)
# 
# rates <- bind_rows(t10, t30)
# 
# qplot(date, value, data=rates, colour=series, geom=c("point", "line"))
# 
# rates %>% group_by(series, year) %>%
#   summarise(value=mean(value, na.rm=TRUE)) %>%
#   qplot(year, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.fy <- rates %>% mutate(fyear=ifelse(month(date)>=7, year+1, year)) %>%
#   group_by(series, fyear) %>%
#   summarise(value=mean(value, na.rm=TRUE))
# 
# rates.fy %>% group_by(series) %>%
#   mutate(diff=value - value[match(2016, fyear)]) %>%
#   select(series, fyear, diff) %>%
#   spread(series, diff)
# 
# glimpse(ppd)
# names(ppd)[str_detect(names(ppd), "sset")]
# ppdir.fy <- ppd %>% group_by(fyear=fy) %>%
#   summarise(value=mean(InvestmentReturnAssumption_GASB, na.rm=TRUE)*100,
#             wvalue=sum(InvestmentReturnAssumption_GASB * MktAssets_net, na.rm=TRUE) /
#               sum(MktAssets_net * !is.na(InvestmentReturnAssumption_GASB), na.rm=TRUE) * 100) %>%
#   mutate(series="survey")
# 
# rates.all <- bind_rows(ppdir.fy, rates.fy, irhist, nycrf) %>% arrange(series, fyear)
# saveRDS(rates.all, "./Data/rates.all.rds")


# rates.all <- readRDS(paste0(runsd, "rates.all.rds"))
# 
# rates.all %>% qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.all %>% filter(!series %in% c("DGS10")) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.all %>% filter(!series %in% c("DGS30")) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.all %>% filter(!series %in% c("DGS30", "nycrf"), fyear>=1973) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.all %>% filter(!series %in% c("DGS10", "nycrf"), fyear>=1973) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# 
# rates.all %>% filter(!series %in% c("DGS30", "nycrf"), fyear>=1983) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# rates.all %>% filter(!series %in% c("DGS10", "nycrf"), fyear>=1983) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# 
# 
# rates.all %>% filter(!series %in% c("nycrf"), fyear>=1983) %>% 
#   qplot(fyear, value, data=., colour=series, geom=c("point", "line"))
# rates.all %>% filter(series %in% "survey", fyear %in% 1973:1989)



rates.all <- readRDS(paste0(runsd, "rates.all.rds"))

levs <- c("DGS10", "survey")
labs <- c("10-year \nTreasury yield", "Average \nassumed return")
pdat <- rates.all %>% filter(series %in% levs, fyear>=1973) %>%
  mutate(value=ifelse(fyear<1989 & series=="survey", NA, value),
         seriesf=factor(series, levels=levs, labels=labs))
gtitle <- "Assumed investment returns of state and local retirement systems\nand risk-free returns"

p <- ggplot(data=pdat, aes(x=fyear, y=value, colour=seriesf)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c(RIG.red,RIG.blue)) +
  geom_segment(x = 1973, y = 5.0, xend = 1989, yend = 7.6, colour = RIG.blue, linetype="dashed", size=.05) +
  geom_point(x=1973, y=5.0, colour = RIG.blue) +
  scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
  scale_x_continuous(name="Pension fund fiscal year", breaks=seq(1975, 2020, 5)) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle(gtitle) + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(2, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))  

p
ggsave(paste0(outputs.dir, "fig1_invest_erorvsriskfree.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig1_invest_erorvsriskfree.pdf"), p, width=10, height=6, units="in")


function (svec, key = FRED_defaultkey(), full = FALSE) 
{
  fredroot <- "http://api.stlouisfed.org/fred/"
  fredpost <- paste0("&api_key=", key, "&file_type=json")
  checknickname <- function(series) {
    if (series == "gdp.q") 
      series <- "GDP"
    else if (series == "gdp.a") 
      series <- "GDPA"
    else if (series == "gdppi.q") 
      series <- "GDPCTPI"
    else if (series == "gdppi.a") 
      series <- "B191RG3A086NBEA"
    else if (series == "rgdp.q") 
      series <- "GDPC1"
    else if (series == "rgdp.a") 
      series <- "GDPCA"
    return(series)
  }
  getoneseries <- function(series) {
    series <- checknickname(series)
    seriesinfo <- paste0("series/observations?series_id=", 
                         series)
    url <- paste0(fredroot, seriesinfo, fredpost)
    result <- jsonlite::fromJSON(url)
    df <- data.frame(result$observations) %>% mutate(value = cton(value), 
                                                     date = as.Date(date))
    firstnonna <- min(which(!is.na(df$value)))
    df <- df %>% filter(row_number() >= firstnonna)
    ddiff <- df$date[nrow(df)] - df$date[nrow(df) - 1]
    if (ddiff >= 0 & ddiff < 7) 
      freq <- "D"
    else if (ddiff >= 27 & ddiff <= 32) 
      freq <- "M"
    else if (ddiff >= 85 & ddiff <= 95) 
      freq <- "Q"
    else if (ddiff >= 360 & ddiff <= 370) 
      freq <- "A"
    else freq <- "Unknown"
    df <- df %>% mutate(freq = freq, year = lubridate::year(date))
    return(df)
  }
  df2 <- data.frame(series = svec) %>% group_by(series) %>% 
    do(getoneseries(.$series))
  if (!full) 
    df2 <- df2 %>% select(date, year, freq, value)
  return(df2)
}





#****************************************************************************************************
#                Individual runs ####
#****************************************************************************************************

# let's get the ir and make my own categories
cdist <- df %>% filter(runname=="A1F075_O30pA5", year<=50) %>% select(runname, year, sim, i.r) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(i.rc=cumprod(1 + i.r)^(1/year)-1)

quantile(cdist$i.rc[cdist$year==30])

# get runs that are close to 7.5
avgruns <- cdist %>% filter(sim>0, year==30, i.rc>.074, i.rc<.076)
quantile(avgruns$i.rc)

# now, see where these runs are at year 15
avg2 <- cdist %>% filter(sim %in% avgruns$sim, year==15)
quantile(avg2$i.rc)

# get year 1-15, 16-30, and 1-30 ir.gm
avg3 <- cdist %>% filter(year %in% c(15, 30)) %>%
  select(sim, year, i.rc) %>%
  mutate(year=paste0("y", year)) %>%
  spread(year, i.rc) %>%
  mutate(y1530={((1+y30)^30) / ((1+y15)^15)} ^(1/15)-1) %>%
  select(sim, first15=y15, next15=y1530, all30=y30)

startlow <- sample(avg2$sim[avg2$i.rc<=.054], 1)

sims <- c(0, 56, 75, 798) # det, starthigh, startlow, endlow
snames <- c("constant", "starthigh", "startlow", "endlow")

sims <- c(0, 56, 75) # det, starthigh, startlow -- YY
# sims <- c(0, 479, 768) # det, starthigh, startlow -- DB
# replacing #75 with 228 or 535 and keeping # 56
sims <- c(0, 56, 228) # det, starthigh, startlow -- YY2
sims <- c(0, 56, 535) # det, starthigh, startlow -- YY3

starthigh <- sample(avg2$sim[avg2$i.rc>=.082], 1)
startlow <- sample(avg2$sim[avg2$i.rc<=.054], 1)
sims <- c(0, starthigh, startlow) # 479 768


sims <- c(0, 56, 75, 228, 535) # det, starthigh, startlow -- YY
snames <- c("Constant 7.5%\n", 
            paste0("Higher returns early\n(sim #", sims[2], ")\n"), 
            paste0("Higher returns late\n(sim #", sims[3], ")\n"),
            paste0("Higher returns late\n(sim #", sims[4], ")\n"),
            paste0("Higher returns late\n(sim #", sims[5], ")\n"))
run <- "A1F075_O30pA5"
# tmp %>% qplot(year, i.r, data=., colour=simf, geom=c("point", "line"))
# tmp %>% qplot(year, FR_MA, data=., colour=simf, geom=c("point", "line"))
# tmp %>% qplot(year, ERC_PR, data=., colour=simf, geom=c("point", "line"))
avg3 %>% filter(sim %in% sims) %>% kable(digits=4)

tmp <- df %>% filter(runname==run, sim %in% sims, year<=30) %>% mutate(simf=factor(sim, levels=sims, labels=snames))
tmp %>% filter(sim %in% sims, year==30) %>% select(sim, year, FR_MA)

# get pv of ERC as % of pv of PR
i <- .075
# i <- .04
v <- 1 / (1+i)
ercpv2 <- tmp %>% group_by(sim) %>%
  arrange(year) %>%
  select(year, runname, sim, i.r, ERC, PR, ERC_PR) %>%
  mutate(i.rc=cumprod(1 + i.r)^(1/year)-1,
         ERC.pv=ERC * (v^(year-1)),
         ERC.pvsum=cumsum(ERC.pv),
         PR.pv=PR * (v^(year-1)),
         PR.pvsum=cumsum(PR.pv),
         ERC_PR2.pvsum=ERC.pvsum / PR.pvsum * 100)
ercpv2 %>% filter(year==30) %>% select(year, runname, sim, i.rc, ERC_PR2.pvsum)



##  Data for Table 2

tab2_part1 <- 
df2 %>% filter(runname == "A1F075_O30pA5", sim %in% c(0, 56, 228), year %in% 1:30) %>%
  group_by(sim) %>% 
  summarize(geoReturnY1_15 = get_geoReturn(i.r[year %in% 1:15]),
            geoReturnY16_30 = get_geoReturn(i.r[year %in% 16:30]),
            geoReturnY11_30 = get_geoReturn(i.r),
            FR_Y30 = FR_MA[year == 30])

tab2_part2 <- 
ercpv2 %>% filter(sim %in% c(0, 56, 228), year == 30) %>% select(runname, year, sim, ERC_PR2.pvsum) %>% 
  ungroup %>% 
  mutate(ERC_PR2.pvsum_diff0 = 100 * (ERC_PR2.pvsum - ERC_PR2.pvsum[sim == 0])/ ERC_PR2.pvsum[sim == 0])

tab2 <- left_join(tab2_part1, tab2_part2) %>% 
  select(runname, sim, everything(), -year)
tab2

write.xlsx(tab2, file=paste0(outputs.dir, "Tables_raw.xlsx"), sheetName = "table2")


# now make presentation quality graphs of i.r (?), FR_MA, and ERC_PR

sims <- c(0, 56, 75, 228, 535) # det, starthigh, startlow -- YY
snames <- c("Constant 7.5%\n", 
            paste0("Higher returns early\n(sim #", sims[2], ")\n"), 
            paste0("Higher returns late\n(sim #", sims[3], ")\n"),
            paste0("Higher returns late\n(sim #", sims[4], ")\n"),
            paste0("Higher returns late\n(sim #", sims[5], ")\n"))

tmp <- df %>% filter(runname==run, sim %in% c(0, 56, 228), year<=30) %>% mutate(simf=factor(sim, levels=sims, labels=snames))

t1 <- "Employer contribution rate for 3 simulations each with"
t2 <- "compound annual return of 7.5% over 30 years"
t3 <- "(Initial funded ratio of 75%)"
gtitle <- paste0(t1, "\n", t2, "\n", t3)

p <- ggplot(data=tmp, aes(x=year, y=ERC_PR, group=simf)) +
  geom_line(aes(colour=simf, linetype=simf)) +
  geom_point(aes(colour=simf, shape=simf), size = 1.2) +
  geom_hline(yintercept=tmp$ERC_PR[tmp$year==1 & tmp$sim==0], linetype="dashed", colour="black") +
  labs(colour="", linetype="", shape="") +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) + 
  theme_bw() + RIG.theme() + 
  ggtitle(gtitle) + #theme(plot.title=element_text(size=12, face="bold")) +
  scale_x_continuous(breaks=seq(0, 50, 5), name="Year") +
  theme(axis.title.x=element_text(size=10, colour="black")) +
  scale_y_continuous(breaks=seq(0, 100, 2), name="Employer contribution rate (% of payroll)") +
  theme(axis.title.y=element_text(size=10, colour="black")) + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))  
p
ggsave(file=paste0(outputs.dir, "fig4_indivruns_ercpr.png"), p, width=10, height=6, units="in" )
ggsave(file=paste0(outputs.dir, "fig4_indivruns_ercpr.pdf"), p, width=10, height=6, units="in" )

fig4 <- p


t1 <- "Funded ratio for 3 simulations each with"
t2 <- "compound annual return of 7.5% over 30 years"
t3 <- "(Initial funded ratio of 75%)"
gtitle <- paste0(t1, "\n", t2, "\n", t3)
p <- ggplot(data=tmp, aes(x=year, y=FR_MA, group=simf)) +
  geom_line(aes(colour=simf, linetype=simf)) +
  geom_point(aes(colour=simf, shape=simf), size = 1.2) +
  geom_hline(yintercept=100) +
  geom_hline(yintercept=75, linetype="dashed", colour="black") +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) +
  labs(colour="", linetype="", shape="") +
  theme_bw() + RIG.theme() + 
  ggtitle(gtitle) + #theme(plot.title=element_text(size=12, face="bold")) +
  scale_x_continuous(breaks=seq(0, 50, 5), name="Year") +
  theme(axis.title.x=element_text(size=10, colour="black")) +
  scale_y_continuous(breaks=seq(0, 200, 10), name="Funded ratio (%)") +
  theme(axis.title.y=element_text(size=10, colour="black")) + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80")) 
p

fig5 <- p

ggsave(file=paste0(outputs.dir, "fig5_indivruns_fr.png"), p, width=10, height=6, units="in" )
ggsave(file=paste0(outputs.dir, "fig5_indivruns_fr.pdf"), p, width=10, height=6, units="in" )

fig4
fig5

fig5_4<- marrangeGrob(list(fig5, fig4), nrow=2, ncol=1, top=NULL)
fig5_4

ggsave(file=paste0(outputs.dir, "fig5_4_indivruns.png"), fig5_4, width=9, height=11, units="in" )
ggsave(file=paste0(outputs.dir, "fig5_4_indivruns.pdf"), fig5_4, width=9, height=11, units="in" )



## Figure 3 ####
## Annual returns and cumulatie returns for selected single runs

tmp <- cdist %>% filter(sim %in% c(56, 228), year <=30) %>% 
   gather(type, value, -runname, -year, -sim) %>% 
   ungroup() %>% 
   mutate(sim  = factor(sim, levels = c(56, 228), labels = paste0("Simulation #", c(56, 228))),
          type = factor(type, levels = c("i.r", "i.rc"), labels = c("Annual return", "Cumulative compound annual return")))



p <- ggplot(tmp, aes(x = year, y = value*100)) + theme_bw() + facet_grid(. ~ sim) + 
  geom_point(aes(colour=type), size=1.2) +
  geom_line(aes(colour=type)) + 
  geom_hline(yintercept = 7.5) +
  geom_hline(yintercept = 0, linetype="dashed", size=.5) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(breaks = c(seq(-50, 5, 5), 7.5, seq(10, 50, 5)), name = "Percent") + 
  scale_color_manual(values = c(RIG.red, RIG.green)) +
  
  labs(title=paste0("Annual returns and cumulative compound annual returns \nunder 2 single runs with 30-year compound annual returns close to 7.5%"),
       colour=NULL) +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80")) 
p

ggsave(file=paste0(outputs.dir, "fig3_indivruns_ir.png"), p, width=10, height=5.5, units="in" )
ggsave(file=paste0(outputs.dir, "fig3_indivruns_ir.pdf"), p, width=10, height=5.5, units="in" )



# Policy Brief figure 2 ERC increase > 10% prob ####
tmp <- probs %>%
  filter(runname %in% roi, !deterministic | (deterministic & runname=="A1F075_O30pA5" & runname!= "A1F075_soa3")) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname)),
         runname2=factor(runname, levels=roi.pb, labels=roi.labs.pb, ordered=TRUE),
         runname2lines=factor(runname, levels=roi.pb, labels=roi.labs.2lines.pb, ordered=TRUE))
xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle <- "Probability that employer contributions will rise\nby more than 10% of payroll in a 5-year period"
p <- ggplot(data=tmp %>% filter(year<=30), aes(x=year, y=ercprdiffgt10, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))

p
ggsave(paste0(outputs.dir,"pb.fig2_ercprdiffgt10.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir,"pb.fig2_ercprdiffgt10.pdf"), p, width=10, height=6, units="in")




# Policy Brief figure 3 Prob FR < 40% ####

tmp <- probs %>%
  filter(runname %in% roi, !deterministic | (deterministic & runname=="A1F075_O30pA5" & runname != "A1F075_soa3")) %>%
  ungroup %>%
  mutate(runname=ifelse(deterministic, "Deterministic", as.character(runname)),
         runname2=factor(runname, levels=roi.pb, labels=roi.labs.pb, ordered=TRUE),
         runname2lines=factor(runname, levels=roi.pb, labels=roi.labs.2lines.pb, ordered=TRUE))

xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle <- "Probability that the funded ratio will fall\nbelow 40% during the first 30 years"
p <- ggplot(data=tmp %>% filter(year<=30), aes(x=year, y=fr7p5lt40, colour=runname2lines)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 20, 2), limits=c(0, 18)) +
  scale_color_manual(values = c(RIG.red, RIG.yellow.dark, RIG.green, RIG.blue)) +
  ggtitle(gtitle) +
  theme(plot.title = element_text(size = 14)) +
  guides(colour=guide_legend(title="Funding scenario:"), size=guide_legend(title=NULL)) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
p
ggsave(paste0(outputs.dir,"pb.fig3_fr7p5lt40.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir,"pb.fig3_fr7p5lt40.pdf"), p, width=10, height=6, units="in")


# FR probabilities for selected funded-ratio thresholds
# fr7p5lt40, fr7p5lt50, fr7p5lt60, fr7p5lt70
xtitle <- "Year"
ytitle <- "Probability (%)"
gtitle1 <- "Probability that the funded ratio will fall below a given threshold during the first 30 years"
gtitle2 <- "under 30-year open level-percent funding, with 5-year asset smoothing"
gtitle <- paste0(gtitle1, "\n", gtitle2)
frcut <- c("fr7p5lt70", "fr7p5lt60", "fr7p5lt50", "fr7p5lt40")
frcut.labs <- c("70%", "60%", "50%", "40%")
tmp <- probs %>% filter(year<=30, runname=="A1F075_O30pA5", !deterministic) %>%
  ungroup %>%
  select(year, starts_with("fr7p")) %>% 
  gather(variable, value, -year) %>%
  mutate(varf=factor(variable, levels=frcut, labels=frcut.labs, ordered=TRUE))
p <- ggplot(data=filter(tmp, variable!="fr7p5lt70"), aes(x=year, y=value, colour=varf)) +
  theme_bw() +
  geom_line(size=1.2) +
  geom_point() +
  geom_hline(yintercept = 50, linetype="dashed") +
  scale_x_continuous(name=xtitle, breaks=seq(0, 100, 5), minor_breaks = seq(0, 100, 1)) +
  scale_y_continuous(name=ytitle, breaks=seq(0, 100, 5)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) +
  ggtitle(gtitle) +
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80")) +
  guides(colour=guide_legend(title="Funding threshold"), size=guide_legend(title=NULL))
p


ggsave(paste0(outputs.dir, "fig11_frthresholds.png"), p, width=10, height=6, units="in")
ggsave(paste0(outputs.dir, "fig11_frthresholds.pdf"), p, width=10, height=6, units="in")










