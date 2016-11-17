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
library(xlsx)
library(pdata)

source("Functions.R")
source("Functions_Measures.R")


#*****************************************************
##  Defining paths for inputs and outputs ####
#*****************************************************
IO_folder       <- "IO_M2.1_new"
outputs.folder  <- "/M2.1a_outputs/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Loading and pre-processing simulation outputs
 # This section will produce two .RData file under the folder "Analysis_Demo"
   # 1. Demo_results_all.RData. Data frame "results_all" that contains results for pension finances. 
   # 2. DemoSum_all.RData".     Various data frames that contains demographic data. 

## Outputs of pension finance  
 get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}
 # results_all <- get_results(IO_folder, "^Outputs_D") # This will take a while because of the large size of output files that contain demographic data
 # save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))

## Outputs of demographics
#source("IO_M2.1_new/Report_M2.1a_LoadingDemoData.R")


## Loading existing data. 
load(paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))
load(paste0(IO_folder, "/Analysis_Demo/DemoSum_all.RData"))


#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)

demo.color5 <- c(RIG.red,    # average -1%
                 RIG.orange, # average 0%
                 RIG.yellow.dark, # average +1% 
                 RIG.blue,   # mature plan
                 RIG.green   # immature plan
                 )
demo.shape5 <- c(16, 16, 16, 15, 17) # 16-average, 15-mature, 17-immature 


RIG.theme <- function(){
             theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
}


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

runs_demo.all <- c("D1F075-average_gn2",
                   "D1F075-average_gn1",
                   "D1F075-average",
                   "D1F075-average_g1",
                   "D1F075-average_g2",
                   "D1F075-mature1_gn1",
                   "D1F075-mature2_gn1",
                   "D1F075-immature_g1")

runs_demo.all_labels <- c("Average, 2% decline",
                          "Average, 1% decline",
                          "Average, constant workforce",
                          "Average, 1% growth",
                          "Average, 2% growth",
                          "Mature plan (high asset-payroll ratio)",
                          "Mature plan 2 (high normal cost)",
                          "Immature plan (low asset-payroll ratio)" )


runs_demo <- setdiff(runs_demo.all, c("D1F075-average_gn1",
                                      "D1F075-average_g1", 
                                      "D1F075-mature2_gn1"))
runs_demo_labels <- setdiff(runs_demo.all_labels, c("Average, 1% decline", 
                                                    "Average, 1% growth", 
                                                    "Mature plan 2 (high normal cost)"))

runs_demo2 <- setdiff(runs_demo.all, c("D1F075-average_gn1",
                                       "D1F075-average_g1", 
                                       "D1F075-average_gn2",
                                       "D1F075-average_g2", 
                                       "D1F075-mature2_gn1"))
runs_demo2_labels <- setdiff(runs_demo.all_labels, c("Average, 1% decline", 
                                                     "Average, 1% growth", 
                                                     "Average, 2% decline", 
                                                     "Average, 2% growth",
                                                     "Mature plan 2 (high normal cost)"))


df_demo.all <- results_all  %>% 
           filter(runname %in% runs_demo.all, sim >= 0, year <= 30)

df_demo.all %<>%   
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
                          levels = runs_demo.all, 
                          labels = runs_demo.all_labels))

df_demo <- df_demo.all %>% filter(runname %in% runs_demo_labels)

df_demo %>% filter(year == 1)

#*****************************************************
## Figure 5-7: risk measures  ####
#*****************************************************
# This section produces Figure 5-7


## Figure 5 The mature plans and the plan with a 2 percent annual workforce decline have 
   # the greatest risk of a high employer contribution rate
fig.title <- "Probability that employer contribution exceeds 30% of payroll \nin any year up to the given year "
fig.ERC_high <- 
  ggplot(df_demo, aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 50)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,100, 10)) + 
  scale_color_manual(values = demo.color5) +
  scale_shape_manual(values = demo.shape5, name = NULL ) +
  labs(x = "Year",
       y = "Probability that employer contribution exceeds 30%",
       title = fig.title ) + 
  guides(col   = guide_legend(title = "Plan type"),
         shape = guide_legend(title = "Plan type")) 
# theme(#legend.justification=c(0,1), legend.position=c(0,1),
#       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig.ERC_high


grid.newpage()
note <- 'Note: The lines for "Average, 2% growth" and "Immature plan" are overlapped.'
fig.ERC_high <- arrangeGrob(fig.ERC_high, bottom = textGrob(note, x = 0, hjust = -0.1, vjust=0.1, 
                                                                            gp = gpar(fontface = "plain", fontsize = 9)))
grid.draw(fig.ERC_high)


## Figure 6 Plans with high initial asset-payroll ratios and plans with a declining workforces have 
   #greatest risk of severe underfunding
fig.title <- "Probability that funded ratio falls below 40% \nin any year up to the given year"
fig.FR40less <- 
  ggplot(df_demo, aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 32)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  scale_color_manual(values = demo.color5) +
  scale_shape_manual(values = demo.shape5, name = NULL ) +
  labs(x = "Year",
       y = "Probability of falling below 40%",
       title = fig.title) + 
  guides(col = guide_legend(title = "Plan type"),
         shape = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig.FR40less



## Figure 7 Funded ratios of plans with high initial asset-payroll ratio and plans with declining workforce recover slower
fig.title <- "Median funded ratio"
fig.FR.med <- 
  ggplot(df_demo, aes(x = year, y = FR.q50, color = runname, shape = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  geom_hline(yintercept = 100, linetype = 2, color = "black") + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,200, 25)) + 
  scale_color_manual(values = demo.color5) +
  scale_shape_manual(values = demo.shape5, name = NULL ) +
  labs(x = "Year",
       y = "Funded ratio (%)",
       title = "Median funded ratio") + 
  guides(col = guide_legend(title = "Plan type"),
         shape = guide_legend(title = "Plan type")) 
  # theme(# legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig.FR.med


## Figure 7(rescale) Funded ratios of plans with high initial asset-payroll ratio and plans with declining workforce recover slower
fig.FR.med_rescale <- fig.FR.med + 
  coord_cartesian(ylim = c(50, 100)) + 
  scale_y_continuous(breaks = seq(0,200, 10))
fig.FR.med_rescale




## Figures of risk measures not shown in the report

## Median ERC
fig.title <- "Median employer contribution as a percentage of payroll"
fig.ERC_PR.med <- 
  ggplot(df_demo, aes(x = year, y = ERC_PR.q50, color = runname, shape = runname)) + theme_bw() + RIG.theme() +
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 25)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,60, 5)) + 
  scale_color_manual(values = demo.color5) +
  scale_shape_manual(values = demo.shape5, name = NULL ) +
  labs(x = "Year",
       y = "Employer contribution rate (%)",
       title = fig.title) + 
  guides(col = guide_legend(title = "Plan type"),
         shape = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig.ERC_PR.med

## ERC rising sharply in a short time period
fig.title <- "Probability of employer contribution rising by more than \n10% of payroll in any 5-year period year up to the given year "
fig.ERC_hike <- 
  ggplot(df_demo, aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + RIG.theme() + 
  geom_point(size = 2) + geom_line() +
  coord_cartesian(ylim = c(0, 60)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,100, 10)) + 
  scale_color_manual(values = demo.color5) +
  scale_shape_manual(values = demo.shape5, name = NULL ) +
  labs(x = "Year",
       y = "Probability (%)",
       title = fig.title ) + 
  guides(col = guide_legend(title = "Plan type"),
         shape = guide_legend(title = "Plan type")) 
  # theme(#legend.justification=c(0,1), legend.position=c(0,1),
  #       legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
fig.ERC_hike




#*****************************************************
## Figure 4  ####
#*****************************************************

fig.title <- c('Accrued liability as a percentage of salary \nby age and entry age \n for workers in an "average plan"')
fig.AL_ea <- 
df_AL_sx_all %>% filter(runname == "D1F075-average", ea %in% seq(0,65, 10)) %>% 
  ggplot(aes(x = age, y = AL_sx ,color = factor(ea), shape = factor(ea))) + theme_bw() + RIG.theme() + 
  geom_line() + geom_point(size = 2) + 
  scale_color_manual(values = demo.color6[-2]) + 
  scale_x_continuous(breaks = seq(20, 60, 10)) + 
  labs(x = "Age",
       y = "Accrued liability as % of salary",
       title = fig.title) + 
  guides(col   = guide_legend(title = "Entry age"),
         shape = guide_legend(title = "Entry age")) 
fig.AL_ea



#*****************************************************
## Table 1  ####
#*****************************************************

# Variables of demographics
tab1_demo_summary <- 
df_demo_summary_all %>% select(runname, year, ends_with("avg"), tot_actives, tot_retirees, tot_termsBen) %>% 
  filter(year %in% c(1, 30), runname %in% runs_demo.all) %>% 
  mutate(abratio = tot_actives/(tot_retirees + tot_termsBen)) %>% 
  select(runname, year, actives_age.avg, retirees_age.avg, abratio)

# Variables of plan finance
tab1_finance_summary <- 
  results_all %>% select(runname, year, NC_PR, MA_PR, ExF_MA) %>% 
  filter(year %in% c(1, 30), runname %in% runs_demo.all) %>% 
  group_by(runname, year) %>% 
  summarise_each(funs(median)) %>% 
  mutate(MA_PR = MA_PR/100) 

# All variables for table 1

tab1 <- tab1_demo_summary %>% 
  left_join(tab1_finance_summary) 
  
tab1 %<>% gather(variable, value, -year, -runname) %>% 
  mutate(var_year = paste0(variable,"_year", year)) %>%
  select(-year, -variable) %>% 
  spread(var_year, value) %>% 
  select(runname, actives_age.avg_year1, actives_age.avg_year30,
                  retirees_age.avg_year1, retirees_age.avg_year30,
                  abratio_year1, abratio_year30,
                  NC_PR_year1, NC_PR_year30,
                  MA_PR_year1, MA_PR_year30,
                  ExF_MA_year1, ExF_MA_year30) %>% 
  left_join(data.frame(runname = runs_demo.all, order = seq_along(runs_demo.all))) %>% 
  arrange(order) %>% 
  select(-order)


ppd %>% names



#*****************************************************
## Table 2  ####
#*****************************************************

tab2 <- tab1 %>% select(runname, abratio_year1, NC_PR_year1, MA_PR_year1, MA_PR_year30, ExF_MA_year1, ExF_MA_year30) %>% 
  left_join(df_demo.all %>% filter(year == 30) %>% select(runname, FR40less, ERC_high, ERC_hike)%>% mutate(runname = factor(runname, labels = runs_demo.all)))


#*****************************************************
## Figures in Appendices  ####
#*****************************************************

runName_list <- c("D1F075-average",
                  "D1F075-mature1_gn1", 
                  # "D1F075-mature2_gn1", 
                  "D1F075-immature_g1")


# Appendix Figure 1
fig.title = 'Normal cost under Entry Age Normal method \nas a percentage of salary by entry age \nfor workers in an "average plan"'
fig.NC_sx <- 
df_NC_sx_all %>% filter(runname == "D1F075-average") %>% 
  ggplot(aes(x = ea, y = NC.av_sx)) + geom_bar(stat = "identity", color = "black", fill = RIG.blue) + theme_bw() + RIG.theme() + 
  coord_cartesian(xlim = c(20, 65), ylim = c(0,20)) + 
  labs(x = "Entry age",
       y = "Normal cost as % of salary",
       title = fig.title)
  
  
# Appendix Figure 4
fig.title <-  "Age Distribution of Actives"
fig.ageDist <- 
df_ageDist_all %>% filter(runname %in% runs_demo2, year == 1 , age <= 74) %>% 
  mutate(runname = factor(runname, levels = runs_demo2, label = runs_demo2_labels)) %>% 
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity", fill = RIG.blue) + theme_bw() + RIG.theme() +  
  scale_x_continuous(breaks = seq(20, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 0.5)) + 
  labs(x = "Age",
       y = "percent",
       title = fig.title) + 
  facet_grid(.~runname)
fig.ageDist

fig.title <- "Entry Age Distribution of Actives"
fig.eaDist <- 
df_eaDist_all %>% filter(runname %in% runs_demo2, year %in% c(1) , age <= 74) %>% 
  mutate(runname = factor(runname, levels = runs_demo2, label = runs_demo2_labels)) %>%
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity", fill = RIG.blue) + theme_bw() + RIG.theme() +  
  scale_x_continuous(breaks = seq(20, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 0.5)) + 
  labs(x = "Entry age",
       y = "percent",
       title = fig.title) + 
  facet_grid(.~runname)
fig.eaDist

# Appendix Figure 5
fig.title <- "Year of Service Distribution of Actives"
fig.yosDist <- 
df_yosDist_all %>% filter(runname %in% runs_demo2, year %in% c(1), yos <= 55) %>% 
  mutate(runname = factor(runname, levels = runs_demo2, label = runs_demo2_labels)) %>%
  ggplot(aes(x = yos,  y = pct_age)) + geom_bar(stat = "identity", fill = RIG.blue) + theme_bw() + RIG.theme() + 
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  labs(x = "Number of years",
       y = "percent",
       title = fig.title) + 
  facet_grid(.~runname)
fig.yosDist

fig.title <- "Age Distribution of New Entrants"
names(df_entDist_all)[3] <-  "pct_age"
fig.entDist <- 
df_entDist_all %>% filter(runname %in% runs_demo2) %>% 
  mutate(runname = factor(runname, levels = runs_demo2, label = runs_demo2_labels)) %>%
  ggplot(aes(x = age,  y = pct_age)) + geom_bar(stat = "identity", fill = RIG.blue) + theme_bw() + RIG.theme() +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  labs(x = "Age",
       y = "percent",
       title = fig.title) + 
  facet_grid(.~runname)
fig.entDist


# Appendix Figure 2 and Figure 3

expand_apply <- function(x, fun, ...){
  # Applying the given function to expanding windows of vector x.
  zoo::rollapply(x, seq_along(x), fun, ...,  fill = NA, align = "right")
}

gpanel <- function(simnum, runs = runs_single, det.runs = "D1F075_average", label = runs_single_labels){
  
    # runs <- c("D1F075-average", "D1F075-mature1_gn1", "D1F075-immature_g1")
    # simnum <- 56
    # det.runs = "D1F075_average"
    # label <- runs_single_labels
  
  df_single <- results_all %>%  filter(runname %in% runs, year <= 30) %>% 
    filter(sim == simnum) %>%
    left_join(label) %>% 
    mutate(run.label = ifelse(is.na(run.label), runname, run.label),
           runname   = ifelse(sim == 0, paste0(runname,   "-Determ"), runname),
           run.label = ifelse(sim == 0, paste0(run.label, "-Determ"), run.label)) %>%  
    group_by(runname, sim) %>% 
    mutate(ir.gm   = expand_apply(i.r, get_geoReturn),
           run.label = factor(run.label, levels = label[["run.label"]])) 
  
  
  gmval <- df_single %>% filter(year == 30, sim == simnum, runname == runs[1]) %>% ungroup %>% select(ir.gm) %>% unlist
  gmval <- 100*round(gmval, 3)
  
  ls <- 1.25 # linesize
  ps <- 2.25 # pointsize
  
  p1 <- ggplot(data=filter(df_single, sim == simnum, runname == runs[1] ) %>% # any run will do
                 select(year, i.r, ir.gm) %>% 
                 gather(variable, value, -year, -runname, -sim),
               aes(x = year, y = value*100, group = variable)) +
    geom_point(aes(colour=variable, shape=variable), size=ps) +
    geom_line(aes(colour=variable), size=ls) +
    scale_color_discrete(name = "",    labels = c("Annual return", "Cumulative Geom \nMean Return")) +
    scale_shape_discrete(name = "",    labels = c("Annual return", "Cumulative Geom \nMean Return")) +
    scale_linetype_discrete(name = "", labels = c("Annual return", "Cumulative Geom \nMean Return")) +
    scale_y_continuous(breaks = c(seq(-50, 5, 5), 7.5, seq(10, 50, 5)), name = "Percent") + 
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    geom_hline(yintercept = 7.5) +
    geom_hline(yintercept = 0, linetype="dotdash", size=.5) +
    labs(title=paste0("Annual returns and cumulative geometric mean: sim # ", simnum , "\n30-year geometric mean=", gmval ), x = "Year") +
    theme(plot.title=element_text(size=14), legend.text = element_text(size = 11))  + theme_bw()
  
  p1
  
  p2 <- ggplot(data = df_single,
               aes(x = year, y = ERC_PR, group = run.label)) +
    geom_point(aes(colour = run.label, shape = run.label), size = ps) +
    geom_line(aes(colour  = run.label), size=ls) +
    scale_color_discrete(name = "" ) +
    scale_shape_discrete(name = "") +
    scale_linetype_discrete(name = "") +
    scale_y_continuous(breaks = seq(-10, 100, 5), name = "Percent") + 
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    # geom_hline(y = ncpct$mdn) +
    labs(title=paste0("Employer Contributions as % of payroll: sim # ", simnum), x = "Year") +
    theme(plot.title=element_text(size=14), legend.text = element_text(size = 11)) + theme_bw() + 
    guides(colour   = guide_legend(label.position = "right", keyheight = 2),
           shape    = guide_legend(label.position = "right", keyheight = 2),
           linetype = guide_legend(label.position = "right", keyheight = 2))
  
  p2
  
  p3 <- ggplot(data= df_single, # use year 31 to get opening balance
               aes(x = year, y = FR_MA, group = run.label)) +
    geom_point(aes(colour=run.label, shape=run.label), size=ps) +
    geom_line(aes(colour=run.label), size=ls) +
    scale_color_discrete(name = "") +
    scale_shape_discrete(name = "") +
    scale_linetype_discrete(name = "") +
    scale_y_continuous(breaks=seq(30, 300, 10), name="Funded ratio at beginning of year") + 
    scale_x_continuous(breaks=seq(0, 30, 5)) +
    geom_hline(yintercept = 100) +
    labs(title=paste0("Funded ratio (based on MV assets): sim # ", simnum), x = "Year") +
    theme(plot.title=element_text(size=14), legend.text = element_text(size = 11)) + theme_bw() + 
    guides(colour   = guide_legend(label.position = "right", keyheight = 2),
           shape    = guide_legend(label.position = "right", keyheight = 2),
           linetype = guide_legend(label.position = "right", keyheight = 2))
  
  
  p3
  
  
  p4 <- ggplot(data= df_single, # use year 31 to get opening balance
               aes(x = year, y = ExF_MA, group = run.label)) +
    geom_point(aes(colour=run.label, shape=run.label), size=ps) +
    geom_line(aes(colour=run.label), size=ls) +
    scale_color_discrete(name = "") +
    scale_shape_discrete(name = "") +
    scale_linetype_discrete(name = "") +
    scale_y_continuous(breaks=seq(-10, 10, 1), name="Percent") + 
    scale_x_continuous(breaks=seq(0, 30, 5)) +
    labs(title=paste0("Net cash flow before investment income as % of assets: sim # ", simnum), x = "Year") +
    theme(plot.title=element_text(size=14), legend.text = element_text(size = 11)) + theme_bw() + 
    guides(colour   = guide_legend(label.position = "right", keyheight = 2),
           shape    = guide_legend(label.position = "right", keyheight = 2),
           linetype = guide_legend(label.position = "right", keyheight = 2))
  
  
  
  #  p4
  
  # Add a table
  tbl <- df_single %>%
    group_by(run.label) %>%
    arrange(year) %>%
    mutate(Contrib.5yrchg=C_PR - lag(C_PR, 5),
           FR.5yrchg=FR_MA - lag(FR_MA, 5)) %>%
    summarise(Contrib.sd=round(sd(C_PR, na.rm=TRUE), 1),
              FR.sd=round(sd(FR_MA, na.rm=TRUE), 1),
              Contrib.max5yr_rise=round(max(Contrib.5yrchg, na.rm=TRUE), 1),
              FR.max5yr_fall=round(-min(FR.5yrchg, na.rm=TRUE), 1),
              Contrib.max5yr_rise=ifelse(Contrib.max5yr_rise>=0, Contrib.max5yr_rise, NA),
              FR.max5yr_fall=ifelse(FR.max5yr_fall>=0, FR.max5yr_fall, NA)) %>%
    mutate(Run = run.label)  %>%
    ungroup %>%
    #arrange(runf2) %>%
    select(Run, Contrib.sd, FR.sd, Contrib.max5yr_rise, FR.max5yr_fall) %>%
    as.data.frame
  tbl
  colnames <- str_replace(names(tbl), "\\.", "\n")
  grob <- tableGrob(tbl, rows=NULL, cols=colnames)
  
  #   ml <- marrangeGrob(list(p1, p2, p3, p4, grob), 
  #                      layout_matrix = rbind(c(1,2),
  #                                            c(3,4),
  #                                            c(5,5)),
  #                      
  #                      nrow=3, ncol=2, top = NULL)
  
  ml <- marrangeGrob(list(p1, p2, p3, p4), 
                     layout_matrix = rbind(c(1,2),
                                           c(3,4)),
                     
                     nrow=3, ncol=2, top = NULL)
  
  
  # ml
  
  #ggsave(paste0(IO_folder, "/Single_runs/g_", simnum, ".png"), ml, width=13*1.2, height=8*1.2, units="in")     
  
  invisible(ml)
}

# runs_single <- c("D1F075-average", "D1F075-mature1_gn1", "D1F075-mature2_gn1", "D1F075-immature_g1")
# runs_single_labels <- c('runname,                run.label, key.feature
#                          D1F075-average,          Average,    Average
#                          D1F075-mature1_gn1,      Mature1,    Mature1                       
#                          D1F075-mature2_gn1,      Mature2,    Mature2
#                          D1F075-immature_g1,      Immature,   Immature')

runs_single <- c("D1F075-average", "D1F075-mature1_gn1", "D1F075-immature_g1")
runs_single_labels <- c('runname,                run.label, key.feature
                         D1F075-average,          Average,    Average
                         D1F075-mature1_gn1,      Mature,    Mature
                         D1F075-immature_g1,      Immature,   Immature')



runs_single_labels <- read.table(text = runs_single_labels, header = TRUE, sep = ",", stringsAsFactors = F) %>% 
  mutate_each(funs(str_trim))


fig.singleRun56 <- gpanel(56)
fig.singleRun228 <- gpanel(228)

# fig.singleRun56
# fig.singleRun228




#*****************************************************
## Saving outputs  ####
#*****************************************************

fig.width = 9
fig.height = 5.5

# Fig 4
ggsave(paste0(IO_folder, outputs.folder, "fig4_AL_ea.png"),fig.AL_ea, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig4_AL_ea.pdf"),fig.AL_ea, width=fig.width, height=fig.height, units="in")

# Fig 5
ggsave(paste0(IO_folder, outputs.folder, "fig5_ERC_high.png"),fig.ERC_high, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig5_ERC_high.pdf"),fig.ERC_high, width=fig.width, height=fig.height, units="in")

# Fig 6
ggsave(paste0(IO_folder, outputs.folder, "fig6_ERC_hike.png"),fig.ERC_hike, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig6_ERC_hike.pdf"),fig.ERC_hike, width=fig.width, height=fig.height, units="in")


# Fig 7
ggsave(paste0(IO_folder, outputs.folder, "fig7_FR40less.png"),fig.FR40less, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig7_FR40less.pdf"),fig.FR40less, width=fig.width, height=fig.height, units="in")


# Fig 8
ggsave(paste0(IO_folder, outputs.folder, "fig8_FR.med.png"),fig.FR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig8_FR.med.pdf"),fig.FR.med, width=fig.width, height=fig.height, units="in")

# Fig 8 rescaled
ggsave(paste0(IO_folder, outputs.folder, "fig8_FR.med_rescaled.png"),fig.FR.med_rescale, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "fig8_FR.med_rescaled.pdf"),fig.FR.med_rescale, width=fig.width, height=fig.height, units="in")


# Appendix Fig 1
ggsave(paste0(IO_folder, outputs.folder, "figA1_NC_sx.png"),fig.NC_sx, width=fig.width, height=fig.height*1.2, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA1_NC_sx.pdf"),fig.NC_sx, width=fig.width, height=fig.height*1.2, units="in")


# Appendix Fig 2
ggsave(paste0(IO_folder, outputs.folder, "figA2_singleRun56.png"),fig.singleRun56, width=15, height=9, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA2_singleRun56.pdf"),fig.singleRun56, width=15, height=9, units="in")

# Appendix Fig 3
ggsave(paste0(IO_folder, outputs.folder, "figA3_singleRun228.png"),fig.singleRun228, width=15, height=9, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA3_singleRun228.pdf"),fig.singleRun228, width=15, height=9, units="in")


# Appendix Fig 4
ggsave(paste0(IO_folder, outputs.folder, "figA4a_ageDist.png"),fig.ageDist, width=15, height=4.5, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA4a_ageDist.pdf"),fig.ageDist, width=15, height=4.5, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figA4b_eaDist.png"),fig.eaDist, width=15, height=4.5, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA4b_eaDist.pdf"),fig.eaDist, width=15, height=4.5, units="in")


# Appendix Fig 5
ggsave(paste0(IO_folder, outputs.folder, "figA5a_yosDist.png"),fig.yosDist, width=15, height=4.5, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA5a_yosDist.pdf"),fig.yosDist, width=15, height=4.5, units="in")

ggsave(paste0(IO_folder, outputs.folder, "figA5b_entDist.png"),fig.entDist, width=15, height=4.5, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figA5b_entDist.pdf"),fig.entDist, width=15, height=4.5, units="in")


# Risk measure figures not shown in the report

ggsave(paste0(IO_folder, outputs.folder, "figX2_ERC_PR.med.png"),fig.ERC_PR.med, width=fig.width, height=fig.height, units="in")
ggsave(paste0(IO_folder, outputs.folder, "figX2_ERC_PR.med.pdf"),fig.ERC_PR.med, width=fig.width, height=fig.height, units="in")


# Table 1 and Table 2 
# Requires package "xlsx"
write.xlsx(tab1, file = paste0(IO_folder, outputs.folder, "Tables_raw.xlsx"), sheetName = "table1")
write.xlsx(tab2, file = paste0(IO_folder, outputs.folder, "Tables_raw.xlsx"), sheetName = "table2", append = TRUE)






