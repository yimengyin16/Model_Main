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


# Graphs for Contributrion rate and funded status. 


expand_apply <- function(x, fun, ...){
  # Applying the given function to expanding windows of vector x.
  zoo::rollapply(x, seq_along(x), fun, ...,  fill = NA, align = "right")
}


gpanel.yy <- function(simnum, runs = runs_single, det.runs = "D1F075_average", label = runs_single_labels){
  
#   runs_single <- c("D1F075-average", "D1F075_mature1-gn1", "D1F075_mature2-gn1", "D1F075-immature_g1")
#   simnum <- 56
#   det.runs = "D1F075_average"
#    label <- runs_single_labels
  
  df_single <- results_all %>%  filter(runname %in% runs_single, year <= 30) %>% 
    filter(sim == simnum) %>%
    left_join(label) %>% 
    mutate(run.label = ifelse(is.na(run.label), runname, run.label),
           runname   = ifelse(sim == 0, paste0(runname,   "-Determ"), runname),
           run.label = ifelse(sim == 0, paste0(run.label, "-Determ"), run.label)) %>%  
    group_by(runname, sim) %>% 
    mutate(ir.gm   = expand_apply(i.r, get_geoReturn)) 

  
  gmval <- df_single %>% filter(year == 30, sim == simnum, runname == runs_single[1]) %>% ungroup %>% select(ir.gm) %>% unlist
  gmval <- 100*round(gmval, 3)
  
  ls <- 1.25 # linesize
  ps <- 2.25 # pointsize
  
  p1 <- ggplot(data=filter(df_single, sim == simnum, runname == runs_single[1]) %>% # any run will do
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
  
  ggsave(paste0(IO_folder, "/Single_runs/g_", simnum, ".png"), ml, width=13*1.2, height=8*1.2, units="in")     
  
  invisible(ml)
}


runs_single <- c("D1F075-average", "D1F075-mature1_gn1", "D1F075-mature2_gn1", "D1F075-immature_g1")

runs_single_labels <- c('runname,                run.label, key.feature
                         D1F075-average,          Average,    Average
                         D1F075-mature1_gn1,      Mature1,    Mature1                       
                         D1F075-mature2_gn1,      Mature2,    Mature2
                         D1F075-immature_g1,      Immature,   Immature')

runs_single_labels <- read.table(text = runs_single_labels, header = TRUE, sep = ",", stringsAsFactors = F) %>% 
  mutate_each(funs(str_trim))


apply(as.matrix(c(56, 228)), 1, gpanel.yy)
apply(as.matrix(c(75, 138,254, 934, 424, 646)), 1, gpanel.yy)


