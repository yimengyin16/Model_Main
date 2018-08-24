# Code snippet for Matt C.
# Yimeng Yin
# 7/13/2018

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

runs_list <- c("D1F075-average", "D1F075-mature1_gn1", 
               "D1F075-mature2_gn1", "D1F075-immature_g1")
# Add runnames you want to examine


## Outputs of pension finance  
get_results <- function(IO_folder, runs_list){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- paste0("Outputs_", runs_list, ".RData")
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}


results_all <- get_results(IO_folder, runs_list)


results_all %>% 
  filter(sim == -1, 
         year <= 60) %>%  
    # sim -1: This is for consistency check. Plan starts 100% funded and actual return is deterministic and equals assumed return.
    #         Funded ratio should always be 100% 
    # sim 0:  Deterministic run, actual return is constant and equals the long-term geometric mean return implied by the expected return and standard deviation. 
    # sim 1 to nsim: stochastic runs, returns are generated from a normal distribution with the mean and SD given in the runControl file (ir.mean and ir.sd, or the "Returns" tab)
  select(runname, sim, year, AL,MA, AA, FR, NC, SC, ADC, ERC, EEC, C, B, PR)


# NC: Normal Cost  
# SC: Supplement cost (amortization cost)
# ADC: Actuarilly determined contribution (NC + SC)

# C : Actual contribution (may differ from ADC, for example, when ERC cap is set), C = ERC + EEC
# ERC: employer contribution
# EEC: employee contribution


# AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
# MA: Market value of assets.
# AA: Actuarial value of assets.
# UAAL: Unfunded accrued actuarial liability, defined as AL - NC
# PR: payroll 
# LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
# i is assumed interest rate. AMs of each period will be amortized seperately.  

# B : Total benefit payment(currently all to retirees)
# B.v: benefit payments for deferred retirement benefits (for those who quit their jobs but with vested retirement benefits)
# Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
# Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
# Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
# I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
# Funded Ratio: AA / AL
# C_PR: contribution as % of payroll










