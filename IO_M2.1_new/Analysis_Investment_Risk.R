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

IO_folder <- "IO_M2.1_new/"


#****************************************************************************************************
#               ## 1. Loading Data ####
#****************************************************************************************************

load(file = paste0(IO_folder, "/Analysis_Investment/InvBen_results.RData"))
load(file = paste0(IO_folder, "/Analysis_Investment/InvBen_metrics.RData"))



#****************************************************************************************************
#              ## Tables of Measures of FR risk and contribution volatility for the report   ####
#****************************************************************************************************
df_metrics_Inv_y30 %>% colnames
df_metrics_Inv_y30 %<>% select(runname, FR40_y30, FR40_7.5_y30,
                                        PV.ERC_PR_med,    PV.ERC_PR_q75,    PV.ERC_PR_q90,
                                        PV.ERC_PR_L10_med, PV.ERC_PR_L10_q75, PV.ERC_PR_L10_q90,
                                        ERC_PR.max_med,   ERC_PR.max_q75,   ERC_PR.max_q90,
                                        FR_MA.min_med,    FR_MA.min_q25,    FR_MA.min_q10,
                                        FR_MA_7.5.min_med, FR_MA_7.5.min_q25, FR_MA_7.5.min_q10,   
                                        ERC_PR.final_med,     ERC_PR.final_q75, ERC_PR.final_q90,
                                        FR_MA.final_med,  FR_MA.final_q25,  FR_MA.final_q10,
                                        FR_MA_7.5.final_med,  FR_MA_7.5.final_q25,  FR_MA_7.5.final_q10,
                                        ERC_PR.pctChg_med,ERC_PR.pctChg_q75,ERC_PR.pctChg_q90,
                                        FR_MA_7.5.y1,   FR_MA.y1,  ERC_PR.y1
                               )

df_maxChg_Inv_y30  %<>% select(runname, ERC_PR.5yMaxChg_med,  ERC_PR.5yMaxChg_q75,  ERC_PR.5yMaxChg_q90,
                                        ERC_PR.10yMaxChg_med, ERC_PR.10yMaxChg_q75, ERC_PR.10yMaxChg_q90)
df_metrics_Inv_y30 <-  left_join(df_metrics_Inv_y30, df_maxChg_Inv_y30)

tab.runname <- df_metrics_Inv_y30$runname


tab.risk <- df_metrics_Inv_y30 %>%  
            left_join(results_all %>% select(runname,sim, year, NC_PR, MA_PR, ExF_MA, MA_B) %>% 
                          filter(runname %in% tab.runname, year %in% c(1, 30)) %>% 
                          group_by(runname, year) %>% 
                          summarise(NC_PR  = median(NC_PR),
                                    MA_PR_med  = median(MA_PR)/100,
                                    ExF_MA_med = median(ExF_MA)/100,
                                    MA_B_med   = median(MA_B)/100,
                                    
                                    MA_PR_q25  = quantile(MA_PR, 0.25)/100,
                                    ExF_MA_q25 = quantile(ExF_MA, 0.25)/100,
                                    MA_B_q25   = quantile(MA_B, 0.25)/100,
                                    
                                    MA_PR_q10  = quantile(MA_PR, 0.1)/100,
                                    ExF_MA_q10 = quantile(ExF_MA, 0.1)/100,
                                    MA_B_q10   = quantile(MA_B, 0.1)/100) %>% 
                          gather(variable, value, - runname, -year) %>% 
                          mutate(variable = paste0(variable,"_y",year), year = NULL) %>%
                          spread(variable, value))
tab.risk

# tab.risk %<>%  left_join(df_demo_summary_all %>% select(runname, year, ab.ratio) %>% 
#                           filter(runname %in% runname_risk, year %in% c(1)))


tab.risk %<>% mutate(runname = factor(runname, levels = tab.runname)) %>% 
  arrange(runname) %>%
  select(runname, 
                  # Cash flow
                  ExF_MA_med_y1,       ExF_MA_med_y30,      ExF_MA_q25_y30, ExF_MA_q10_y30, 
                  
                  # Costs
                  NC_PR_y1,
                  PV.ERC_PR_med, PV.ERC_PR_q75, PV.ERC_PR_q90,
                  PV.ERC_PR_L10_med, PV.ERC_PR_L10_q75, PV.ERC_PR_L10_q90,
                  ERC_PR.final_med,  ERC_PR.final_q75,  ERC_PR.final_q90, 
         
                  # Funded status
                  MA_PR_med_y1,        MA_PR_med_y30,       MA_PR_q25_y30,  MA_PR_q10_y30,
                  MA_B_med_y1,         MA_B_med_y30,        MA_B_q25_y30, MA_B_q10_y30, 
                  FR40_y30, FR40_7.5_y30,           
                  
                  FR_MA_7.5.y1,
                  FR_MA_7.5.min_med, FR_MA_7.5.min_q25, FR_MA_7.5.min_q10,
                  FR_MA_7.5.final_med,  FR_MA_7.5.final_q25,  FR_MA_7.5.final_q10,           
         
                  FR_MA.y1, 
                  FR_MA.min_med,    FR_MA.min_q25,    FR_MA.min_q10,
                  FR_MA.final_med,  FR_MA.final_q25,  FR_MA.final_q10, 
                
                  # Contribution volatility
                  ERC_PR.5yMaxChg_med, ERC_PR.5yMaxChg_q75, ERC_PR.5yMaxChg_q90,  
                  ERC_PR.10yMaxChg_med, ERC_PR.10yMaxChg_q75, ERC_PR.10yMaxChg_q90,                   
                            
                  ERC_PR.y1,
                  ERC_PR.max_med,    ERC_PR.max_q75,    ERC_PR.max_q90,
                            
                  ERC_PR.pctChg_med, ERC_PR.pctChg_q75, ERC_PR.pctChg_q90)   


# tab.risk.selected %>% filter(grepl("I1", runname)) %>%  kable(digits = 3)
# tab.risk.selected %>% filter(grepl("I2", runname)) %>%  kable(digits = 3)
# tab.risk.selected %>% filter(grepl("I3", runname)) %>%  kable(digits = 3)
# tab.risk.selected %>% filter(grepl("I4", runname)) %>%  kable(digits = 3)
# tab.risk.selected %>% filter(grepl("I5", runname)) %>%  kable(digits = 3)
# 
# tab.risk.selected %>% filter(grepl("^B", runname)) %>%  kable(digits = 3)


write.xlsx2(tab.risk, file = paste0(IO_folder,"/", "Analysis_Investment/Inv_Tables.xlsx"), sheetName = "risks1", append = TRUE)




