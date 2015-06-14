library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())
source("Functions.R")
folder_run <- "IO_Initial_Runs"



# for (fileName in dir(folder_outputs)){
#   folder_outputs <- "Outputs_Initial_Runs_2"
#   
# #  fileName <- "Outputs_R1F2_06-04-2015.RData"
#   
#   load(paste0(folder_outputs, "/", fileName))
#   
#   RunName <- outputs_list$paramlist$runname
#   
#   outputs_list$results %<>% mutate(FR_MA   = 100 * MA / exp(log(AL)),
#                                    B_PR    = 100 * B / PR, 
#                                    MA_PR   = 100 * MA / PR,
#                                    ERC_PR  = 100 * ERC / PR,
#                                    dERC_PR = ERC_PR - lag(ERC_PR),
#                                    ADC_PR  = 100 * ADC / PR, 
#                                    C_PR    = 100 * C / PR,
#                                    runname = RunName) %>% 
#                             select(runname, sim, year, everything())
#   
#   save(outputs_list, file = paste0(folder_outputs, "/", fileName))
#   
# }




rm(list = ls())
source("Functions.R")
folder_outputs <- "Outputs"

load(paste0(folder_outputs, "/", "Outputs_R4F1_06-06-2015.RData"))
draw_quantiles("R4F1", "FR", outputs_list$results)


load(paste0(folder_outputs, "/", "Outputs_R4F2_06-06-2015.RData"))
draw_quantiles("R4F2", "FR", outputs_list$results)


load(paste0(folder_outputs, "/", "Outputs_R4F3_06-06-2015.RData"))
draw_quantiles("R4F3", "FR", outputs_list$results)


