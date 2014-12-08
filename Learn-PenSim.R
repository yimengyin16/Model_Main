####################################################################################
#                                 Note
#
# Code in this script is for leaning basic techniques for programming pension plans
#
# Major learning resource is Winklevoss book.
#
# Author: Yimeng Yin
# 
####################################################################################


#%%%%%%%%%%%%%%%
## Preparation %
#%%%%%%%%%%%%%%%

library(magrittr)

path_Data <- "C:/Dropbox (FSHRP)/Pension simulation project/How to model pension funds/Winklevoss"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Loading Winkelvoss data  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%####


load(paste0(path_Data, "/winklevossdata.Rdata"))

# Notes on data objects in the "winklevoss.Rdata"

 # disb:    Disability rates,  Table  2-7, p23
 # merit:   Merit Salary scale, Table 2-10, p27
 # hire:    Hiring age distribution  and salary scale Table 4-6, p64
 # er:      Early retirement rates Table 2-9, p25
 # gam1971: Mortality Rates for Males, Table 2-1, p16
 # dbl:     Disabled-Life Mortality Rates, Table 2-5, p22
 # term:    Termination Rates Table 2-3, p19





