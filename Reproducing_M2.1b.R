# Master file for preproducing graphs and tables in reports M1 and M2.1
# Yimeng Yin
# Oct 2016


#**************************************************************************************
#                       Software requirements                                      ####
#**************************************************************************************
# 1. R      https://cran.r-project.org/mirrors.html
# 2. Rtools https://cran.r-project.org/bin/windows/Rtools/ (Choose Rtools version compatible with the version of R)
# 3. Rstudio with packrat support https://www.rstudio.com/products/rstudio/#Desktop
# 4. Java https://java.com/en/download/ (Choose 32-bit or 64-bit version depending on the operating system)



#**************************************************************************************
#                      Lunch RStudio and open project "Model.Main" in Rstudio     ####
#**************************************************************************************
# Packrate will download and install all required packages automatically



#**************************************************************************************
#                      Running model                                               ####
#**************************************************************************************

## Run model for report M1
source("Model_RunControl_reprod_M1.R")

## Run model for report M2.1a and M2.1b
source("Model_RunControl_reprod_M2.1.R")


#**************************************************************************************
#                      Producing graphs and tables                                 ####
#**************************************************************************************

## Producing graphs and tables
source("IO_M1_new/Report_M1.R")  
source("IO_M2.1_new/Report_M2.1a.R")
source("IO_M2.1_new/Report_M2.1b.R")

## Where are the outputs saved
 # Report M1: "IO_M1_new/M1_outputs/"
 # "IO_M2.1_new/M2.1a_outputs"
 # "IO_M2.1_new/M2.1b_outputs"

# test change


