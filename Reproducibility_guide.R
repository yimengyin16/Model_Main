# Master file for preproducing model results in reports M1 and M2.1
# Yimeng Yin
# Oct 2016

# Report M1:    http://www.rockinst.org/pdf/government_finance/2016-06-02-Pension_Funding_Practices.pdf
# Report M2.1a: http://www.rockinst.org/pdf/government_finance/2016-12-07-Demographic_Policy_Brief.pdf
# Report M2.1b: http://www.rockinst.org/pdf/government_finance/2017-01-10-Pension_Investment_Risks.pdf


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
# Only one simulation will be run by default for illustration, 
# User can chnage column C in "/IO_M1_new/Repord_RunControl_M1_new.xlsx)" to 
# include more simulations.


## Run model for report M2.1a and M2.1b
source("Model_RunControl_reprod_M2.1.R")
# Only one simulation will be run by default for illustration, 
# User can chnage column C in "/IO_M2.1_new/Repord_RunControl_M2.1_new.xlsx)" to 
# include more simulations.

## Where are the outputs saved
# Report M1:      "IO_M1_new/"
# Report M2.1a/b: "IO_M2.1_new/"


#**************************************************************************************
#                      Producing graphs and tables                                 ####
#**************************************************************************************

## Producing graphs and tables
source("IO_M1_new/Report_M1.R")  
source("IO_M2.1_new/Report_M2.1a.R")
source("IO_M2.1_new/Report_M2.1b.R")

## Where are the outputs saved
 # Report M1:      "IO_M1_new/"
 # Report M2.1a:   "IO_M2.1_new/M2.1a_outputs"
 # Report M2.1b:   "IO_M2.1_new/M2.1b_outputs"




