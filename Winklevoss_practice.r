# Winklevoss Walkthrough based on Don's code
# Yimeng Yin
# 12/13/2013

# The code in script replicate works through Winklevoss pension book following the code written by Don.(Winklevoss(7).r)
# for practice purpose. 
# Don's comments are retained as much as possible



## Preamble ####

# Defining data directory and data file
wvd <- "E:/Dropbox (Personal)/Proj-PenSim/Winklevoss/"
wvxl <- "Winklevoss(6).xlsx"


library(zoo) # rollapply
library(knitr)
library(gdata) # read.xls
library(dplyr)
library(ggplot2)
library(tidyr) # gather, spread
library(xlsx)

# functions created by Don
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA
ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail


#****************************************************************************************************
#
#                Read and save model-plan tables - RUN ONCE ####
#
#****************************************************************************************************

# Data table 2-1 mortality ####
# Mortality table GAM-1971 (http://mort.soa.org/Export.aspx?Type=xls&TableIdentity=818) is used. 
fn <- "GAM-1971-Male.xls"
gam1971 <- read.xls(paste0(wvd, fn), colClasses = "character")
names(gam1971) <- c("age", "qxm")
gam1971 <- gam1971 %>%  mutate_each(funs(cton)) %>%
           filter(age %in% 5:110)


# data table 2-3 termination rates ####
term <- read.xls(paste0(wvd, wvxl), sheet = "Tab2-3TermRates", colClasses = "character")
names(term) <- c("age", paste0("ea", seq(20, 60, 5)))
term <- term %>% mutate_each(funs(cton)) %>%
        filter(!is.na(age))

# data table 2-5 disability life rates ####
dbl <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-5DisbLife", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(dbl) <- c("age", "qxmd")
dbl <- dbl %>%
  mutate_each(funs(cton))


# data table 2-7 disability ####
disb <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-7Disb", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(disb) <- c("age", "qxd")
disb <- disb %>%
  mutate_each(funs(cton))


# data table 2-9 early retirement ####
er <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-9EarlyRet", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(er) <- c("age", "qxe")
er <- er %>%
  mutate_each(funs(cton))


# data table 2-10 salary merit table ####
merit <- read.xlsx2(paste0(wvd, wvxl), sheetName = "Tab2-10Merit", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(merit) <- c("age", "scale")
merit <- merit %>%
  mutate_each(funs(cton))


# data table 4-6 hiring distribution table ####
hire <- read.xls(paste0(wvd, wvxl), sheet= "Tab4-6HireDist", colClasses = "character")
hire <- hire[-1, ]
names(hire) <- c("eage", "dist", "salscale")
hire <- hire %>%
  mutate_each(funs(cton))


save(gam1971, term, dbl, disb, er, merit, hire, file = paste0(wvd, "winklevossdata.rdata"))

## Note the difference between read.xls(gdata) and read.xlsx(xlsx)



#****************************************************************************************************
#
#                Construct various tables in the book ####
#
#****************************************************************************************************







