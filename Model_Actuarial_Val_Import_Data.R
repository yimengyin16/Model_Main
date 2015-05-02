# This program imports the following data for the actuarial valuation model
 # Decrement tables

library(dplyr)
library(tidyr)
library(gdata) # read.xls
library(xlsx)
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA

file_path <- paste0("Data/")
wvxl <- "Winklevoss(6).xlsx"


# Data table 2-1 mortality ####
# Mortality table GAM-1971 (http://mort.soa.org/Export.aspx?Type=xls&TableIdentity=818) is used. 
fn <- "GAM-1971-Male.xls"
gam1971 <- read.xls(paste0(file_path, fn), colClasses = "character")
names(gam1971) <- c("age", "qxm.p")
gam1971 <- gam1971 %>%  mutate_each(funs(cton)) %>%
  filter(age %in% 5:110)


# data table 2-3 termination rates ####
term <- read.xls(paste0(file_path, wvxl), sheet = "Tab2-3TermRates", colClasses = "character")
names(term) <- c("age", paste0("ea", seq(20, 60, 5)))
term <- term %>% mutate_each(funs(cton)) %>%
  filter(!is.na(age))


# Reorganize termination table into long format
term2 <- term %>% 
  gather(ea, qxt.p, -age) %>%
  mutate(ea = as.numeric(gsub("[^0-9]", "", ea)),
         qxt.p=ifelse(is.na(qxt.p), 0, qxt.p))


# data table 2-5 disability life rates ####
dbl <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-5DisbLife", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(dbl) <- c("age", "qxmd.p")
dbl <- dbl %>%
mutate_each(funs(cton))


# data table 2-7 disability ####
disb <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-7Disb", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(disb) <- c("age", "qxd.p")
disb <- disb %>%
  mutate_each(funs(cton))


# data table 2-9 early retirement ####
er <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-9EarlyRet", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(er) <- c("age", "qxe.p")
er <- er %>%
  mutate_each(funs(cton))


# data table 2-10 salary merit table ####
merit <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-10Merit", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(merit) <- c("age", "scale")
merit <- merit %>%
  mutate_each(funs(cton))


# data table 4-6 hiring distribution table ####
hire <- read.xls(paste0(file_path, wvxl), sheet= "Tab4-6HireDist", colClasses = "character")
hire <- hire[-1, ]
names(hire) <- c("eage", "dist", "salscale")
hire <- hire %>%
  mutate_each(funs(cton))

save(gam1971, term, term2, dbl, disb, er, merit, hire, file = paste0(file_path, "winklevossdata.RData"))