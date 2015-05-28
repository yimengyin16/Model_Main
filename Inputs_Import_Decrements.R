# This program imports the following data for the actuarial valuation model
 # Decrement tables

library(dplyr)
library(tidyr)
library(gdata) # read.xls
library(xlsx)
library(XLConnect) # slow but convenient because it reads ranges

cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA

file_path <- paste0("Data/")
wvxl <- "Winklevoss(6).xlsx"

# 
# # Data table 2-1 mortality ####
# # Mortality table GAM-1971 (http://mort.soa.org/Export.aspx?Type=xls&TableIdentity=818) is used. 
# fn <- "GAM-1971-Male.xls"
# gam1971 <- read.xls(paste0(file_path, fn), colClasses = "character")
# names(gam1971) <- c("age", "qxm")
# gam1971 <- gam1971 %>%  mutate_each(funs(cton)) %>%
#   filter(age %in% 5:120)
# 
# 
# # data table 2-3 termination rates ####
# term <- read.xls(paste0(file_path, wvxl), sheet = "Tab2-3TermRates", colClasses = "character")
# names(term) <- c("age", paste0("ea", seq(20, 60, 5)))
# term <- term %>% mutate_each(funs(cton)) %>%
#   filter(!is.na(age))
# 
# # expand the termination rates to all entry ages, assuming the rates are the same within each 5-year interval
# 
# term2 <- term
# term2[36:40,2:7] <- term2[36:40,8]
# term2[41:45,2:8] <- term2[41:45,9]
# 
# 
# # Reorganize termination table into long format
# term2 %<>% 
#   gather(ea, qxt, -age) %>%
#   mutate(ea = as.numeric(gsub("[^0-9]", "", ea)),
#          qxt=ifelse(is.na(qxt), 0, qxt))
# 
# 
# term3 <-  expand.grid(age = 20:64, ea = 20:64) %>% 
#           mutate(ea.match = floor(ea*2/10)/2*10,  
#                  yos = age - ea) %>% 
#           left_join(term2 %>% rename(ea.match = ea)) %>%
#           filter(yos >= 0) %>% 
#           select(-ea.match)


# data table 2-5 disability life rates ####
dbl <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-5DisbLife", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(dbl) <- c("age", "qxmd")
dbl <- dbl %>%
mutate_each(funs(cton))


# data table 2-7 disability ####
disb <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-7Disb", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(disb) <- c("age", "qxd")
disb <- disb %>%
  mutate_each(funs(cton))


# data table 2-9 early retirement ####
er <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-9EarlyRet", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
names(er) <- c("age", "qxr")
er <- er %>%
  mutate_each(funs(cton))


# # data table 2-10 salary merit table ####
# merit <- read.xlsx2(paste0(file_path, wvxl), sheetName = "Tab2-10Merit", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
# names(merit) <- c("age", "scale")
# merit <- merit %>%
#   mutate_each(funs(cton))


# data table 4-6 hiring distribution table ####
hire <- read.xls(paste0(file_path, wvxl), sheet= "Tab4-6HireDist", colClasses = "character")
hire <- hire[-1, ]
names(hire) <- c("eage", "dist", "salscale")
hire <- hire %>%
  mutate_each(funs(cton))




save(dbl, disb, er, hire, file = paste0(file_path, "winklevossdata.RData"))