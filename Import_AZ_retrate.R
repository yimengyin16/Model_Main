
library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
# library(data.table)
library(readxl)


AZ_data <- read_excel("Data/Retrate_AZ.xlsx", skip = 1) %>% filter(!is.na(Age)) %>% 
           gather(yos.id, qxr, -Age) %>% 
           rename(age.id = Age) %>% 
           mutate(yos.id = f2n(yos.id),
                  qxr = qxr/100)

retrate_AZ <- expand.grid(age = 50:75, yos = 0:55)

retrate_AZ[retrate_AZ$age %in% 50 ,"age.id"] <-  50
retrate_AZ[retrate_AZ$age %in% 51:55 ,"age.id"] <-  55
retrate_AZ[retrate_AZ$age %in% 56:60 ,"age.id"] <-  60
retrate_AZ[retrate_AZ$age %in% 61:62 ,"age.id"] <-  62
retrate_AZ[retrate_AZ$age %in% 63:65 ,"age.id"] <-  65
retrate_AZ[retrate_AZ$age %in% 66:85 ,"age.id"] <-  70

retrate_AZ[retrate_AZ$yos %in% 0:3   ,"yos.id"] <-  3
retrate_AZ[retrate_AZ$yos %in% 11:18 ,"yos.id"] <-  18
retrate_AZ[retrate_AZ$yos %in% 25 ,   "yos.id"] <-  25
retrate_AZ[retrate_AZ$yos %in% 31:55 ,"yos.id"] <-  31


retrate_AZ %<>% left_join(AZ_data) %>% select(-yos.id, -age.id) %>% 
                splong("yos", 0:55) %>% 
                filter(age - yos >= 20) %>% 
                mutate(planname = "AZ-PERS-6")

retrate_AZ %>% spread(yos, qxr)

save(retrate_AZ, file = "Data/retrates_AZ.RData")


