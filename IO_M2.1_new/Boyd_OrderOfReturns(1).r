# Don Boyd
# 11/1/2015


#****************************************************************************************************
#                System-specific definitions ####
#****************************************************************************************************
runsd <- "IO_M1_new/"


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("plyr") # always load BEFORE loading dplyr
library("dplyr")
library("magrittr")
library("tidyr")
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library("foreign") # various import and export routines - e.g., for reading Stata files
# library("gdata") # for reading spreadsheets
library("knitr")
library("lubridate")
# devtools::install_github("hadley/ggplot2") # latest version arranges grobs
library("ggplot2")
library("readr")
library("readxl")
library("stringr")
library("zoo") # for rollapply

library("grid")
library("gridExtra") # to create objects to place on graphs

# library("apitools")
# library("bdata")
# library("btools")
# devtools::install_github("donboyd5/pdata")
# library("pdata")
# devtools::install_github("donboyd5/ptools")

#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

# ma4 <- function(x) {rollapply(x, 4, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}
rollsd <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) sd(x, na.rm=TRUE), fill=NA, align="right")}
# note that this is sample standard deviation

rollmean <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}

rollmin <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) min(x, na.rm=TRUE), fill=NA, align="right")}

getrun <- function(runfile, dir) {
  fn <- paste0(dir, runfile)
  load(fn)
  df <- outputs_list$results
  return(df)
}


#****************************************************************************************************
#                Get data ####
#****************************************************************************************************

allrunfiles <- list.files(runsd, pattern="RData")

# define desired runs
# f <- function(rname) str_subset(allrunfiles, coll(rname)) # use coll so we can have period in the pattern
# fnsearch <- c(paste0(runs, "_"), paste0(runs, ".")) # search pattern might be followed by _ or .
# files2get <- unlist(lapply(fnsearch, f))

djbruns <- c(
  "0",
  "C15d",
  "C15p",
  "O15d",
  "O15p",
  "O30d",
  "O30p",
  "C30p",
  "O30pA5",
  "O30pA5_cap",
  "soa3",
  "soa4"
)
files2get <- paste0("Outputs_A1F075_", djbruns, ".RData")

system.time(df1 <- ldply(files2get, getrun, runsd))
glimpse(df1)

#****************************************************************************************************
#                Get returns and classify them by groups ####
#****************************************************************************************************
# get sims near a given percentile of ir.gm at year 30
irgm <- df1 %>% filter(runname=="A1F075_O30pA5", sim>0) %>%
  select(year, sim, i.r) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(ir.gm=cumprod(1+i.r)^(1/year) - 1)

# %>%  group_by(year) %>%  mutate(ir.gm.ptile=percent_rank(ir.gm))

# get year 15 and year 30 ir.gm to classify sims as high returns early vs high returns late 
ir.groups <- irgm %>% select(-i.r) %>% filter(year %in% c(15, 30)) %>%
  mutate(irname=paste0("ir.gm", year)) %>%
  select(irname, sim, ir.gm) %>%
  spread(irname, ir.gm) %>%
  mutate(asset15=(1+ir.gm15) ^ 15,
         asset30=(1+ir.gm30)^30,
         asset1630=asset30 / asset15,
         ir.gm1630=asset1630 ^ (1/15)  - 1) %>%
  # now we have gm ir for 1-15, 16-30, and 1-30 - gather and then get percentiles
  select(sim, starts_with("ir")) %>%
  mutate(ptile15=percent_rank(ir.gm15),
         ptile1630=percent_rank(ir.gm1630),
         ptile30=percent_rank(ir.gm30),
         highretearly=(ir.gm15>ir.gm1630)*1,
         highretlate=(ir.gm1630>ir.gm15)*1,
         order=ifelse(highretearly, "highearly", "highlate"))
summary(ir.groups)


simgroups <- ir.groups %>% filter(ir.gm30>=.073, ir.gm30<=.075)



#****************************************************************************************************
#                Examine impact of order of returns ####
#****************************************************************************************************

v <- 1 / (1 + .075)
tmp2 <- df1 %>% filter(runname=="A1F075_O30pA5", sim>0, year<=30) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(C.pv=C * (v^(year-1)),
         PR.pv=PR * (v^(year-1)),
         C.pvsum=cumsum(C.pv),
         PR.pvsum=cumsum(PR.pv),
         C_PR.pv=C.pvsum / PR.pvsum) %>%
  filter(year==30) %>%
  mutate(order=ir.groups$order[match(sim, ir.groups$sim)],
         ir.gm30=ir.groups$ir.gm30[match(sim, ir.groups$sim)]) %>%
  # filter(ir.gm30>.074, ir.gm30<.076) %>%
  group_by(order) %>%
  summarise(n=n(),
            FR_MA=median(FR_MA),
            ir.gm30=median(ir.gm30),
            C_PR.pv=median(C_PR.pv))
tmp2

