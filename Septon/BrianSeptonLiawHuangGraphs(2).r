# Don Boyd
# 10/16/2015


#****************************************************************************************************
#                System-specific definitions ####
#****************************************************************************************************
runsd <- "E:/Dropbox/Pension simulation project/01_Model_Outputs/IO_M1/"


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
library("bdata")
library("btools")
# devtools::install_github("donboyd5/pdata")
 library("pdata")
# devtools::install_github("donboyd5/ptools")
library(ptools) 


 
#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

# ma4na<-function(x) rollapply(x, 4, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
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

gpanel <- function(simnum) {
  gmval <- round(gm30$ir.gm[gm30$sim==simnum]*100, 1)
  ls <- 1.25 # linesize
  ps <- 2.25 # pointsize
  p1 <- ggplot(data=filter(df2, year<=30, sim==simnum, runf=="FR.75-30y.CP.Open") %>% # any run will do
                 select(year, i.r, ir.gm) %>% gather(variable, value, -year, -runname, -sim),
               aes(x=year, y=value*100, group=variable)) +
    geom_point(aes(colour=variable, shape=variable), size=ps) +
    geom_line(aes(colour=variable), size=ls) +
    scale_color_discrete(name="") +
    scale_shape_discrete(name="") +
    scale_linetype_discrete(name="") +
    scale_y_continuous(breaks=c(seq(-50, 5, 5), 7.5, seq(10, 50, 5)), name="Percent") + 
    scale_x_continuous(breaks=seq(0, 30, 5)) +
    # geom_hline(y=mean(df$ir.gm[df$year==30 & df$sim==simnum])*100) +
    geom_hline(y=7.5) +
    geom_hline(y=0, linetype="dotdash", size=.5) +
    #geom_hline(y=gm30$ir.gm[gm30$sim==simnum]*100, size=.75) +
    labs(title=paste0("Annual returns and cumulative geometric mean: sim # ", simnum, "\n30-year geometric mean=", gmval)) +
    theme(plot.title=element_text(size=14))
  
  p2 <- ggplot(data=filter(df2, year<=30, sim %in% c(simnum, 0), runf %in% runs),
               aes(x=year, y=C_PR, group=runf2)) +
    geom_point(aes(colour=runf2, shape=runf2), size=ps) +
    geom_line(aes(colour=runf2), size=ls) +
    scale_color_discrete(name="") +
    scale_shape_discrete(name="") +
    scale_linetype_discrete(name="") +
    scale_y_continuous(breaks=seq(-10, 100, 5), name="Percent") + 
    scale_x_continuous(breaks=seq(0, 30, 5)) +
    geom_hline(y=ncpct$mdn) +
    labs(title=paste0("Contributions as % of payroll: sim # ", simnum)) +
    theme(plot.title=element_text(size=14))
  
  p3 <- ggplot(data=filter(df2, year<=31, sim %in% c(simnum, 0), runf %in% runs), # use year 31 to get opening balance
               aes(x=year, y=FR_MA, group=runf2)) +
    geom_point(aes(colour=runf2, shape=runf2), size=ps) +
    geom_line(aes(colour=runf2), size=ls) +
    scale_color_discrete(name="") +
    scale_shape_discrete(name="") +
    scale_linetype_discrete(name="") +
    scale_y_continuous(breaks=seq(30, 300, 10), name="Funded ratio at beginning of year (MV assets)") + 
    scale_x_continuous(breaks=seq(0, 30, 5)) +
    geom_hline(y=100) +
    labs(title=paste0("Funded ratio: sim # ", simnum)) +
    theme(plot.title=element_text(size=14))
  
  
  # Add a table
  tbl <- filter(df2, year<=30, sim %in% c(simnum, 0), runf %in% runs) %>%
    group_by(runname, runf2) %>%
    arrange(year) %>%
    mutate(Contrib.5yrchg=C_PR - lag(C_PR, 5),
           FR.5yrchg=FR_MA - lag(FR_MA, 5)) %>%
    summarise(Contrib.sd=round(sd(C_PR, na.rm=TRUE), 1),
              FR.sd=round(sd(FR_MA, na.rm=TRUE), 1),
              Contrib.max5yr_rise=round(max(Contrib.5yrchg, na.rm=TRUE), 1),
              FR.max5yr_fall=round(-min(FR.5yrchg, na.rm=TRUE), 1),
              Contrib.max5yr_rise=ifelse(Contrib.max5yr_rise>=0, Contrib.max5yr_rise, NA),
              FR.max5yr_fall=ifelse(FR.max5yr_fall>=0, FR.max5yr_fall, NA)) %>%
    mutate(Run=runf$keyfeature[match(runname, runf$runname)],
           Run=ifelse(Run=="30-year constant percent - open (Deterministic)",
                      "30-year constant percent - open\n(Deterministic)",
                      Run)) %>%
    ungroup %>%
    #arrange(runf2) %>%
    select(Run, Contrib.sd, FR.sd, Contrib.max5yr_rise, FR.max5yr_fall) %>%
    as.data.frame
  tbl
  colnames <- str_replace(names(tbl), "\\.", "\n")
  grob <- tableGrob(tbl, rows=NULL, cols=colnames)
  
  ml <- marrangeGrob(list(p1, p2, p3, grob), nrow=2, ncol=2, top=NULL)
  #ml
  ggsave(paste0("g_", simnum, ".png"), ml, width=13, height=7.5, units="in")
  return(ml)
}




#****************************************************************************************************
#                Get data ####
#****************************************************************************************************

s <- "runname,runf,text,keyfeature
C1F075_0,	   FR.75-none,	                     75% initial Funding; No amortization; No asset smoothing,   No amortization or smoothing
C1F075_1,	   FR.75-10y.CD.Open,	               75% initial Funding; Open 10-year cd; no asset smoothing,    10-year constant dollar - open
C1F075_1c,	 FR.75-10y.CD.Closed,	             75% initial Funding; Closed 10-year cd; no asset smoothing,    10-year constant dollar - closed
C1F075_2c,	 FR.75-30y.CD.Closed,	             75% initial Funding; Closed 30-year cd; no asset smoothing,    30-year constant dollar - closed
C1F075_3,	   FR.75-30y.CP.Open,	               75% initial Funding; Open 30-year cp; no asset smoothing,    30-year constant percent - open
C1F075_3c,	 FR.75-30y.CP.Closed,              75% initial Funding; Closed 30-year cp; no asset smoothing,    30-year constant percent no asset - closed
C1F075_4,	   FR.75-Asset5,	                   75% initial Funding; No amortization; 5-year asset smoothing,    5-year asset-smoothing only
C1F075_6,	   FR.75-30y.CP.Open-Asset5,	       75% initial Funding; Open 30-year cp; 5-year asset smoothing,    30-year constant percent - open
C1F075_6a,	 FR.75-30y.CP.Open-Asset5-shock,	 75% initial Funding; Open 30-year cp; 5-year asset smoothing; -25% asset shock,    30-year constant percent - open with shock
C1F075_6d,	 FR.75-30y.CP.Open-Asset5-Determ,	 75% initial Funding; Open 30-year cp; 5-year asset smoothing; DETERMINISTIC,    30-year constant percent - open (Deterministic)
C1F075_soa1, FR.75-15y.CP.Open-Asset5-6.4SOA.true75pct,	 75% initial funding; Open 15-year cp; 5-year asset smoothing; 6.4% discount rate; SOA,    SOA contribution benchmark - true 75% FR at 6.4%
C1F075_soa2, FR.75-15y.CP.Open-Asset5-7.5SOA,	 75% initial funding; Open 15-year cp; 5-year asset smoothing; 7.5% discount rate; SOA,    SOA contribution benchmark but 7.5% discount rate
C1F075_soa3, FR.75-15y.CP.Open-Asset5-6.4SOA,  75% initial funding (calculated at 7.5% discount rate); Open 15-year cp; 5-year smoothing; 6.4% discount rate for contributions; SOA SCB,    SOA contribution benchmark
C1F075_u,    FR.75-unbounded,	                 75% initial Funding; No amortization; No asset smoothing; no non-negative restriction,    No amortization or smoothing - unbounded
"

runf <- read_csv(s) %>% mutate_each(funs(str_trim))
runf

allrunfiles <- list.files(runsd, pattern="RData")


f <- function(rname) str_subset(allrunfiles, coll(rname)) # use coll so we can have period in the pattern
fnsearch <- c(paste0(runf$runname, "_"), paste0(runf$runname, ".")) # search pattern might be followed by _ or .
files2get <- unlist(lapply(fnsearch, f))

system.time(df1 <- ldply(files2get, getrun, runsd)) # CAUTION: assumes there is only one file of each type in the dir
glimpse(df1)
count(df1, runname)


#****************************************************************************************************
#                Probabilities of ruin graph ####
#****************************************************************************************************

# Probability of Funded Status Falling Below a Threshold Level by a Given Year (i.e., Probability of ruin);
# Percentile: All; Median Geometric Return: 7.52%

# construct Brian Septon's probability of ruin ####
tmp <- df1 %>% filter(runname=="C1F075_6") %>% select(runname, year, sim, FR_MA) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(x = row_number(),
         FR_MA.min=rollmin(FR_MA, row_number())) %>%
  mutate(prob50=FR_MA.min<50, prob40=FR_MA.min<40, prob30=FR_MA.min<30, prob20=FR_MA.min<20, prob10=FR_MA.min<10) %>%
  gather(variable, value, starts_with("prob")) %>%
  group_by(year, variable) %>%
  summarise(value=mean(value))

filter(tmp, year<=50) %>% qplot(year, value, data=., colour=variable, geom=c("point", "line")) + scale_y_continuous(breaks=seq(0, .8, .1))

# why are our probs of ruin so much lower than Brian's? He says he had statutory funding rule - maybe
# that's it; I wonder what FR he started at


#****************************************************************************************************
#                Probability of ruin vs compound investment return graph ####
#****************************************************************************************************
# looks like he calc'd annualized returns for year 40 and then binned the results somehow - maybe 50
# sims per return bin - and got mean prob for each bin
tmp <- df1 %>% filter(runname=="C1F075_6", year<=50) %>% select(runname, year, sim, FR_MA, i.r) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(FR_MA.min=rollmin(FR_MA, row_number()), 
         i.rc=cumprod(1 + i.r)^(1/year)-1, 
         i.rc40=i.rc[year==40]) %>%
  mutate(prob50=FR_MA.min<50, prob40=FR_MA.min<40, prob30=FR_MA.min<30, prob20=FR_MA.min<20, prob10=FR_MA.min<10) %>%
  ungroup %>%
  mutate(bin=ntile(i.rc40, 50)) %>%
  group_by(year, bin) %>%
  summarise_each(funs(mean), starts_with("prob"), i.rc40)

count(tmp, bin)
qplot(i.rc40, prob30, data=filter(tmp, year==40), geom="point") + scale_x_continuous(breaks=seq(.01, .15, .01))

# add cumulative prob distribution of i.rc40 to the graph
cdist <- df1 %>% filter(runname=="C1F075_6", year<=50) %>% select(runname, year, sim, i.r) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(i.rc=cumprod(1 + i.r)^(1/year)-1) %>%
  filter(year==40)

quantile(cdist$i.rc)


t1 <- "Probability of ruin (i.e., of falling below 30% funded ratio) - scatter"
t2 <- "\nAnd cumulative distribution of investment returns - curved line"
t3 <- paste0(t1, t2)
ggplot() + 
  geom_point(data = filter(tmp, year==40), aes(x = i.rc40, y=prob30), size=1.5, colour="blue") + 
  stat_ecdf(data=cdist, aes(i.rc), size=.7) +
  #geom_hline(y=.5) + 
  geom_vline(x=.0685, linetype="dashed") + 
  labs(title=t3) +
  scale_x_continuous(breaks=c(seq(0, .06, .01), .0686, seq(.08, .20, .01)),
                     limits = c(.02, .12), # will exclude some obs but that's ok
                     name="Compound annual investment return at 40 years") +
  scale_y_continuous(name="Probability", breaks=seq(0, 1, .1))


#****************************************************************************************************
#                Present value of contributions vs compound investment return graph ####
#****************************************************************************************************  
glimpse(df1)
# maybe best to use C_PR? or maybe just C - but then we'd have to normalize somehow?
# stick with 40 years for now even though we may prefer to use 30 years
v <- 1 / (1 + .075)
tmp <- df1 %>% filter(runname=="C1F075_6", year<=40) %>% select(runname, year, sim, i.r, C_PR) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(i.rc=cumprod(1 + i.r)^(1/year)-1,
         C_PR.pv=C_PR * (v^year),
         C_PR.pvsum=cumsum(C_PR.pv))

t1 <- "Present value of contributions as % of payroll over 40 years"
t2 <- "\nversus compound average investment returns at 40 years"
t3 <- paste0(t1, t2)
ggplot() + 
  geom_point(data = filter(tmp, year==40), aes(x = i.rc, y=C_PR.pvsum), size=1.5, colour="blue") + 
  # stat_ecdf(data=cdist, aes(i.rc), size=.7) + # could add this if we scale C_PR.pvsum to [0, 1]
  geom_hline(y=median(tmp$C_PR.pvsum[tmp$year==40])) + 
  geom_vline(x=.0685, linetype="dashed") + 
  labs(title=t3) +
  scale_x_continuous(breaks=c(seq(0, .06, .01), .0686, seq(.08, .20, .01)),
                     limits = c(.02, .12), # will exclude some obs but that's ok
                     name="Compound annual investment return at 40 years") +
  scale_y_continuous(name="Present value of cumulative contributions (as % of payroll) over 40 years",
                     breaks=seq(0, 800, 50))


v <- 1 / (1 + .075)
tmp <- df1 %>% filter(runname=="C1F075_6", year<=40) %>% select(runname, year, sim, i.r, C) %>%
  group_by(sim) %>%
  arrange(year) %>%
  mutate(i.rc=cumprod(1 + i.r)^(1/year)-1,
         C.pv=C * (v^year),
         C.pvsum=cumsum(C.pv))

t1 <- "Present value of contributions over 40 years"
t2 <- "\nversus compound average investment returns at 40 years"
t3 <- paste0(t1, t2)
ggplot() + 
  geom_point(data = filter(tmp, year==40), aes(x = i.rc, y=C.pvsum), size=1.5, colour="blue") + 
  # stat_ecdf(data=cdist, aes(i.rc), size=.7) + # could add this if we scale C_PR.pvsum to [0, 1]
  geom_hline(y=median(tmp$C.pvsum[tmp$year==40])) + 
  geom_vline(x=.0685, linetype="dashed") + 
  labs(title=t3) +
  scale_x_continuous(breaks=c(seq(0, .06, .01), .0686, seq(.08, .20, .01)),
                     limits = c(.02, .12), # will exclude some obs but that's ok
                     name="Compound annual investment return at 40 years") +
  scale_y_continuous(name="Present value of cumulative contributions over 40 years")



#****************************************************************************************************
#                Histograms and density curves ####
#****************************************************************************************************

# construct Brian Septon's probability of ruin ####
tmp <- df1 %>% filter(runname=="C1F075_6") %>% select(runname, year, sim, FR_MA, C_PR)

# define basic graph , binwidth=5
p <- ggplot(aes(x=FR_MA), data=filter(tmp, year==30)) + geom_histogram(fill="blue", binwidth=5) +
  scale_x_continuous(limits=c(0, 200), breaks=seq(0, 1000, 20)) +
  geom_vline(x=75, linetype="dashed") +
  geom_vline(x=100, linetype="solid")
p

# Density curve
ggplot(aes(x=FR_MA), data=filter(tmp, year==30)) + geom_density()

# Histogram overlaid with kernel density curve
dat <- filter(tmp, year==30)
ggplot(aes(x=FR_MA), data=dat) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=5,
                 colour="black", fill="white") +
  # Overlay with transparent density plot
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(limits=c(0, 200), breaks=seq(0, 1000, 20)) +
  geom_vline(x=75, linetype="dashed") +
  geom_vline(x=median(dat$FR_MA)) +
  geom_vline(x=mean(dat$FR_MA), colour="green") +
  geom_vline(x=100, linetype="solid") +
  stat_function(fun=dnorm, color="blue", arg=list(mean=mean(dat$FR_MA), sd=sd(dat$FR_MA)))

# # + stat_function(fun=dnorm,
# color="red",
# arg=list(mean=mean(mtcars$mpg), 
#          sd=sd(mtcars$mpg)))


# Kernel density curve by year
mdline <- .8
ggplot(aes(x=FR_MA, colour=year, group=year), data=filter(mutate(tmp, year=as.factor(year)), year %in% c(20, 30, 40))) + 
  geom_density(fill=NA, size=1) +
  scale_x_continuous(limits=c(0, 200), breaks=seq(0, 1000, 20)) +
  # geom_vline(x=75, linetype="dashed") +
  geom_vline(x=100, linetype="solid") +
  geom_vline(x=median(tmp$FR_MA[tmp$year==40]), linetype="dotted", colour="blue", size=mdline) +
  geom_vline(x=median(tmp$FR_MA[tmp$year==30]), linetype="dotted", colour="green", size=mdline) +
  geom_vline(x=median(tmp$FR_MA[tmp$year==20]), linetype="dotted", colour="red", size=mdline)


#****************************************************************************************************
#                C_PR ####
#****************************************************************************************************

dat <- filter(tmp, year==30)

ggplot(aes(x=C_PR), data=dat) + 
  geom_histogram(aes(y=..density.. * 100),
                 binwidth=1,
                 colour="black", fill="green")

ggplot(aes(x=C_PR), data=dat) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  # Overlay with transparent density plot
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 100, 2)) +
  #geom_vline(x=75, linetype="dashed") +
  geom_vline(x=median(dat$C_PR)) +
  geom_vline(x=mean(dat$C_PR), colour="green") +
  # geom_vline(x=100, linetype="solid") +
  stat_function(fun=dnorm, color="blue", arg=list(mean=mean(dat$C_PR), sd=sd(dat$C_PR)))






