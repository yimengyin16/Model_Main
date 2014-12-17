# Crane(#).r
# Don Boyd
# 4/29/2013

# Create and run a VERY simple stochastic pension simulation, just to see how to do it
# The numbers are ENTIRELY made up
# All it really does is show how to incorporate stochastic investment returns.

# Rather than calculating benefit payments and contributions, it just assumes an annual net payment amount
# that becomes increasingly negative as time goes on (the workforce gets older, we have more and more retirees,
# and the pension fund balance runds down)

# Workforce flows are NOT calculated

# It does NOT model liabilities. It compares actual assets to expected assets.


#****************************************************************************************************
#
#                Load packages ####
#
#****************************************************************************************************
library(plyr)
library(ggplot2)


#****************************************************************************************************
#
#                Define directories and constants ####
#
#****************************************************************************************************



#****************************************************************************************************
#
#                Define functions ####
#
#****************************************************************************************************
pdiff <- function(aa, xa) return((aa - xa) / xa * 100) # % diff, actual assets from expected assets

pmt <- function(p, i, n) { 
  # amortization function
  # p=principal, i=interest rate, n=periods
  pmt <- p * (1+i)^n * i / ((1+i)^n - 1)
  return(pmt)
}

pvann <- function(i, n, pmt) {
  # present value of an annuity-immediate (with pmt at end of period)
  if(i==0) pv <- n * pmt else pv <- pmt * ((1 - (1 + i)^(-n)) / i)
  return(pv)
}

gaip <- function(p, i, n, g) {
  # graduated annuity initial payment - where payments grow at constant annual rate
  # p=principal, i=interest rate, n=periods, g=growth rate in payments
  # returns POSITIVE payment for positive principal, unlike Excel
  imgc <- (1 + i) / (1 + g) - 1 # i minus g, compounded
  gaf <- (1 + i) / (1 - pvann(imgc, n-1, -1)) # grad annuity factor - note "minus" pvann to adjust sign vs Excel
  return(gaf*p)
}

# test it out - compare initial payment if growth to a flat initial payment
# p <- 10e6
# i <- .075
# n <- 30
# g <- .03
# (ipay.flat <- pmt(p, i, n))
# (ipay.gaip <- gaip(p, i, n, g))
# ipay.gaip/ipay.flat * 100 - 100 # it's a big difference, no wonder pension funds like this approach


showdist <- function(mat, probs, showcols){  
  # show the distribution of values for a given matrix, for given probabilities and selected columns
  if(missing(showcols)) showcols <- 1:ncol(mat)
  df <- as.data.frame(apply(mat, 2, quantile, probs))
  names(df) <- paste0("yr", 1:ncol(mat))
  return(round(df[, showcols], 1))
}




#****************************************************************************************************
#
#                Assumptions and initialization ####
#
#****************************************************************************************************
nyears <- 100
nsims <- 10e3

# set up empty vectors
# cash flows
# benpay<-vector("numeric",nyears) # not used yet
# contrib<-vector("numeric",nyears) # not used yet
expassetseoy <- vector("numeric", nyears)  # expected assets at end of year

# set up empty matrices
# position
assetsboy <- matrix(nrow=nsims, ncol=nyears) # beginning of year
assetseoy <- matrix(nrow=nsims, ncol=nyears) # end of year
assetseoydiff <- matrix(nrow=nsims, ncol=nyears) # end of year difference between actual and expected assets
assetsinv <- matrix(nrow=nsims,ncol=nyears) # investible
# flows
ii <- matrix(nrow=nsims, ncol=nyears) # investment income
netflow_xii <- matrix(nrow=nsims, ncol=nyears) # net flow before investment income
amort <- matrix(nrow=nsims, ncol=nyears) # amortization payment required, for investment shortfalls

# initialize
asset0 <- 100
assetsboy[, 1] <- asset0 # assetsboy in year 1
amort[, 1] <- 0 # amortization in year 1


#****************************************************************************************************
#
#                Set the simulation up ####
#
#****************************************************************************************************
set.seed(4321) # so results are reproducible
irmean <- .075 # investment return mean - the CalPERS assumption is 7.5%
irsd <- .1296  # CalPERS assumes a std deviation of 12.96%
# amortization parameters for any unfunded liabilities
arate <- irmean # use the investment return assumption; other assumptions are possible
ayears <- 15 # number of years for amortization

# investment return matrix nsims x nyears -- all returns normal and independent of each other
irm <- matrix(rnorm(nsims*nyears, mean=irmean, sd=irsd), nrow=nsims, ncol=nyears)
head(irm); tail(irm)


#****************************************************************************************************
#
#                Define a simple net outflow (contributions minus benefit payments) for each year ####
#                Also, compute expected assets, assuming that expected returns (irmean) are achieved
#
#****************************************************************************************************
# create net outflow BEFORE investment income and amortization (contributions in minus benefit payments out)
# in this simple model I assume this away rather than calculate contributions and benefits
# it is the same for all sims, so it's just a vector; in complex models contributions and benefits might be stochastic

# set netflows in first and last years, and then fill in the in-between years
nffirst <- -2 # net external flow, in dollars, in the first year of the simulated data (nffirst=net flow first)
nflast <- nffirst * (1 + .01)^nyears # let net flows grow at some rate
# $ amount of net inflow or outflow each year before investment income and before amortization
# netflow_xiixa is netflow excluding investment income, excluding amortization)
(netflow_xiixa <- seq(nffirst, nflast, length.out=nyears)) # fill in the in-between years, again simplistically

# compute expected assets at year end
# (asset0 + netflow_xiixa[1]/2) * irmean is investment income - assumes net flow comes evenly during the year so on average half is investible
expassetseoy[1] <- asset0 + (asset0 + netflow_xiixa[1]/2) * irmean + netflow_xiixa[1]  
for(yr in 2:nyears) expassetseoy[yr] <- expassetseoy[yr-1] + (expassetseoy[yr-1] + netflow_xiixa[yr]/2) * irmean + netflow_xiixa[yr]
expassetseoy


#****************************************************************************************************
#
#                Run the simulation ####
#
#****************************************************************************************************
# here is the simulation - each column is a year; go through them sequentially
a <- proc.time()
for(yr in 1:nyears) {
  netflow_xii[, yr] <- netflow_xiixa[yr] + amort[, yr] # net flow before ii is the amount before amortization plus any amortization
  assetsinv[,yr] <- assetsboy[,yr] + netflow_xii[, yr] / 2 # investible assets
  ii[, yr] <- assetsinv[, yr] * irm[, yr]
  assetseoy[, yr] <- assetsboy[,yr] + netflow_xii[, yr] + ii[, yr]
  assetseoydiff[, yr] <- assetseoy[, yr] - expassetseoy[yr]
  if(yr < nyears){ # set starting values for next year
    assetsboy[, yr + 1] <- assetseoy[, yr]
    # amortization of asset shortfall - note minus sign: when diff>0 we are ahead of the game and so inflow is REDUCED
    # amort[, yr + 1] <- pmt(-assetseoydiff[, yr], arate, ayears) # level dollar
    amort[, yr + 1] <- gaip(-assetseoydiff[, yr], arate, ayears, .04) # level percent
  }
}
b <- proc.time()
b - a
# end of simulation!


#****************************************************************************************************
#
#                analyze results ####
#
#****************************************************************************************************
apply(assetseoy, 2, mean) # "2" means operate on each column -- get mean of each
apply(assetseoy, 2, sd)
assetpdiff <- sweep(assetseoy, MARGIN=2, expassetseoy, pdiff) # get % diff, actual assets vs. expected assets
head(assetpdiff); tail(assetpdiff)

# summarize distribution of results
probs <- c(0, .1, .2, .25, .33, .5, .67, .75, .8, .9, 1) # quantiles to look at
showcols <- c(1:5, seq(10, nyears, 5)) # years to look at


showdist(assetpdiff, probs, showcols) # asset differences from expected
showdist(amort, probs, showcols) # amortization of asset differences

showdist(amort/assetseoy*100, probs, showcols) # how big is amortization relative to assets?



#****************************************************************************************************
#
#                Trace and debug results as needed ####
#
#****************************************************************************************************
# put key results for all sims into one big data frame
getdf <- function(matname) {
  # create dataframe from matrix
  df <- as.data.frame(get(matname))
  ncols <- ncol(df)
  names(df) <- paste0("yr", 1:ncol(df))
  df$type <- matname
  df$simnum <- rownames(df)
  df <- df[, c(ncols+1, ncols+2, 1:ncols)]
  return(df)
}

# create two matrices from vectors
expasseteoymat <- matrix(nrow=nsims, ncol=nyears, rep(expassetseoy, nsims), byrow=TRUE)
netflowxiixamat <- matrix(nrow=nsims, ncol=nyears, rep(netflow_xiixa, nsims), byrow=TRUE)

matnames <- c("assetsboy", "netflowxiixamat", "amort", "netflow_xii", "assetsinv", "irm", "ii", 
              "assetseoy", "expasseteoymat", "assetseoydiff")
tracedf <- ldply(matnames, getdf, .progress="text")
tracedf$ordr <- as.numeric(factor(tracedf$type, levels=matnames, labels=1:length(matnames)))
tracedf$simnum <- as.numeric(tracedf$simnum)
head(tracedf); tail(tracedf)
plyr::count(tracedf, "type") # quotes no longer used in dplyr so force it to use plyr

tracedf <- arrange(tracedf, simnum, ordr)
subset(tracedf, simnum==10, select=1:9) # look at results from a specific sim

# set things up to plot a few sims
tdfl <- melt(tracedf, id=c("type", "simnum", "ordr"))
tdfl$year <- as.numeric(gsub("[yr]", "", tdfl$variable))
plyr::count(tdfl, "year")
head(tdfl)
str(tdfl)

# select some sims with year5 amortization of about 1.9 and look at them
tmp <- subset(tdfl, year==5 & type=="amort" & value<1.95 & value>1.85)
tmp$simnum
looksims <- tmp$simnum[c(1, 7, nrow(tmp)-3, nrow(tmp)-5)] # define a few sims to look at
qplot(year, value, data=subset(tdfl, simnum %in% looksims & type=="amort"), 
      colour=as.factor(simnum), geom=c("point","line")) + geom_hline(y=0)



