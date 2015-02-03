

wvd <- "E:\\Dropbox (Personal)\\Pensions\\Pension simulation project\\How to model pension funds\\Winklevoss\\"
load(paste0(wvd, "winklevossdata.rdata"))


# chapter 7 of Winklevoss


# Unfunded AL ####
# set up a data frame with expected values
nyears <- 50
i <- .08 # the assumed rate of earnings
v <- 1 / (1+i)
d <- v*i # rate of discount

AL0 <- 100 # actuarial liability, initial
assets0 <- 100 # assets, initial - no initial unfunded liability

pr0 <- .4*assets0 # initial payroll (scale to assets)
sg <- .05 # the assumed total annual rate of payroll (salary) growth

NCpct <- .1 # normal cost as a percent of payroll

Ben0 <- .065*assets0 # initial benefit payments (scale to assets)
bg <- .055 # benefits growth


dfbase <- data.frame(year=1:nyears) %>% 
  # use .e suffix for expected, .a for actual
  # all values are beginning of year unless noted with eoy
  mutate(pay.e=pr0*(1+sg)^(year-1), 
         pay.a=pay.e, # for now, assume actual and expected are the same
         
         NC.e=NCpct*pay.e,
         NC.a=NC.e,
         
         Ben.e=Ben0 * (1+bg)^(year-1),
         Ben.a=Ben.e,
         
         Cont.e=c(NC.e[1], rep(NA, nyears-1)),
         Cont.a=Cont.e,
         
         AL.e=c(AL0, rep(NA, nyears-1)),
         AL.a=AL.e,
         
         AA.e=c(assets0, rep(NA, nyears-1)),         
         AA.a=AA.e,
         
         UL.e=AL.e - AA.e,  # has to be this at the start
         UL.a=AL.a - AA.a # eq 7.1
         
         )
ht(dfbase)

# Let values evolve over time
set.seed(12)
i.a <- rnorm(nyears, mean=i, sd=.1) # investment return, actual - note that if we use i, then the geometric mean will be too low due to volatility drag
# i.a <- rep(i, nyears) # uncomment this to force actual rates to equal expected
i.a 
# note that we won't need the final year's interest rate


suppcosts <- function(type){
  # note that this does not treat positive surprises differently from negative surprises
  # thus, we could even have negative contributions - if neg supp cost exceeds positive normal cost
  # I don't think real pension systems allow negative contributions; I think their rules generally force surpluses to build [VERIFY]
  if(type=="ulconst") sc <- d * df$UL.a[j] else # keep UL (unfunded liability) constant
    if(type=="straight") sc <- df$UL.a[j] / m else # straight line over m years, open (always allow m years)
      sc <- 0
  return(sc)  
}

m <- 25 # amortization period


df <- dfbase %>% mutate(Cont.ulconst=NA, sc=NA) # add empty columns for vars we will create
for(j in 1:(nrow(df)-1)) {
  # calculate values for the current year
  # df$UL.a[j] = df$AL.a[j] - df$AA.a[j] # eq 7.1
  df$Cont.ulconst[j] <- with(df, NC.a[j] + d * UL.a[j]) # contribution needed in this year to keep UL constant - eq 7.6b
  
  # we need a rule for actual contributions at beginning of this year
#   df$Cont.a[j] <- df$Cont.ulconst[j] # simple rule
  df$sc[j] <- suppcosts("straight")
  df$Cont.a[j] <- df$NC.a[j] + df$sc[j]
  
  # calculate values for beginning of next year
  df$AL.e[j+1] <- with(df, (AL.a[j] + NC.a[j] - Ben.a[j])) * (1+i) # eq 7.3
  df$AL.a[j+1] <- df$AL.e[j+1] # for now, assume no liability surprises - treat NC as known
  
  df$AA.e[j+1] <- with(df, (AA.a[j] + Cont.a[j] - Ben.a[j])) * (1+i) # eq 7.4
  df$AA.a[j+1] <- with(df, (AA.a[j] + Cont.a[j] - Ben.a[j])) * (1+ i.a[j]) # use actual returns for assets
  
  df$UL.e[j+1] <- with(df, AL.e[j+1] - AA.e[j+1]) # 2nd term of eq 7.2b - expected UL at start of next year
  df$UL.a[j+1] <- with(df, AL.a[j+1] - AA.a[j+1]) # eq 7.1
  df$dUL.a[j] <- with(df, UL.a[j+1] - (UL.a[j] + NC.a[j] - Cont.a[j])*(1+i))  # eq 7.5 difference in UL.a developed during the year
}

# compute measures of interest
df %<>% mutate(FR.a=AA.a / AL.a * 100, # funded ratio
               Contpct.a=Cont.a / pay.a * 100,
               Benpct.a=Ben.a / AA.a * 100,
               xcfpct=(Cont.a - Ben.a) / AA.a * 100)
ht(df)
qplot(year, FR.a, data=df, geom=c("point", "line")) + geom_hline(y=100)
qplot(year, Contpct.a, data=df, geom=c("point", "line")) + geom_hline(y=NCpct*100)
qplot(year, Benpct.a, data=df, geom=c("point", "line")) + geom_hline(y=df$Benpct.a[1])
qplot(year, xcfpct, data=df, geom=c("point", "line")) + geom_hline(y=df$xcfpct[1])

df
  

set.seed(12)
alg <- .07 # actuarial liability growth
ag.gm <- .07 # expected asset growth, geometric mean - need not be same as liability growth
ag.am <- .087 # expected asset growth, arithmetic mean, make it larger to offset anticipated volatility drag so that .gm of result is similar
ag <- c(0, rnorm(nyears-1, ag.am, sd=.1)) # asset growth, actual

# check:  compare expected assets in terminal year to liability in terminal year
prod(1+ag) - (1+alg)^(nyears-1) # pretty close when alg=.07, ag.am=.087, nyears=50, seed=12

df <- 
  data.frame(year=1:nyears) %>%
  mutate(AL=AL0*(1+alg)^(year-1), # actuaries usually use AAL for actuarial accrued liability, rather than AL
         AA=assets0*cumprod(1+ag), # actuarial assets
         AA.e=assets0*(1+ag.gm)^(year-1),
         UL=AA - AL) %>%   # Winklevoss uses UL; actuaries usually use UAAL for unfunded actuarial accrued liability
  mutate()
df

df <- data.frame(year=0:nyears, AL=NA, AA=NA, NC=NA, B=NA) 
df <- within(df, {
  AL[1] <- AL0
  AA[1] <- assets0
  NC[1] <- 10
  B[1]  <- 8
  })
df




