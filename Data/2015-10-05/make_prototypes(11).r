
# Get actives, retirees, salary growth, and term rates.
# Create filled-in version of actives data.

# AZ PERS ppdid 6


#****************************************************************************************************
#                    Global constants ####
#****************************************************************************************************

dir <- "./data-raw/"
fn <- "Prototype inputs(13).xlsx"


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
library("knitr")
library("lubridate")
# devtools::install_github("hadley/ggplot2") # latest version arranges grobs
library("ggplot2")
library("readr")
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library("stringr")
library("zoo") # for rollapply
library("devtools")

library("bdata")
library("btools")


#****************************************************************************************************
#                    General tools ####
#****************************************************************************************************

splong <- function(df, fillvar, fitrange=NULL, method = "natural"){
  ## spline smoothing
  # df should have only 2 columns: fillvar, and valvar, where:
  #   valvar is the variable to be interpolated - the y value
  #   fillvar is the variable it corresponds to - the x value
  #   fitrange is the set of NEW x values (fillvar values) for which we will get NEW interpolated y values (valvar)
  # note the double-bracket indexing - i.,e., [[]] - a change from before. This ensures that the column selected is
  # created as a vector, which spline needs, rather than as a one-column data frame, to get around dplyr's tbl_df
  # example:  splong(df, "age", 20:40)
  valvar <- names(df)[length(names(df))]
  nonfillvar <- setdiff(names(df), c(fillvar, valvar))
  f <- function(x) {
    if(is.null(fitrange)) fitrange <- min(x[, fillvar]):max(x[, fillvar])
    spl <- spline(x[[fillvar]], x[[valvar]], xout=fitrange, method = method) # double brackets ensure we return a vector rather than 1-column dataframe
    dfout <- data.frame(x=spl$x, y=spl$y)
    names(dfout) <- c(fillvar,valvar)
    return(dfout)
  }
  dfl2 <- f(df)
  return(dfl2)
}

getcell <- function(dir, fn, sheet, cell) {
  require(XLConnect)
  value <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=FALSE, region=cell, colTypes="character")
  return(as.character(value))
}


xlrange <- function(dir, fn, sheet, cell1, cell2) {
  startcell <- getcell(dir, fn, sheet, cell1)
  endcell <- getcell(dir, fn, sheet, cell2)
  range <- paste0(startcell, ":", endcell)
  return(range)
}


getcuts <- function(df, cutvar) {
  # df has a numeric variable named age or ea - as in a data frame of actives or retirees
  # get lower bounds (lb) and upper bounds for each age or ea group (where cutvar is "age" or "ea")
  # cuts may not be perfect (compared to what we could figure by hand) but they're pretty good
  cutdf <- data.frame(stub=sort(unique(df[, cutvar]))) %>%
    mutate(stubdiff=stub - lag(stub))
  cutdf$lb <- NA; cutdf$ub <- NA
  cutdf$lb[1] <- min(cutdf$stub, na.rm=TRUE) -1; cutdf$ub[1] <- cutdf$stub[1]
  for(i in 2:(nrow(cutdf))) {
    cutdf$lb[i] <- cutdf$ub[i-1] + 1
    if(i<nrow(cutdf)) {
      cutdf$ub[i] <- cutdf$lb[i] + cutdf$stub[i+1] - cutdf$stub[i] - 1
      cutdf$ub[i] <- max(cutdf$ub[i], cutdf$stub[i])
    } else
      cutdf$ub[i] <- max(cutdf$lb[i], 80)
  }
  cutdf <- select(cutdf, -stubdiff)
  return(cutdf)
}


getactives <- function(plan, dir, fn) {
  planname <- toupper(plan)
  sheet <- paste0(plan, ".actives")

  range <- xlrange(dir, fn, sheet, "B2", "B3")
  df <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=TRUE, region=range, colTypes="character")

  # now make long file
  df2 <- df %>% filter(type %in% c("nactives", "salary"), !is.na(as.numeric(midage))) %>% # ensure that we exclude totals
    # select(-order, -agelb, -ageub, -agegrp) %>%
    select(type, midage, starts_with("X")) %>%
    gather(yos, value, -type, -midage) %>%
    mutate(age=as.integer(midage),
           yos=as.integer(gsub("[^0-9]", "", yos)),
           ea=as.integer(age - yos),
           # age.cell and ea.cell are convenient to have even though they are the same here
           age.cell=age,
           ea.cell=ea,
           value=cton(value),
           planname=planname) %>%
    select(planname, age, ea, age.cell, ea.cell, value, type) %>%
    spread(type, value) %>%
    filter(nactives>0)

  # get the cut points for age and ea
  # get cuts for age directly from the raw data
  agecuts <- df %>% select(stub=midage, lb=agelb, ub=ageub) %>%
    mutate_each(funs(as.numeric)) %>%
    filter(!is.na(stub)) %>%
    group_by(stub, lb, ub) %>%
    summarise(n=n()) %>%
    select(-n)

  eacuts <- getcuts(select(df2, ea), "ea")

  # prepare to return results
  lactives <- list()
  lactives$actives <- df2
  lactives$agecuts <- agecuts
  lactives$eacuts <- eacuts
  return(lactives)
}

# plan <- "AZ-PERS"
getretirees <- function(plan, dir, fn) {
  planname <- toupper(plan)
  sheet <- paste0(plan, ".retirees")
  range <- xlrange(dir, fn, sheet, "B2", "B3")
  benperiod <- getcell(dir, fn, sheet, "B4")
  benmult <- ifelse(benperiod=="month", 12, 1)

  df <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=TRUE, region=range, colTypes="character")

  # now make long file
  df2 <- df %>% filter(type %in% c("nretirees", "benefit"), !is.na(as.numeric(midage))) %>%
    select(type, age=midage, value=total) %>%
    mutate(age=as.integer(age),
           age.cell=age,
           value=cton(value),
           planname=planname) %>%
    spread(type, value) %>%
    mutate(benefit=cton(benefit) * benmult)

  agecuts <- df %>% select(stub=midage, lb=agelb, ub=ageub) %>%
    mutate_each(funs(as.numeric)) %>%
    filter(!is.na(stub)) %>%
    group_by(stub, lb, ub) %>%
    summarise(n=n()) %>%
    select(-n)

  # prepare to return results
  lretirees <- list()
  lretirees$retirees <- df2
  lretirees$agecuts <- agecuts

  return(lretirees)
}


getsalgrowth <- function(plan, dir, fn) {
  planname <- toupper(plan)
  sheet <- paste0(plan, ".salgrowth")
  range <- xlrange(dir, fn, sheet, "B2", "B3")
  df <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=TRUE, region=range, colTypes="character")
  # If data are on a yos basis convert to age basis (20:70) assuming entrance at 20
  df2 <- df %>% mutate(age=as.integer(20 + cton(yos) - 1),
                       rate=cton(rate)) %>% select(age, rate)
  maxage <- max(df2$age)
  lastrate <- df2$rate[df2$age==maxage]
  # simple fill-in: interpolate missing values, extrapolate last values
  df3 <- left_join(data.frame(age=20:80), df2) %>%
    arrange(age) %>%
    mutate(planname=planname,
           rate=na.approx(rate, na.rm="FALSE"),
           rate=ifelse(age>maxage, lastrate, rate)) %>%
    select(planname, age, salgrowth=rate)
  return(df3)
}


gettermrates <- function(plan, dir, fn) {
  planname <- toupper(plan)
  sheet <- paste0(plan, ".termrates")
  range <- xlrange(dir, fn, sheet, "B2", "B3")
  df <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=TRUE, region=range, colTypes="character")
  df2 <- df %>% mutate(yos=as.integer(cton(yos)), rate=cton(rate))
  maxyos <- max(df2$yos)
  lastrate <- df2$rate[df2$yos==maxyos]
  # simple fill-in: interpolate missing values, extrapolate last values
  df3 <- left_join(data.frame(yos=0:60), df2) %>%
    arrange(yos) %>%
    mutate(planname=planname,
           rate=na.approx(rate, na.rm="FALSE"),
           rate=ifelse(yos>maxyos, lastrate, rate)) %>%
    select(planname, yos, termrate=rate)
  return(df3)
}

checkactives <- function(adf) {
  print(paste0("tot nactives= ", sum(adf$nactives)))
  print(paste0("avg salary= ", sum(adf$nactives * adf$salary) / sum(adf$nactives)))
  print(paste0("avg age= ", sum(adf$nactives * adf$age) / sum(adf$nactives)))
}


make_plist <- function(planname, actives, retirees, salgrowth, termrates) {
  # make and save a list of dataframes for a single plan
  dir <- "./data-raw/"
  plist <- list()
  plist$actives <- actives
  plist$retirees <- retirees
  plist$salgrowth <- salgrowth
  plist$termrates <- termrates
  saveRDS(plist, paste0(dir, planname, ".rds"))
  return(NULL)
}


#****************************************************************************************************
#                    Tools for interpolating actives ####
#****************************************************************************************************
# adf %>% group_by(age, ea) %>% summarise(n=n()) %>% spread(ea, n) # show the grouping, in matrix form

fillin.actives <- function(lactives, sgdf, inflation) {
  # lactives: list for actives with actives data frame, agecuts df, and eacuts df
  # sgdf salary growth data frame
  adf <- select(lactives$actives, planname, age, ea, nactives, salary) # keep only the vars we want

  # agecuts <- getcuts(adf, "age")
  # eacuts <- getcuts(adf, "ea")
  agecuts <- lactives$agecuts
  eacuts <- lactives$eacuts

  planname <- paste0(adf$planname[1], ".fillin")

  # add group ranges to the actives data frame
  combo <- adf %>%
    mutate(pay=nactives*salary) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx],
           eaidx=findInterval(ea, eacuts$lb),
           ea.lb=eacuts$lb[eaidx],
           ea.ub=eacuts$ub[eaidx])

  # now create a "guess" dataframe spreading actives uniformly within groups and using average group salary
  # compute total pay, which we will target
  guessdf <- expand.grid(age=min(combo$age.lb):max(combo$age.ub), ea=min(combo$ea.lb):max(combo$ea.ub)) %>%
    filter(age>=ea) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           eaidx=findInterval(ea, eacuts$lb),
           age.cell=combo$age[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           ea.cell=combo$ea[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           nactives.cell=combo$nactives[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           salary.cell=combo$salary[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))]) %>%
    filter(!is.na(nactives.cell)) %>%
    arrange(ageidx, eaidx) %>%
    # group_by(ageidx, eaidx) %>%
    group_by(age.cell, ea.cell) %>%
    mutate(n.cell=n(),
           nactives=nactives.cell / n.cell,
           salary=salary.cell,
           pay=nactives * salary)

  # now impose the salary scale on employees - create a vector from 1 - 120 with scale relative to age 20
  # extend it backward to ages before 20 as we have some people in the data at earlier ages
  sgdf <- sgdf %>% arrange(age) %>% # just to be safe; also, note that we assume no gaps in years
    mutate(salgrowth=salgrowth-inflation) # we don't want to include inflation for this purpose

  sscale <- vector(length=120)
  sscale[min(sgdf$age):(min(sgdf$age) + nrow(sgdf))] <- c(1, cumprod(1 + sgdf$salgrowth)) # note that length grows by 1
  # extend to earlier years - from 10 to min age
  sscale[10:(min(sgdf$age)-1)] <- seq(0, 1, length=min(sgdf$age) - 10)

  # refine the guess by adjusting salary using the scale, and adjusting again to ensure that we hit the right
  # total pay in each group
  guessdf2 <- guessdf %>% mutate(adjsalary=salary * sscale[age] / sscale[age.cell],
                                 adjpay=nactives * adjsalary) %>%
    # group_by(ageidx, eaidx) %>%
    group_by(age.cell, ea.cell) %>%
    mutate(adjust=sum(pay) / sum(adjpay),
           adjsalary2=adjsalary*adjust,
           adjpay2=nactives*adjsalary2)

  adf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
    select(planname, age, ea, age.cell, ea.cell, nactives, salary=adjsalary2)
  return(adf.fillin)
}


fillin.actives.uniform <- function(lactives) {
  # lactives: list for actives with actives data frame, agecuts df, and eacuts df
  # sgdf salary growth data frame
  adf <- select(lactives$actives, planname, age, ea, nactives, salary) # keep only the vars we want

  # agecuts <- getcuts(adf, "age")
  # eacuts <- getcuts(adf, "ea")
  agecuts <- lactives$agecuts
  eacuts <- lactives$eacuts

  planname <- paste0(adf$planname[1], ".fillin")

  # add group ranges to the actives data frame
  combo <- adf %>%
    mutate(pay=nactives*salary) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx],
           eaidx=findInterval(ea, eacuts$lb),
           ea.lb=eacuts$lb[eaidx],
           ea.ub=eacuts$ub[eaidx])

  # now create a "guess" dataframe spreading actives uniformly within groups and using average group salary
  # compute total pay, which we will target
  guessdf <- expand.grid(age=min(combo$age.lb):max(combo$age.ub), ea=min(combo$ea.lb):max(combo$ea.ub)) %>%
    filter(age>=ea) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           eaidx=findInterval(ea, eacuts$lb),
           age.cell=combo$age[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           ea.cell=combo$ea[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           nactives.cell=combo$nactives[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           salary.cell=combo$salary[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))]) %>%
    filter(!is.na(nactives.cell)) %>%
    arrange(ageidx, eaidx) %>%
    # group_by(ageidx, eaidx) %>%
    group_by(age.cell, ea.cell) %>%
    mutate(n.cell=n(),
           nactives=nactives.cell / n.cell,
           salary=salary.cell,
           pay=nactives * salary)

  adf.fillin <- guessdf %>% mutate(planname=planname) %>%
    select(planname, age, ea, age.cell, ea.cell, nactives, salary)
  return(adf.fillin)
}


fillin.actives.spline <- function(lactives) {
  adf <- lactives$actives
  agecuts <- lactives$agecuts
  eacuts <- lactives$eacuts
  minage <- min(agecuts$lb)
  maxage <- max(agecuts$ub)
  planname <- paste0(adf$planname[1], ".fillin.spline")

  adf %>% select(age, ea, salary) %>% spread(ea, salary)
  adf %>% select(age, ea, nactives) %>% spread(ea, nactives)

  # get weighted average salary by age, then expand to all ages using spline
  wasdf <- adf %>% group_by(age) %>% summarise(salary=sum(nactives * salary, na.rm=TRUE) / sum(nactives, na.rm=TRUE))
  wasdf.s <- splong(wasdf, "age", minage:maxage)

  # add group ranges to the actives data frame
  combo <- adf %>%
    mutate(pay=nactives*salary) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx],
           eaidx=findInterval(ea, eacuts$lb),
           ea.lb=eacuts$lb[eaidx],
           ea.ub=eacuts$ub[eaidx])

  # now create a "guess" dataframe spreading actives uniformly within groups and using spline salary
  # compute total pay, which we will target
  guessdf <- expand.grid(age=minage:maxage, ea=min(combo$ea.lb):max(combo$ea.ub)) %>%
    filter(age>=ea) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           eaidx=findInterval(ea, eacuts$lb),
           age.cell=combo$age[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           ea.cell=combo$ea[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           nactives.cell=combo$nactives[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))],
           salary.cell=combo$salary[match(paste(ageidx, eaidx), paste(combo$ageidx, combo$eaidx))]) %>%
    filter(!is.na(nactives.cell)) %>%
    arrange(ageidx, eaidx) %>%
    group_by(age.cell, ea.cell) %>%
    mutate(n.cell=n(),
           nactives=nactives.cell / n.cell,
           salary=wasdf.s$salary[match(age, wasdf.s$age)],
           pay=nactives * salary)

  # now force the group total pay to match what is in the source data

  adf.fillin <- guessdf %>% mutate(planname=planname) %>%
    select(planname, age, ea, age.cell, ea.cell, nactives, salary)

  tmp <- bind_rows(mutate(select(adf, age, ea, nactives, salary), type="grouped"),
                   mutate(select(guessdf, age, ea, nactives, salary), type="splined")) %>%
    group_by(type, age) %>%
    summarise(salary=sum(nactives * salary, na.rm=TRUE) / sum(nactives, na.rm=TRUE))
  qplot(age, salary, data=tmp, colour=type, geom=c("line")) +
    geom_point(aes(size=type)) +
    scale_size_manual(values = c(3, 1)) # make the points for grouped values bigger


  tmp <- bind_rows(mutate(select(adf, age.cell, ea.cell, nactives, salary), type="grouped"),
                   mutate(select(guessdf, age.cell, ea.cell, nactives, salary), type="splined")) %>%
    group_by(type, age.cell, ea.cell) %>%
    summarise(pay=sum(nactives * salary, na.rm=TRUE), nactives=sum(nactives))
  tmp %>% spread(type, pay) %>% mutate(diff=splined - grouped, pdiff=diff / grouped * 100) %>% arrange(desc(abs(diff)))
  tmp %>% spread(type, pay) %>% mutate(diff=splined - grouped, pdiff=diff / grouped * 100) %>% arrange(desc(abs(diff)))
  tmp %>% group_by(type) %>% summarise(pay=sum(pay), nactives=sum(nactives))
  qplot(age, salary, data=tmp, colour=type, geom=c("line")) +
    geom_point(aes(size=type)) +
    scale_size_manual(values = c(3, 1)) # make the points for grouped values bigger


  tmp <- bind_rows(mutate(wasdf, type="grouped"), mutate(wasdf.s, type="splined"))
  qplot(age, salary, data=tmp, colour=type, geom=c("line")) +
    geom_point(aes(size=type)) +
    scale_size_manual(values = c(3, 1)) # make the points for grouped values bigger

  return(adf.fillin)
}


fillin.actives.spreadea.splineage <- function(lactives) {
  # salary:
  #   first spread uniformly within age.cell-ea.cell group (same salary for all)
  #   then for every ea, estimate salary for each age using a spline - adjust endpoints first for plausibility
  #   finally, adjust resulting salary within each age.cell-ea.cell proportionately to hit total payroll values from grouped data
  # nactives: spread uniformly within age.cell-ea.cell group (same nactives for all)
  adf <- lactives$actives
  agecuts <- lactives$agecuts
  eacuts <- lactives$eacuts
  minage <- min(agecuts$lb)
  maxage <- max(agecuts$ub)
  minea <- min(eacuts$lb)
  maxea <- max(eacuts$ub)

  planname <- paste0(adf$planname[1], ".fillin")

  # adf %>% select(age, ea, salary) %>% spread(ea, salary)
  # adf %>% select(age, ea, nactives) %>% spread(ea, nactives)

  # create a master grouped data frame
  adf.g <- adf %>% select(-planname, -age, -ea, nactives.cell=nactives, salary.cell=salary) %>%
    mutate(pay.cell=nactives.cell * salary.cell) %>%
    mutate(ageidx=findInterval(age.cell, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx],
           eaidx=findInterval(ea.cell, eacuts$lb),
           ea.lb=eacuts$lb[eaidx],
           ea.ub=eacuts$ub[eaidx]) %>%
    select(age.cell, ea.cell, age.lb, age.ub, ea.lb, ea.ub, nactives.cell, salary.cell, pay.cell)

  # expand the grouped data frame to all allowable age-ea combinations ####
  xpnd <- function(df) {
    df2 <- expand.grid(age=df$age.lb:df$age.ub, ea=df$ea.lb:df$ea.ub) %>%
      filter(age>=ea)
    return(df2)
  }

  adf.x <- adf.g %>% rowwise() %>%
    do(cbind(., xpnd(.))) %>%
    ungroup %>%  # get rid of rowwise
    group_by(age.cell, ea.cell) %>%
    mutate(n.cell=n()) %>%
    select(age, ea, everything()) %>%
    arrange(age, ea)


  # work with the expanded data ####

  # we have to anchor the endpoints with reasonable values BEFORE computing the spline
  adjustends <- function(age, salary) {
    # the basic idea is that if an endpoint is NA, insert a plausible value

    # simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
    # otherwise use the capped value
    firstnonna <- salary[which.min(is.na(salary))]
    lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
    bound <- .5
    firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
    lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
    cap <- function(sal, range) {
      cappedval <- max(sal, range[1])
      cappedval <- min(cappedval, range[2])
      return(cappedval)
    }

    salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
    salary.adjusted <- salary

    if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
    ilast <- length(salary)
    if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)

    return(salary.adjusted)
  }

  # test out adjustends
  # fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
  # # various salaries to try out
  # salary <- seq(20, 50, length.out = 10)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
  # salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
  # age <- 21:30
  # d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
  #   mutate(sal.spline=fs(age, salary),
  #          saladj.spline=fs(age, saladj))
  # d
  # qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)


  spline.y2 <- function(age, salary, safesalary) {
    # safesalary is what we use if salary has no data
    if(all(is.na(salary))) {
      print("AllNA")
      salary <- safesalary
    }
    salary.adjusted <- adjustends(age, salary)

    sp.out <- spline(age, salary.adjusted, xout=age)
    salout <- sp.out$y
    return(salout)
  }
  adf.x3 <- adf.x %>% group_by(age.cell, ea.cell) %>%
    mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
           salary.group=ifelse(age==age.cell & ea==ea.cell, salary.cell, NA),
           salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
    group_by(ea) %>%
    arrange(age) %>%
    mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
    group_by(age.cell, ea.cell) %>%
    mutate(planname=planname,
           pay.unadj=sum(salary.spline.adjep * nactives),
           adjust=pay.cell / pay.unadj,
           salary.final=salary.spline.adjep * adjust,
           pay.adj=sum(salary.final * nactives))

  return(adf.x3)
}


fillin.retirees <- function(lretirees) {
  rdf <- select(lretirees$retirees, planname, age, nretirees, benefit) # keep only the vars we want
  agecuts <- lretirees$agecuts

  planname <- paste0(rdf$planname[1], ".fillin")

  # add group ranges to the retirees data frame
  combo <- rdf %>%
    mutate(totben=nretirees * benefit) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx]) %>%
    arrange(age)

  # get avg benefits by age, via spline
  avgben <- splong(select(combo, age, benefit), "age", min(combo$age.lb):max(combo$age.ub))

  guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
    mutate(ageidx=findInterval(age, agecuts$lb),
           age.cell=combo$age[match(ageidx, combo$ageidx)],
           nretirees.cell=combo$nretirees[match(ageidx, combo$ageidx)],
           benefit.cell=combo$benefit[match(ageidx, combo$ageidx)]) %>%
    group_by(age.cell) %>%
    mutate(n.cell=n(),
           nretirees=nretirees.cell / n.cell, # spread nretirees evenly
           adjbenefit=avgben$benefit[match(age, avgben$age)], # get the spline-based avg benefit
           adjtotben=nretirees * adjbenefit)

  # refine the guess by adjusting ensure that we hit the right total benefits in each group
  guessdf2 <- guessdf %>% group_by(age.cell) %>%
    mutate(adjust=mean(nretirees.cell * benefit.cell) / sum(adjtotben),
           benefit=adjbenefit*adjust,
           totben=nretirees * benefit)

  rdf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
    select(planname, age.cell, age, nretirees, benefit)

  return(rdf.fillin)
}



#****************************************************************************************************
#                    Notes on files created ####
#****************************************************************************************************
# actives: df with planname, age, ea, nactives, salary, for AVAILABLE ages only
# retirees: df with planname, age, benefit (annual), nretirees, for AVAILABLE ages only
# salgrowth: df with planname, age, salgrowth, for ALL ages 20-70
# termrates: df with planname, age, termrate, for ALL ages 20-70
# actives_adjusted: df with planname, age, ea, nactives, salary, for ALL
#   ALLOWABLE ages and ea 20-70 for which values can be inferred

# interpolation and extrapolation:
# salgrowth and termrates: ages with missing values:
#   interpolated linearly if between available values
#   last nonmissing value is carried forward through age 70


#****************************************************************************************************
#                    Prototype retirement rates ####
#****************************************************************************************************

sheet <- "proto.retrates"
range <- xlrange(dir, fn, sheet, "B2", "B3")
df <- readWorksheetFromFile(paste0(dir, fn), sheet=sheet, header=TRUE, region=range, colTypes="character")

# now make long file
retrates <- df %>% gather(planname, retrate, -age) %>%
  mutate(retrate=cton(retrate), planname=as.character(planname), age=as.integer(age)) %>%
  filter(!is.na(retrate)) %>%
  select(planname, age, retrate)

saveRDS(retrates, paste0(dir, "protos.rds"))



#****************************************************************************************************
#                    AZ-PERS ####
#****************************************************************************************************
plan <- "AZ-PERS-6"

lactives <- getactives(plan, dir=dir, fn=fn)
adf.grouped <- lactives$actives

lretirees <- getretirees(plan, dir=dir, fn=fn)
rdf.grouped <- lretirees$retirees

sgdf <- getsalgrowth(plan, dir=dir, fn=fn)
trdf <- gettermrates(plan, dir=dir, fn=fn)

# fill in actives
# adf.fillin <- fillin.actives(lactives, sgdf, inflation=.0)
# adf.fillinx3 <- fillin.actives(lactives, sgdf, inflation=.03) %>% mutate(planname=paste0(planname, ".x3pct"))
# adf <- bind_rows(adf.grouped, adf.fillin, adf.fillinx3)
# adf.fillin <- fillin.actives(lactives, sgdf, inflation=.03)
# adf.fillinunif <- fillin.actives.uniform(lactives) %>% mutate(planname=paste0(planname, ".unif"))
adf.fillin <- fillin.actives.spreadea.splineage(lactives) %>%
  select(planname, age, ea, age.cell, ea.cell, nactives, salary=salary.final)

checkactives(adf.grouped)
checkactives(adf.fillin)

adf <- bind_rows(adf.grouped, adf.fillin)


# glimpse(adf.fillin)
# glimpse(adf.fillin2)
# glimpse(adf.fillinunif)

# fill in retirees
rdf.fillin <- fillin.retirees(lretirees)
rdf <- bind_rows(rdf.grouped, rdf.fillin)

make_plist(plan, actives=adf, retirees=rdf, salgrowth=sgdf, termrates=trdf) # save a file for this plan



#****************************************************************************************************
#                    LA-CERA-43 ####
#****************************************************************************************************
plan <- "LA-CERA-43"

lactives <- getactives(plan, dir=dir, fn=fn)
adf.grouped <- lactives$actives

lretirees <- getretirees(plan, dir=dir, fn=fn)
rdf.grouped <- lretirees$retirees

sgdf <- getsalgrowth(plan, dir=dir, fn=fn)
trdf <- gettermrates(plan, dir=dir, fn=fn)

# fill in actives
adf.fillin <- fillin.actives.spreadea.splineage(lactives) %>%
  select(planname, age, ea, age.cell, ea.cell, nactives, salary=salary.final)

checkactives(adf.grouped)
checkactives(adf.fillin)

adf <- bind_rows(adf.grouped, adf.fillin)


# fill in retirees
rdf.fillin <- fillin.retirees(lretirees)
rdf <- bind_rows(rdf.grouped, rdf.fillin)

make_plist(plan, actives=adf, retirees=rdf, salgrowth=sgdf, termrates=trdf) # save a file for this plan



#****************************************************************************************************
#                    OH-PERS-85 ####
#****************************************************************************************************
plan <- "OH-PERS-85"

lactives <- getactives(plan, dir=dir, fn=fn)
adf.grouped <- lactives$actives

lretirees <- getretirees(plan, dir=dir, fn=fn)
rdf.grouped <- lretirees$retirees

sgdf <- getsalgrowth(plan, dir=dir, fn=fn)
trdf <- gettermrates(plan, dir=dir, fn=fn)

# fill in actives
adf.fillin <- fillin.actives.spreadea.splineage(lactives) %>%
  select(planname, age, ea, age.cell, ea.cell, nactives, salary=salary.final)

checkactives(adf.grouped)
checkactives(adf.fillin)

adf <- bind_rows(adf.grouped, adf.fillin)

# fill in retirees
rdf.fillin <- fillin.retirees(lretirees)
rdf <- bind_rows(rdf.grouped, rdf.fillin)

make_plist(plan, actives=adf, retirees=rdf, salgrowth=sgdf, termrates=trdf) # save a file for this plan



#****************************************************************************************************
#                    WA-PERS2-119 ####
#****************************************************************************************************
plan <- "WA-PERS2-119"

lactives <- getactives(plan, dir=dir, fn=fn)
adf.grouped <- lactives$actives

lretirees <- getretirees(plan, dir=dir, fn=fn)
rdf.grouped <- lretirees$retirees

sgdf <- getsalgrowth(plan, dir=dir, fn=fn)
trdf <- gettermrates(plan, dir=dir, fn=fn)

# fill in actives
adf.fillin <- fillin.actives.spreadea.splineage(lactives) %>%
  select(planname, age, ea, age.cell, ea.cell, nactives, salary=salary.final)

checkactives(adf.grouped)
checkactives(adf.fillin)

adf <- bind_rows(adf.grouped, adf.fillin)

# fill in retirees
rdf.fillin <- fillin.retirees(lretirees)
rdf <- bind_rows(rdf.grouped, rdf.fillin)

make_plist(plan, actives=adf, retirees=rdf, salgrowth=sgdf, termrates=trdf) # save a file for this plan





#****************************************************************************************************
#                    Combine files (multiple plans) ####
#****************************************************************************************************
# Each prototype is a list with 4 data frames. Create 4 data frames, each of which has data for all prototypes.
plannames <- c("AZ-PERS-6", "LA-CERA-43", "OH-PERS-85", "WA-PERS2-119") # a vector of plannames

getplan <- function(plan) readRDS(paste0(dir, paste0(plan, ".rds")))
biglist <- llply(plannames, getplan)

actives <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$actives))
retirees <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$retirees))
salgrowth <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth))
termrates <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$termrates))
retrates <- readRDS(paste0(dir, "protos.rds"))


#****************************************************************************************************
#                    Now save the combined data frames ####
#****************************************************************************************************
use_data(actives, overwrite = TRUE)
use_data(retirees, overwrite = TRUE)
use_data(salgrowth, overwrite = TRUE)
use_data(termrates, overwrite = TRUE)
use_data(retrates, overwrite = TRUE)



#****************************************************************************************************
#                    Get the data and examine ####
#****************************************************************************************************
load("./data/actives.rda")
load("./data/retirees.rda")
load("./data/salgrowth.rda")
load("./data/termrates.rda")
load("./data/retrates.rda")

glimpse(actives)
glimpse(retirees)
glimpse(salgrowth)
glimpse(termrates)
glimpse(retrates)

count(actives, planname)
count(retirees, planname)
count(salgrowth, planname)
count(termrates, planname)
count(retrates, planname)

ht(count(salgrowth, age))
ht(count(termrates, yos))


count(actives, planname)

actives %>% filter(!str_detect(planname, "unif")) %>%
  group_by(planname, age) %>%
  summarise(salary=sum(salary * nactives) / sum(nactives)) %>%
  qplot(age, salary, data=., colour=planname, geom=c("point", "line"))

actives %>% filter(str_detect(planname, "fillin"), !str_detect(planname, "unif")) %>%
  group_by(planname, age) %>%
  summarise(nactives=sum(nactives)) %>%
  mutate(nactives.share=nactives / sum(nactives)) %>%
  qplot(age, nactives.share, data=., colour=planname, geom=c("point", "line"))

retirees %>% filter(!str_detect(planname, "unif")) %>%
  group_by(planname, age) %>%
  summarise(benefit=sum(benefit * nretirees) / sum(nretirees)) %>%
  qplot(age, benefit, data=., colour=planname, geom=c("point", "line"))

retirees %>% filter(str_detect(planname, "fillin"), !str_detect(planname, "unif")) %>%
  group_by(planname, age) %>%
  summarise(nretirees=sum(nretirees)) %>%
  mutate(nretirees.share=nretirees / sum(nretirees)) %>%
  qplot(age, nretirees.share, data=., colour=planname, geom=c("point", "line"))

salgrowth %>% qplot(age, salgrowth, data=., colour=planname, geom=c("point", "line"))

termrates %>% qplot(yos, termrate, data=., colour=planname, geom=c("point", "line"))

retrates %>% qplot(age, retrate, data=., colour=planname, geom=c("point", "line"))


# actives %>% filter(planname=="AZ-PERS.fillin", age>=40, age<=44) %>%
#   select(age, ea, nactives, salary) %>%
#   write_csv("e:/temp/check.csv")
#
# retirees %>% qplot(age, benefit, data=., colour=as.factor(planname), geom=c("point", "line"))
# retirees %>% write_csv("e:/temp/check.csv")



actives %>% filter(str_detect(planname, "AZ-")) %>%
  group_by(planname, age) %>%
  summarise(salary=sum(salary * nactives) / sum(nactives)) %>%
  qplot(age, salary, data=., colour=planname, geom=c("point", "line"))





#****************************************************************************************************
#                    Actives ####
#****************************************************************************************************
# result: df with planname age ea  nactives   salary
# read actives matrix - age x yos, and convert to long df with age, ea, nactives, salary,
# define the range - include column headers, but NOT row or column totals
# upper-left is row-name row and "order" column
# lower-right is last data row, last data column, NOT INCLUDE ANY TOTALS ROWS OR COLUMNS

# check total n actives and avg salary here to be sure we hit published numbers, before adjusting and scaling


#****************************************************************************************************
#                    Retirees ####
#****************************************************************************************************
# age, benefit (annual), nretirees, for available ages

#****************************************************************************************************
#                    Salary growth ####
#****************************************************************************************************
# planname, age, salgrowth, for all ages 20-70

#****************************************************************************************************
#                    Term rates ####
#****************************************************************************************************

#****************************************************************************************************
#                    OLD STUFF ####
#****************************************************************************************************







