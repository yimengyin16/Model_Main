
#**************************************
#    1. PV of Annuities           #####
#**************************************

# 1.1 function calculating temporary annuity values from age x to retirment age (fixed end)
get_tla <- function(px, i, scale = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN. 
  # The length of px is f - a1 
  # Note that the last element is redundant, just used as a place holder. 
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
  # i:  discount rate, scalar
  # scale: how the annuity scale up over time. eg: 
  #        1) salary scale. default is a n vector of 1, meaning no salary scale. used when calculating career based annuity
  #        2) simple COLA scale: COLA increasing at a fixed percentage very year.  
  # cashflow: The cashflow of the annuity can be specified by this argument. This is useful when calculating PV of benefits with COLA.
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr <-  1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- scale[j:n]/scale[j]                # scale
    tla[j] <-  sum(SS * v * pxr)                # computing annuity value at j
  } 
  return(tla)
}

# microbenchmark(
# get_tla(rep(0.98, 55), 0.08) # test the function
# )


# 1.2 function calculating temporary annuity values from a fixed entry age y to x (fixed start)
get_tla2 = function(px, i, sx = rep(1, length(px))){
  # Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
  # This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 
  
  # Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
  # For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
  # eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
  # eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
  # Note that the last 2 survival rates and last salary scale are redundant in the calculation, they are just used as place holders. 
  #(calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
  # because the last annuity payment is paid at the begining of 64. )
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n. The minimum length of px allowed is 2. 
  # i:  discount rate, scalar
  # sx: salary scale. default is an n vector of 1, meaning no salary scale. 
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla = numeric(length(px))
  n = length(tla)
  
  # tla[1] will be kept as 0, next calculate tla[2:n]:
  for(j in 1:(n - 1)){
    v   <- 1/(1 + i)^(0:(j - 1))                                  # dicount vector
    if(j == 1) pxr <- 1 else pxr <- cumprod(c(1, px[1:(j - 1)]))  # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- sx[1:j]/sx[1]                                          # salary scale
    tla[j + 1] <-  sum(SS * v * pxr)                              # computing annuity value at j;
  } 
  return(tla) 
}

# 1.2a A simpler implementation of 1.2
get_tla2a <- function(px, i, sx = rep(1, length(px))){
  
  n <- length(px)
  tla <- numeric(n)
  v <- 1/(1 + i)
  
  tla[-1] <- cumsum(cumprod(c(1,px[1:(n-2)])* v * sx[1:(n - 1)]/sx[1])/v)
  
  return(tla)   
 }

# microbenchmark(
# get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65)),  # test the function
# get_tla2a(rep(0.98, 65), 0.08, rep(1.1, 65))# test the function
# )

# 1.3 PVFB of term costs
get_PVFB <- function(px, v, TC){ # present values of subsets of TC (fixed end)
  # This function compute the total present value of TC[j:n] at the beginning of time j, with j running from 1 to n. 
  # The function can be used to calculate PVFB of term costs of ancillary benefits or retirement benefits with multiple
  # retirement ages. 
  
  # Inputs
    # px: numeric vector of length n. Probability of survival at time 1 through n
    # v : numeric. discount factor 1/(1 + i)
    # TC: numeric vector of length n. A series of term costs. Term costs are valued at the begninning of period. 
  # Returns
    # PVFBs of fixed end contracting windows of TC. 
  
  n <- length(px)
  
  PVFB <- sapply(seq_len(n), function(j) ifelse(j == n, TC[j], sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n], na.rm = TRUE)))
  
  return(PVFB)
}


# microbenchmark(
# get_PVFB(rep(0.98, 65), 0.08, rep(1.1, 65))
# )

# 1.4 NC of UC and PUC
get_NC.UC <- function(px, v, TC){
  # This function is a variation of get_PVFB. It is used to calculate NC under UC and PUC methods.
  # Below we explain the major difference between get_NC.UC and get_PVFB:
    # 1. Why TC[(j + 1):n]?  Remember NC is the discounted value of benefit accrual. During age x, the individual can 
    #    accrue benefit for age x + 1 to r'', so the corresponding elements in TC are TC[(j + 1):n]. Note that 
    #    TC[j+1] is gx.r(j+1)*qxr(j+1)*ax(j+1) in PUC. 
    # 2. Why start discounting from the 1st element? Since at j the individual starts accruing benefit from j + 1, 
    #    we need to discount the value in j + 1.  
  # Note The last elements (at age r'') of the result is NA by construction. 
  # px must be survival probability from min(age) to r''.
  # TC must be defined as  
    #  UC for retirement:    gx.r(x) * qxr(x) * ax(x), x running from y (entry age) to r'' (eg. 20 to 65 in Winklevoss book)
    #  PUC for retirement:   Bx(x)/(x - y) * gx.r(x) * qxr(x) * ax(x), x running from entry age (y) to r'' (0 when x = y)      
    #  PUC for vested terms: Bx(x)/(x - y) * gx.r(x) * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max]
  n <- length(px) # n is r''
  
  Fun_NC <- function(j) ifelse(j == n, NA, sum(cumprod(px[j:(n - 1)]) * v^(1:(n-j)) * TC[(j + 1):n]))
  
  NC <- sapply(seq_len(n), Fun_NC)
  
  return(NC)
}

# microbenchmark(
# get_NC.UC(rep(0.98, 65), 0.08, rep(1.1, 65))
# )

# 1.5 AL of PUC
get_AL.PUC <- function(px, v, TC){
  # This function is a variation of get_PVFB. It is used to calculate AL under PUC methods.
  
  # Note that the only difference between get_AL.PUC and get_PVFB is that TC[j] is multiplied by (j - 1)
  
  # Note that y(entry age) corresponds to index 1 and age x corresponds to index j, so at age x the individual
  # has been accruing benefits for x - y years, which is equal to j - 1 years. (eg. Assuming y = 20, then when x = 21 and j = 2 the 
  # individual have accrued benefits for 1 year (x - y = 21 - 20 and j - 1 =  2 - 1).
  
  # TC must be defined the same way as in get_NC.UC. 
  # the first element (age y) should be zero, the last element should be the same as the last element in TC.
  
  n <- length(px) # n is r'' - y + 1
  AL <- sapply(seq_len(n), 
                 function(j) ifelse(j == n, TC[j]*(j-1), sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n] * (j - 1)))
                 )
  return(AL)
}


# microbenchmark(
# get_AL.PUC(rep(0.98, 65), 0.08, rep(1.1, 65))
# )


#**************************************
# 2. Amortization Functions       #####
#**************************************

pmt <- function(p, i, n, end = FALSE){
  # amortization function with constant payment at each period 
  # p = principle, i = interest rate, n = periods. 
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i)
  a_n <- (1 - (1 + i)^(-n))/(1 - 1/(1 + i))
  pmt <- p / a_n
  return(pmt)  
}

 pmt(100, 0.02, 10)
# pmt2(100, 0.08, 10, TRUE)
# pmt2(-100, 0.08, 10)

gaip <- function(p, i, n, g, end = FALSE){
  # p=principal, i=interest rate, n=periods, g=growth rate in payments
  # calculating gaip directly
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i) 
  k <- (1 + g)/(1 + i)
  a_sn <- (1 - k^n )/(1 - k)
  pmt <- p/a_sn
  return(pmt)
}

# gaip(100, 0.10, 10, 0.04)
# gaip3(100, 0.08, 10, 0.02, end = TRUE)



# Constant dollar amortization method
amort_cd <- function(p, i, m, end = FALSE) rep(pmt(p, i, m, end), m)

# Constant percent amortization method
amort_cp <- function(p, i, m, g, end = FALSE) gaip(p, i, m, g, end)*(g + 1)^(1:m - 1)

# Strait line method #
amort_sl <- function(p, i, m, end = FALSE){
  # Straitline amortization method
  # See Winklevoss(1993, p101)
  if(end){
    sl <- i*(p - p*(0:(m - 1))/m) + p/m
  } else {
    d <- 1/(1+i)
    sl <- d*(p - p*(1:m)/m) + p/m}
  return(sl)
  }
    
# Test the functions
# amort_cd(100, 0.08, 10, F)
# amort_cp(100, 0.08, 10, 0.05, F)
# amort_sl(100, 0.08, 10, F)


# Function for choosing amortization methods
amort_LG <- function(p, i, m, g, end = FALSE, method = "cd"){
  # amortize the gain/loss using specified amortization method
  switch(method,
         cd = amort_cd(p, i ,m, end),
         cp = amort_cp(p, i, m, g, end),
         sl = amort_sl(p, i, m, end)
         )
  }


#********************************
# 3.Utility functions        ####
#********************************
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA

ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail

memory<-function(maxnobjs=5){
  # function for getting the sizes of objects in memory
  objs<-ls(envir=globalenv())
  nobjs<-min(length(objs),maxnobjs)
  tmp<-as.data.frame(sapply(objs, function(x) object.size(get(x)))/1048600)
  tmp<-data.frame(name=row.names(tmp), sizeMB=tmp[,1])
  tmp<-tmp[order(-tmp$sizeMB),]
  tmp$sizeMB<-formatC(tmp$sizeMB,format="f",digits=2,big.mark=",",preserve.width="common")
  print(paste("Memory available: ",memory.size(NA),sep=""))
  print(paste("Memory in use before: ",memory.size(),sep=""))
  print("Memory for selected objects: ")
  print(head(tmp,nobjs))
  print(gc())
  print(paste("Memory in use after: ",memory.size(),sep=""))
}

#na2zero <- function(x){x[is.na(x)] <- 0 ;return(x)}
na2zero <- function(x){replace(x, is.na(x), 0)}


f2n <- function(x) {
  if(is.numeric(x)|is.integer(x)) x else
    if(!is.factor(x)) stop("Not a factor") else
    as.numeric(levels(x)[x])
}
  
  
  
#f2n2 <- function(x) as.numeric(as.character(factor(x))) # much slower than f2n

get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1


## spline smoothing 
splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
  # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
  # or just 2 columns, with no nonfillvar
  # last column ALWAYS must be the value var
  valvar<-names(df)[length(names(df))]
  nonfillvar<-setdiff(names(df),c(fillvar,valvar))
  f<-function(x) {
    if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
    spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
    dfout<-data.frame(x=spl$x, y=spl$y)
    names(dfout)<-c(fillvar,valvar)
    return(dfout)
  }
  if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
  return(dfl2)
}





## Functions for model control
assign_parmsList <- function(paramlist, excludes = NULL, ...){
  varNames   <- setdiff(names(paramlist), excludes)
  assign_var <- function(x) assign(x, paramlist[[x]], ...)
  sapply(varNames, assign_var)
}



get_parmsList <- function(rundf, runname) { # don't exclude anything
  # Assign the data from the spreadsheet for a single runname to a list. We'll pass the list to the model.
  runlist <- as.list(rundf[which(rundf$runname==runname), ])
  return(runlist)
}



trans_cont <- function(cont, run){
  # Transform the user-defined contribution table to "long" form for the selected "run".
    # Before transformation: start, duration, pct_ADC;
    # After transformation:  year, pct_ADC.
  df_cont <- function(start, duration, pct) data.frame(year = start + 0:(duration - 1), pct_ADC = pct)
  df <- with(cont %>% filter(runname == run), 
             mapply(df_cont,
                    start = start, duration = duration, pct = pct_ADC, SIMPLIFY = FALSE)) %>% 
    bind_rows
  return(df)
}  



create_returns <- function(r.mean, r.sd, period){
  # Create return series with time varying mean, sd.
  # Mean and sd in each period are given by "r.mean" and "r.sd". 
  # Length of each period is given by "period". 
  i.r <- unlist(mapply(rnorm, period, r.mean, r.sd)) %>% as.vector # when the length of the arguments is 1, need to convert the reusult to vector from a matrix 
}



# rolling window return

get_rollingReturns <- function(returnSeries, rolling_type = c("moving", "expanding"), window){
  # calculate moving window or expanding win dow geometric mean return for a return series. 
  window_width <- switch(rolling_type,
                         moving = window,
                         expanding = seq_along(returnSeries))
  
  rollingReturn <- zoo::rollapply(returnSeries, width = window_width, get_geoReturn, fill = NA, align = "right")
  return(rollingReturn)
}



getcell <- function(file, sheet, cell) {
  require(XLConnect)
  value <- readWorksheetFromFile(file, sheet=sheet, header=FALSE, region=cell, colTypes="character")
  return(as.character(value))
}


xlrange <- function(file, sheet, cell1, cell2) {
  startcell <- getcell(file, sheet, cell1)
  endcell   <- getcell(file, sheet, cell2)
  range     <- paste0(startcell, ":", endcell)
  return(range)
}

read_ExcelRange <- function(file, sheet, cellStart = "B2", cellEnd = "B3", ...){
  require(XLConnect)
  range <- xlrange(file, sheet, cellStart, cellEnd)
  readWorksheetFromFile(file, sheet = sheet, header=TRUE, region=range, ...)
}



#**********************************************
#  4. Functions for analyzing results        ####
#**********************************************

get_quantiles <- function( runName,     # character
                           varName,     # character
                           data = results_all,
                           year.max = 100,
                           qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
  
    # runName = c("R4F1")     # character
    # varName = "FR"     # character
    # data = results_all
    # year.max = 100
    # qts = c(0.1, 0.25, 0.5, 0.75, 0.9)

  
    # runName = "C.ADC_r7.25"     # character
    # varName = "FR.MA"     # character
    # data = penSim_results
    # year.max = 100
    # qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
    # 
  
  df <- data %>% filter(runname %in% runName, sim >= 1) %>%  
    select_("runname",  "sim","year", varName) %>% spread_("year", varName)
  
  fn <- function(df) { 
    df_q <- sapply(select(df, -sim, -runname), function(x) quantile(x, qts, na.rm = TRUE)) %>% as.data.frame
    
    df_q %<>% mutate(Quantile = rownames(df_q)) %>% gather(year, Value, -Quantile) %>%
      
      mutate(#year = f2n(year),
             Quantile = factor(Quantile)) %>% filter(year <= year.max)
    
    df_q %<>% spread(Quantile, Value)
  }
  

  df <- ldply(split(df, df$runname), fn, .id = "runname")
  
  return(df)
  
}


# get_quantiles2 <- function(varName,     # character
#                            data,
#                            qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
# 
# 
#   varName = "maxChg5y"     # character
#   data    = maxChg5y
#   qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
#   
# 
#   
#   df <- select_(data, "sim", , varName) %>% spread_("year", varName)
#   
#   fn <- function(df){ 
#     df_q <- sapply(select(df, -sim), function(x) quantile(x, qts, na.rm = TRUE)) %>% as.data.frame
#     
#     df_q %<>% mutate(Quantile = rownames(df_q)) %>% gather(year, Value, -Quantile) %>%
#       
#     mutate(#year = f2n(year),
#            Quantile = factor(Quantile)) %>% filter(year <= year.max)
#     
#     df_q %<>% spread(Quantile, Value)
#   }
#   
#   df <- fn(df)
# }




draw_quantiles  <- function(runName,     # character
                            varName,     # character
                            data = results_all,
                            year.max = 80,
                            qts = c(0.1, 0.25, 0.5, 0.75, 0.9),
                            ylim = NULL,
                            EEC_line = 5){
  
#   runName <- c("D1F075-average_gn2","D1F075-average")     # character
#   varName <- c("C_PR")     # character
#   data = results_all
#   year.max = 80
#   qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
#   ylim = NULL
#   EEC_line = 5

  col1 <- colorRampPalette(c("darkgreen","yellowgreen", "dodgerblue4", "orangered", "red4"))
  
  if (varName %in% c("C_PR","ERC_PR")) color_values <- rev(col1(length(qts))) else
                                       color_values <- col1(length(qts))
  
  df_q <- get_quantiles(runName = runName, 
                        varName = varName,
                        data    = data,
                        year.max = year.max,
                        qts = qts)  %>% 
    gather(Quantile, Value, -runname, -year) %>% 
    mutate(Quantile = factor(Quantile, levels = paste0(sort(qts, decreasing = TRUE) * 100, "%")))
  
  
  
  plot_q <- 
    ggplot(df_q, aes(x = year, y = Value, color = Quantile)) + theme_bw() + 
    geom_point(size = 1.5) + geom_line()+ 
    labs(y = varName, title = paste0("Quantile plots of ", varName)) 
    # scale_color_manual(values = color_values)
  

  
  if (varName %in% c("C_PR","ERC_PR")) {
    df_NC.rate <- data %>% filter(runname %in% runName, sim == 1, year <= year.max) %>% select(runname, year, NC_PR) %>% 
                  mutate(Quantile = "Normal Cost Rate",
                         runname = factor(runname, levels = runName)) %>% 
                  rename(Value = NC_PR)
    plot_q <- plot_q + geom_line(data = df_NC.rate, aes(x = year, y = Value), linetype = 1)+ scale_color_manual(values = c(color_values, "black")) 
  } else {
    plot_q <- plot_q + scale_color_manual(values = color_values)
  }
    
    
  if(length(runName) > 1) plot_q <- plot_q + facet_grid(. ~ runname)
  if(!is.null(ylim))      plot_q <- plot_q + coord_cartesian(ylim = ylim)
  if(varName == "FR")     plot_q <- plot_q + geom_hline(yintercept = 100,     color = "black", linetype = 2)
  if(varName == "C_PR")   plot_q <- plot_q + geom_hline(yintercept = EEC_line,color = "black", linetype = 2)

  list(df = df_q, plot = plot_q)
}




draw_quantiles2  <- function(runName,     # character
                            varName,      # character
                            data = results_all,
                            year.max = 80,
                            qts = c(0.1, 0.25, 0.5, 0.75, 0.9),
                            ylim = NULL){
  
  df_q <- get_quantiles(runName = runName, 
                        varName = varName,
                        data    = data,
                        year.max = year.max,
                        qts = qts)  %>% 
    gather(Quantile, Value, -runname, -year)
  
  plot_q <- 
    ggplot(df_q, aes(x = year, y = Value, color = Quantile)) + theme_bw() + 
    geom_point(size = 1.5) + geom_line()+ 
    labs(y = varName, title = paste0("Quantile plot of ", varName, " in ", runName))
  
  if(length(runName) > 1) plot_q <- plot_q + facet_wrap( ~ runname) 
  if(!is.null(ylim)) plot_q <- plot_q + coord_cartesian(ylim = ylim)
  
  plot_q
}






get_metrics <- function(runs,  year.max, plan_AL, data = results_all ){
  
#     runs = runs_investment
#     #runs = "I6F075-5"
#     year.max = 30
#     include.maxChg = FALSE
#     plan_AL = "I6F075-1" # any plan with 7.5% discount rate will do.
#   
  AL_7.5_v <- results_all %>% filter(runname == plan_AL, sim == 1, year <= year.max) %>% select(AL) %>% unlist
    
  df_TO <- results_all %>% filter(runname %in% runs, year <= year.max, sim > 0) %>%
           arrange(runname, sim, year) %>% 
           group_by(runname, sim) %>% 
           mutate(AL_7.5 = AL_7.5_v,
                  FR_MA_7.5 = 100 * MA / AL_7.5) %>% 
           select(runname, year, sim, AL, AL_7.5, MA, FR_MA, FR_MA_7.5, ERC_PR, C_PR, C, ERC, PR)  
  
  ## Measures of funded status *********************************************
  
  # 1. Probability of funded ratio falling below X% in 5, 15, 30 years.  
  # 2. VaR-like measure:  5th percentile of funded ratio in year 5, 15, and 30.
  # 3. CVaR-like measure: weighted average of funded ratio below 5th percentile in year 5, 15, and 30. 
  #                       question: how to calculate the weight?  
  
  
  
  df_ruin <- 
    df_TO %>% group_by(runname, sim) %>% 
    mutate(FR50 = cumany(FR_MA  <= 50),
           FR40 = cumany(FR_MA  <= 40),
           FR50_7.5 = cumany(FR_MA_7.5  <= 50),
           FR40_7.5 = cumany(FR_MA_7.5  <= 40)) %>% 
    filter(year %in% c(15,30,40)) %>% 
    ungroup %>% group_by(runname, year) %>% 
    summarise(FR50 = 100 * sum(FR50)/n(),
              FR40 = 100 * sum(FR40)/n(),
              FR50_7.5 = 100 * sum(FR50_7.5)/n(),
              FR40_7.5 = 100 * sum(FR40_7.5)/n()) %>% 
    gather(variable, value, -runname, -year) %>% 
    mutate(variable = paste0(variable, "_y", year),
           year = NULL) %>% 
    spread(variable, value)
  
  df_ruin %>% kable(digits = 3)
  
 
  
  ## Measures of contribution volatility *************************************
  
  df_sd <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>% 
    group_by(sim, runname) %>% 
    summarise(C_PR.sd = sd(C_PR, na.rm = TRUE), ERC_PR.sd = sd(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(median)) %>% select(-sim)
  df_sd
  
  
  df_dsd <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>% 
    group_by(sim, runname) %>% 
    summarise(C_PR.dsd = sd(diff(C_PR), na.rm = TRUE), ERC_PR.dsd = sd(diff(ERC_PR), na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(med = median(., na.rm = TRUE), q75 = quantile(., 0.75,na.rm = TRUE), q90 = quantile(., 0.9, na.rm = TRUE)), -sim)
    # summarise_each(funs(median)) %>% select(-sim)
  df_dsd
  
  
  df_maxC <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%  
    group_by(sim, runname) %>% 
    summarise(C_PR.max = max(C_PR, na.rm = TRUE), ERC_PR.max = max(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9)), -sim)
    # summarise_each(funs(median)) %>% select(-sim)
  df_maxC
  
  df_minFR <- df_TO %>%
    select(runname, year, sim, FR_MA, FR_MA_7.5) %>%  
    group_by(sim, runname) %>% 
    summarise(FR_MA.min = min(FR_MA),
              FR_MA_7.5.min = min(FR_MA_7.5)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(med = median, q25 = quantile(., 0.25), q10 = quantile(., 0.1)), -sim)
  # summarise_each(funs(median)) %>% select(-sim)
  df_minFR
  
  
  
  df_final_C <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%   
    group_by(sim, runname)    %>% 
    filter(year == max(year)) %>%
    rename(C_PR.final = C_PR,
           ERC_PR.final = ERC_PR) %>% 
    group_by(runname) %>%
    summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9)), -sim)
#     summarise(C_PR.final   = median(C_PR), 
#               ERC_PR.final = median(ERC_PR))
    df_final_C

    df_final_FR <- df_TO %>%
      select(runname, year, sim, FR_MA, FR_MA_7.5) %>%   
      group_by(sim, runname)    %>% 
      filter(year == max(year)) %>%
      rename(FR_MA.final = FR_MA,
             FR_MA_7.5.final = FR_MA_7.5) %>% 
      group_by(runname) %>%
      summarise_each(funs(med = median, q25 = quantile(., 0.25), q10 = quantile(., 0.1)), -sim, -year)
    #     summarise(C_PR.final   = median(C_PR), 
    #               ERC_PR.final = median(ERC_PR))
    df_final_FR
    
    
  df_5yearChg <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%   
    group_by(sim, runname)  %>% 
    mutate_each(funs(. - lag(., 5)), one_of(c("C_PR", "ERC_PR")) ) %>% 
    summarise(C_PR.5yChg   = max(C_PR,   na.rm = TRUE),
              ERC_PR.5yChg = max(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9) ))  %>% 
    select(-starts_with("sim"))
  
  df_5yearChg
  
  
  df_pctChg <- df_TO %>%
    select(runname, year, sim, C_PR, ERC_PR) %>%   
    group_by(sim, runname)  %>% 
    mutate_each(funs(. / lag(.) - 1  ), one_of(c("C_PR", "ERC_PR"))) %>% 
    summarise(C_PR.pctChg   = 100 * median(C_PR,   na.rm = TRUE),
              ERC_PR.pctChg = 100 * median(ERC_PR, na.rm = TRUE)) %>% 
    group_by(runname) %>% 
    summarise_each(funs(med = median(., na.rm = TRUE), q75 = quantile(., 0.75, na.rm = TRUE), q90 = quantile(., 0.9, na.rm = TRUE)), -sim) 
  df_pctChg
  
  
  
  # with(df_5yearChg, df_5yearChg[runname == "A1F075_O30pA5","C_PR.5yChg"] %>% unlist) %>% hist(breaks = 50)
  
  ## Present Value of contribution *************************************
  
  df_PVC <- df_TO %>% 
    select(runname, year, sim, C, ERC, PR) %>% 
    group_by(sim, runname) %>% 
    mutate(discount = 1/(1 + 0.075)^(year - 1),
           discount_L10 = ifelse(max(year) - year >= 10, 0, discount)) %>% 
    summarise(PV.C   = sum(C * discount),
              PV.ERC = sum(ERC * discount),
              PV.PR  = sum(PR * discount),
              PV.C_L10   = sum(C * discount_L10),
              PV.ERC_L10 = sum(ERC * discount_L10),
              PV.PR_L10  = sum(PR * discount_L10)) %>% 
    group_by(runname) %>% 
    mutate(PV.C_PR   = 100 * PV.C / PV.PR,
           PV.ERC_PR = 100 * PV.ERC / PV.PR,
           PV.C_PR_L10   = 100 * PV.C_L10 / PV.PR_L10,
           PV.ERC_PR_L10 = 100 * PV.ERC_L10 / PV.PR_L10 ) %>% 
    summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9)), -sim) 
  
  df_PVC
  
  df_year1 <- df_TO %>%
    select(runname, year, sim, FR_MA, FR_MA_7.5, ERC_PR) %>%  
    filter(year == 1, sim == 1) %>%
    rename(FR_MA.y1 = FR_MA,
           FR_MA_7.5.y1 = FR_MA_7.5,
           ERC_PR.y1 = ERC_PR)
  df_year1 
  
  df_metrics <- join_all(list(df_ruin, 
                              df_PVC,
                              df_sd,
                              df_dsd,
                              df_maxC,
                              df_minFR,
                              df_final_C,
                              df_final_FR,
                              df_5yearChg,
                              df_pctChg,
                              df_year1))
  
  return(df_metrics)
  
}


get_metrics_maxChg <- function(runs,  year.max, data = results_all){
# This function only calculate 5-year and 10-year max change of ERC rate.
  
  #     runs = runs_investment
  #     year.max = 30
  #     include.maxChg = FALSE
  #     prefix = "I1F075-"
  
  df_TO <- results_all %>% filter(runname %in% runs, year <= year.max, sim > 0) %>% 
    select(runname, year, sim, FR_MA, ERC_PR, C_PR, C, ERC, PR)  
  
  ## Measures of funded status *********************************************


  

    ## Create functions to calculate max changes in 5-year intervals. 
    maxChgWithin <- function(y, fn, ...){
      # max/min change within a single interval.
      zoo::rollapply(y, rev(seq_along(y)), function(x) fn(x - x[1], ...), fill = NA, align = "left") %>% fn(., ...)
      #y <- outer(x, x, "-")
      #y[lower.tri(y)] %>% fn(., ...)  
    }
    
    roll_maxChg <- function(x, fun, width,  ... ){
      # For a given vector x, calculate the max/min change WITHIN each interval of the width "width" 
      zoo::rollapply(x, width, maxChgWithin, fn = fun, ...,  fill = NA, align = "right")
    }
    
    
      df_5yearMaxChg <- df_TO %>%
        select(runname, year, sim, C_PR, ERC_PR) %>%
        group_by(sim, runname)  %>% 
        mutate(C_PR  = roll_maxChg(C_PR,  max, 5),
               ERC_PR= roll_maxChg(ERC_PR,max, 5)) %>% 
        summarise(C_PR.5yMaxChg  = max(C_PR,   na.rm = TRUE),
                  ERC_PR.5yMaxChg= max(ERC_PR, na.rm = TRUE)) %>% 
        group_by(runname) %>% 
        summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9) )) %>% 
        select(-starts_with("sim"))


      df_10yearMaxChg <- df_TO %>%
        select(runname, year, sim, C_PR, ERC_PR) %>%
        group_by(sim, runname)  %>% 
        mutate(C_PR  = roll_maxChg(C_PR,  max, 10),
               ERC_PR= roll_maxChg(ERC_PR,max, 10)) %>% 
        summarise(C_PR.10yMaxChg  = max(C_PR,   na.rm = TRUE),
                  ERC_PR.10yMaxChg= max(ERC_PR, na.rm = TRUE)) %>% 
        group_by(runname) %>% 
        summarise_each(funs(med = median, q75 = quantile(., 0.75), q90 = quantile(., 0.9) )) %>% 
        select(-starts_with("sim"))
  
  df_maxChg <- join_all(list(df_5yearMaxChg,
                             df_10yearMaxChg))

  return(df_maxChg)
}




maxChgWithin <- function(y, fn, ...){
  # max/min change within a single interval.
  zoo::rollapply(y, rev(seq_along(y)), function(x) fn(x - x[1], ...), fill = NA, align = "left") %>% fn(., ...)
  #y <- outer(x, x, "-")
  #y[lower.tri(y)] %>% fn(., ...)  
}

roll_maxChg <- function(x, fun, width,  ... ){
  # For a given vector x, calculate the max/min change WITHIN each interval of the width "width" 
  zoo::rollapply(x, width, maxChgWithin, fn = fun, ...,  fill = NA, align = "right")
}
























# pmt <- function(p, i, n){
#   # amortization function, with payment at the end of period. 
#   # p = principle, i = interest rate, n = periods. 
#   pmt <- p * (1 + i)^n * i/((1 + i)^n - 1)
#   return(pmt)  
# }

# gaip2 <- function(p, i, n, g){
#   # p=principal, i=interest rate, n=periods, g=growth rate in payments
#   # calculating gaip directly
#   # end: , if TRUE, payment at the end of period. 
#   #if(end) p <- p*(1 + i) 
#   k <- (1 + i)/(1 + g)
#   gaf <- (1 + i) * (1 - k)/(k * (k^(-n) - 1))
#   return(gaf*p)
# }




