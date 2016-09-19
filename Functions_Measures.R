

#### measure of contribution volatility ####
#**********************************************************

# What presures the political system?
# 1. ERC rate rises unexpectedly in a relatively short period of time. This threats budget planning.
# 2. ERC rate rises to a very high level, even through a relatively long period of time, that is unaffordable to the plan sponsor. 
#    This threats the affordability and may cause benefit cuts, tax increase, and crowding out expenditure on other public services.   

# Questions we may want to ask plan sponsor/policymakers:
# How big is the rise in ERC rate in a short period time that can threat budget planning?
# How big is the ERC rate that can threat the affordability?


# General measure of volatility
# - standard devation of year-to-year change in

# Measures of sharp rise of ERC rate in a short time period
# - max 5-year  change in ERC rate in each simulation, median over all simulations
# - max 10-year change in ERC rate in each simulation, median over all simulations
# - max deviation from 5/10 year moving average. For ERC rate and ERC
# - probability(over all simulations) of ERC rate rising by a% in 5/10 years, through year x  


# Measure of high ERC/ERC rate level
# - probability of ERC exceeding 2*NC in any of the years through year x
# - probability of ERC rate exceeding a% in any of the years through year x



  
  
  calc_FR.pctless  <- function(df_results, FR.pct = c(40, 60)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- df_results$runname[1]
    
    fn <- function(df_results, FR.pct.single){
      df_results %>% 
        group_by(sim) %>% 
        mutate(FR.pctless = cumany(FR_MA <= FR.pct.single)) %>% 
        select(sim, year, FR.pctless) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("FR.pctless" = paste0("FR", FR.pct.single,"less")))
    }
    
    sapply(FR.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_FR.pctmore  <- function(df_results, FR.pct = c(80, 95), rolling = FALSE){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- df_results$runname[1]
    
    fn <- function(df_results, FR.pct.single){
      df_results %>% 
        group_by(sim) %>% 
        mutate(rolling = rolling, 
               FR.pctmore = ifelse(rolling, cumany(FR_MA >= FR.pct.single),  (FR_MA >= FR.pct.single))) %>% 
        select(sim, year, FR.pctmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("FR.pctmore" = paste0("FR", FR.pct.single,"more")))
    }
    
    sapply(FR.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_ERCsharpRise <- function(df_results, ERC.pct = c(5, 10)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- df_results$runname[1]
    
    fn <- function(df_results, ERC.pct.single){
      
      df_results %>% 
        group_by(sim) %>% 
        mutate(ERC.ChgPts5y =  ERC_PR - lag(ERC_PR, 5),  # year1-5 change in pct points 
               ERC.ChgPts5y = na2zero(ERC.ChgPts5y),
               ERC.ChgPts5y.Xmore = cumany(ERC.ChgPts5y >= ERC.pct.single)) %>% 
        select(sim, year, ERC.ChgPts5y.Xmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("ERC.ChgPts5y.Xmore" = paste0("ERC.ChgPts5y.", ERC.pct.single,"more")))
    }
    
    sapply(ERC.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_highERC      <- function(df_results, ERC.pct = c(30, 50)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- df_results$runname[1]
    
    fn <- function(df_results, ERC.pct.single){
      
      df_results %>% 
        group_by(sim) %>% 
        mutate(ERC_PR.Xmore = cumany(ERC_PR >= ERC.pct.single)) %>% 
        select(sim, year, ERC_PR.Xmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("ERC_PR.Xmore" = paste0("ERC_PR.", ERC.pct.single,"more")))
    }
    
    sapply(ERC.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_qts  <- function(df_results, varname, qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
    
    # df_results <- results.stch 
    # qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
    # varname = "FR"
    # qts.single = 0.1
    
    runname <- df_results$runname[1]
    
    df_results %<>% select(runname, sim, year, one_of(varname)) %>% 
      dplyr::rename_("var" = varname)
    
    fn <- function(df_results, qts.single){
      
      df_results %>% 
        group_by(year) %>% 
        summarize(var = quantile(var, qts.single)) %>% 
        plyr::rename(c("var" = paste0(varname, ".q", qts.single)))
    }
    
    
    sapply(qts, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  get_df_results <- function(folderName, fileName){
    fileName <- paste0(folderName, fileName) 
    load(fileName)
    return(outputs_list$results)
    
  }

  get_measureList <- function(df_results){
    

    results.stch <- df_results %>% filter(sim > 0)
    
    runname <- results.stch$runname[1]
    
    prob.FR.pctless <- results.stch %>%  calc_FR.pctless
    prob.FR.pctmore <- results.stch %>%  calc_FR.pctmore(rolling = T)
    prob.ERCsharpRise <- results.stch %>%  calc_ERCsharpRise
    prob.ERChighERC <- results.stch %>%  calc_highERC
    FR.qts <- results.stch %>%  calc_qts("FR")
    ERC_PR.qts <- results.stch %>%  calc_qts("ERC_PR")
    
    
    assign(paste0("RiskMeasures_", runname), 
           list( 
             prob.FR.pctless = prob.FR.pctless,
             prob.FR.pctmore = prob.FR.pctmore,
             prob.ERCsharpRise = prob.ERCsharpRise,
             prob.ERChighERC = prob.ERChighERC,
             FR.qts  = FR.qts ,
             ERC_PR.qts = ERC_PR.qts))
    
    #  return(get(paste0("RiskMeasures_", runname)))
    
    # do.call(save, list(paste0("RiskMeasures_", runname), file=paste0(folderName, "RiskMeasures_", runname, ".RData")))
    
    #do.call( return, list(get(paste0("RiskMeasures_", runname))))
    return(get(paste0("RiskMeasures_", runname)))
    
  }
  
  
    
  # RiskMeasures_F50mature1_gn1 <- get_df_results("IO_M2.1_new/", "Outputs_D1F050-mature1_gn1.RData") %>%  get_measureList()
  # RiskMeasures_F50mature2_gn1 <- get_df_results("IO_M2.1_new/", "Outputs_D1F050-mature2_gn1.RData") %>%  get_measureList()
  #
  # RiskMeasures_F75mature1_gn1 <- get_df_results("IO_M2.1_new/", "Outputs_D1F075-mature1_gn1.RData") %>%  get_measureList()
  # RiskMeasures_F75mature2_gn1 <- get_df_results("IO_M2.1_new/", "Outputs_D1F075-mature2_gn1.RData") %>%  get_measureList()
   
  
                                  
                                               




