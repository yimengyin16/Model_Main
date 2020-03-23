# This module create investment return series. 


# ecdf_fun <- function(x,perc) ecdf(x)(perc)
get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1



load("IO_penSimMacro/penSimInputs_returns.RData")

penSimInputs_returns %<>% 
  mutate(return60_40 = 0.6*stockreturn_nom + 0.4*bondreturn_nom,
         return70_30 = 0.7*stockreturn_nom + 0.3*bondreturn_nom)


penSimInputs_returns$return70_30 %>% mean # 6.8%, approx geoMean is 6.1%, very close to forward looking assumption of SOA Blue Ribbon 
penSimInputs_returns$return70_30 %>% sd   # 12.14%

penSimInputs_returns$return60_40 %>% mean 
penSimInputs_returns$return60_40 %>% sd

# Create return matrix for a 70/30 portfolio based on simulated returns 
i.r_70_30 <- 
penSimInputs_returns %>% 
  select(year, sim, return70_30) %>% 
  spread(sim, return70_30) %>% 
  select(-year) %>% 
  as.matrix



# Create normally distributed returns with the same mean and SD as the simulated 70/30 portfolio
mean_70_30 <- i.r_70_30 %>% mean
SD_70_30   <- i.r_70_30 %>% sd

nsim  <- dim(i.r_70_30)[2]
nyear <- dim(i.r_70_30)[1]

i.r_normal <- matrix(rnorm(nsim*nyear, mean_70_30, SD_70_30), nyear, nsim)



# selecting return  


if(paramlist$return_type == "port70_30") i.r <- i.r_70_30
if(paramlist$return_type == "normal")    i.r <- i.r_normal



i.r <- cbind(rep(paramlist$i, Global_paramlist$nyear), # Check consistency
             rep(paramlist$i, Global_paramlist$nyear), # Check consistency
             #rep(paramlist$ir.mean - paramlist$ir.sd^2/2 , Global_paramlist$nyear), # Deterministic run
             i.r)
colnames(i.r) <- c(-1:Global_paramlist$nsim)

#i.r %>% dim()


#as.numeric(i.r_70_30[,1:410]) %>% get_geoReturn()


#i.r_70_30[,409]



# #**************************************************************************************
# #                        Fat-tails in simulated returns                             ####
# #**************************************************************************************
# 
# ## simulated data
# df_sim_gdp_y %<>% as.data.frame()         %>% rename(gdp_chg = return_y)
# df_sim_stockreturn_y %<>% as.data.frame() %>% rename(stock_return   = return_y)
# df_sim_bondreturn_y %<>% as.data.frame()  %>% rename(bond_return = return_y)
# 
# df_sim <- df_sim_gdp_y %>% 
#   left_join(df_sim_stockreturn_y) %>% 
#   left_join(df_sim_bondreturn_y) %>% 
#   mutate(port60_40_return = 0.6*stock_return + 0.4*bond_return)
# 
# 
# mean_sim_stock <- df_sim$stock_return %>% mean
# sd_sim_stock   <- df_sim$stock_return %>% sd
# 
# mean_port60_40 <- df_sim$port60_40_return %>% mean
# sd_port60_40   <- df_sim$port60_40_return %>% sd
# 
# 
# df_sim %<>% mutate(port_norm  = rnorm(nrow(df_sim), mean_port60_40, sd_port60_40),
#                    stock_norm = rnorm(nrow(df_sim), mean_sim_stock, sd_sim_stock))
# 
# 
# qts <- c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)
# 
#  # comparing percentiles 
# quantile(df_sim$stock_return, qts)
# quantile(df_sim$stock_norm,   qts)
# 
# quantile(df_sim$port60_40_return, qts)
# quantile(df_sim$port_norm,   qts)
# 
# 
# df_probSim <- 
# df_sim %>% 
#   select(stock_return, stock_norm, port60_40_return, port_norm) %>% 
#   gather(var, value) %>% 
#   group_by(var) %>% 
#   summarise(prob40 = ecdf_fun(value, -0.4),
#             prob30 = ecdf_fun(value, -0.3),
#             prob20 = ecdf_fun(value, -0.2),
#             prob10 = ecdf_fun(value, -0.1))
# 
# 
# #**************************************************************************************
# #           Fat tails in bootstrapped returns                                      ####
# #**************************************************************************************
# 
# # Historical data
# df_hist <- 
# df_dataAll_y %>% 
#   select(year, LCapStock_TRI, LTGBond_TRI, CBond_TRI) %>% 
#   mutate_at(vars(-year), funs(log(./lag(.)))) %>% 
#   filter(year %in% 1955:2015)
# 
# 
# df_boot <- df_hist[c("LCapStock_TRI", "LTGBond_TRI")][sample(1:nrow(df_hist), nrow(df_sim), replace = TRUE), ] %>% 
#   rename(stock_boot = LCapStock_TRI, 
#          GBond_boot = LTGBond_TRI) %>% 
#   mutate(port60_40_boot = 0.6*stock_boot + 0.4*GBond_boot)
# 
# # stock_boot <- sample(df_hist$LCapStock_TRI, nrow(df_sim), replace = TRUE)
# # GBond_boot <- sample(df_hist$LTGBond_TRI,   nrow(df_sim), replace = TRUE)
# # boot_port60_40 <- 0.6*boot_stock + 0.4*boot_GBond
# 
# mean_boot_stock <- mean(df_boot$stock_boot); mean_boot_stock
# sd_boot_stock   <- sd(df_boot$stock_boot);     sd_boot_stock
# 
# mean_boot_GBond <- mean(df_boot$GBond_boot); mean_boot_GBond
# sd_boot_GBond   <- sd(df_boot$GBond_boot);     sd_boot_GBond
# 
# mean_boot_port60_40 <- mean(df_boot$port60_40_boot); mean_boot_port60_40
# sd_boot_port60_40   <- sd(df_boot$port60_40_boot);   sd_boot_port60_40
# 
# 
# stock_norm2 <- rnorm(nrow(df_sim), mean_boot_stock, sd_boot_stock)
# port_norm2  <- rnorm(nrow(df_sim), mean_boot_port60_40, sd_boot_port60_40)
# 
# df_boot %<>% mutate(stock_norm = stock_norm2,
#                     port60_40_norm  = port_norm2) 
#   
# 
#  # comparing percentiles 
# # quantile(boot_stock,   qts)
# # quantile(stock_norm2,  qts)
# # 
# # quantile(boot_port60_40, qts)
# # quantile(port_norm2,   qts)
# 
# df_probBoot <- 
# df_boot %>% 
#   select(stock_boot, stock_norm, port60_40_boot, port60_40_norm) %>% 
#   gather(var, value) %>% 
#   group_by(var) %>% 
#   summarise(prob40 = ecdf_fun(value, -0.4),
#             prob30 = ecdf_fun(value, -0.3),
#             prob20 = ecdf_fun(value, -0.2),
#             prob10 = ecdf_fun(value, -0.1))
# df_probBoot
# 
# 
# write.xlsx2(df_probSim,  "Data_SimMacro/Table_probsFatTail.xlsx", sheet = "sim")
# write.xlsx2(df_probBoot, "Data_SimMacro/Table_probsFatTail.xlsx", sheet = "boot", append = T)
# 
# 
# # examine changes during recession periods
# # recession with positive GDP growth but negative asset returns
# 
# 
# 
# 
# 
# 
# # gen_returns <- function( #.paramlist = paramlist,
# #                          #.Global_paramlist = Global_paramlist,
# #   
# #                         nyear   = Global_paramlist$nyear,
# #                         nsim    = Global_paramlist$nsim,
# #                         ir.mean = paramlist$ir.mean,
# #                         ir.sd   = paramlist$ir.sd,
# #                         seed    = 1234) {
# # 
# # #assign_parmsList(.Global_paramlist, envir = environment())
# # #assign_parmsList(.paramlist,        envir = environment())
# #   
# #   
# #   set.seed(seed)
# #   i.r <- matrix(rnorm(nyear  *nsim, mean = ir.mean, sd = ir.sd),nrow = nyear, ncol = nsim)
# #   
# #   if (all(i.r >= -1)) return(i.r) 
# #   else {
# #     warning("A draw is discarded because it contains value(s) smaller than -1.")
# #     gen_returns(nyear = nyear, nsim = nsim, ir.mean = ir.mean, ir.sd = ir.sd, seed = seed + 1)}
# # }
# # 
# # 
# # 
# # 
# # if(devMode){
# #   set.seed(1234)
# #   #i.r <- with(Global_paramlist, matrix(rnorm(nyear*nsim, mean = 0.08, sd = 0.12),nrow = nyear, ncol = nsim)) 
# #   i.r <- with(Global_paramlist, matrix(0.08, nrow = nyear, ncol = nsim))
# #   i.r[10,] <- 0.00 # Create a loss due to zero return in year 10. For the purpose of checking amortization of UAAL
# #   
# # } else {
# #   
# #   if(paramlist$return_type == "simple") i.r <- gen_returns()
# #   
# #   if(paramlist$return_type == "internal"){
# #     
# #     if(sum(paramlist$plan_returns$duration) != Global_paramlist$nyear) stop("Length of return series does not match nsim.", call. = FALSE)
# #     
# #     # set.seed(1234)
# #     i.r <- with(paramlist, mapply(gen_returns, 
# #                                     nyear   = paramlist$plan_returns$duration,
# #                                     nsim    = Global_paramlist$nsim,
# #                                     ir.mean = paramlist$plan_returns$ir.mean,
# #                                     ir.sd   = paramlist$plan_returns$ir.sd,
# #                                     SIMPLIFY  = (nrow(paramlist$plan_returns) == 1)
# #     )) %>% 
# #       do.call(rbind, .)
# #   }
# #   
# #   
# #   if(paramlist$return_type == "external") source(paste0(folder_run, "/getReturn.R"))
# #   
# #   
# #   ## Add two additional runs as run 0 and run -1.
# #    # Run 0: deterministic return the same as ir.mean - ir.sd^2/2
# #    # Run -1: deterministic return the same as i. 
# #   
# #   i.r <- cbind(rep(paramlist$i, Global_paramlist$nyear), # Check consistency
# #                rep(paramlist$ir.mean - paramlist$ir.sd^2/2 , Global_paramlist$nyear), # Deterministic run
# #                i.r)
# #   colnames(i.r) <- c(-1:Global_paramlist$nsim)
# #   
# # }
# # 
# # 
# # paramlist$plan_returns


