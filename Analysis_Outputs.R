library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())
source("Functions.R")
folder_run <- "IO_Initial_Runs"



## Combine selected files into a single list.
file_select <- dir(folder_run, pattern = "06-06")

fn <- function(x) {
       load(paste0(folder_outputs, "/", x))
       outputs_list
  }

lists_all <- alply(file_select, 1, fn)

names(lists_all) <- laply(lists_all, function(x) x$paramlist$runname)


## Combine the results into a single data frame.
results_all <- ldply(lists_all, function(x) x$results) %>% select(-X1)







draw_quantiles <- function(runName,     # character
                           varName,     # character
                           data = results_all,
                           year.max = 50,
                           qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
  
  df <- data %>% filter(runname == runName) %>%  
        select_("sim","year", varName) %>% spread_("year", varName)
  
  df_q <-   sapply(select(df, -sim), function(x) quantile(x, qts)) %>% 
    as.data.frame
  
  df_q %<>% mutate(Quantile = rownames(df_q)) %>% gather(year, Value, -Quantile) %>%
    mutate(year = f2n(year),
           Quantile = factor(Quantile)) %>% 
    filter(year <= year.max)
  
  
  ggplot(df_q, aes(x = year, y = Value, color = Quantile)) + theme_bw() + 
    geom_point(size = 2.5) + geom_line()+ 
    labs(y = varName, title = paste0("Quantile plot of ", varName, " in ", runName))
}



draw_quantiles("R1F1", "FR")

draw_quantiles("R2F1", "FR", year.max = 80)
draw_quantiles("R2F2", "FR", year.max = 80)
draw_quantiles("R2F3", "FR", year.max = 80)

draw_quantiles("R3F1", "FR", year.max = 80)
draw_quantiles("R3F2", "FR", year.max = 80)
draw_quantiles("R3F3", "FR", year.max = 80)


draw_quantiles("R4F1", "FR")
draw_quantiles("R4F2", "FR")
draw_quantiles("R4F3", "FR")


draw_quantiles("R5F1", "FR", year.max = 100)
draw_quantiles("R5F2", "FR", year.max = 100)
draw_quantiles("R5F3", "FR", year.max = 100)

draw_quantiles("R6F1", "FR", year.max = 100)
draw_quantiles("R6F2", "FR", year.max = 100)
draw_quantiles("R6F3", "FR", year.max = 100)



draw_quantiles("R4F1", "C_PR",year.max = 100)
draw_quantiles("R4F2", "C_PR",year.max = 100)
draw_quantiles("R4F3", "C_PR",year.max = 100)

draw_quantiles("R4F1", "B_PR",year.max = 100)
draw_quantiles("R4F2", "B_PR",year.max = 100)
draw_quantiles("R4F3", "B_PR",year.max = 100)

draw_quantiles("R4F1", "MA_PR",year.max = 100)
draw_quantiles("R4F2", "MA_PR",year.max = 100)
draw_quantiles("R4F3", "MA_PR",year.max = 100)



draw_quantiles("R4F2", "ERC")
draw_quantiles("R4F3", "ERC")















outputs_list$results %>% filter(sim == 1) %>% kable

%>% filter(sim %in% 1:50) %>% 
  ggplot(aes_string(x = "year", y = var, color = "factor(sim)")) + theme_bw() + 
  geom_point() + geom_line()
df_q$Quantile %>% factor



# for (fileName in dir(folder_outputs)){
#   folder_outputs <- "Outputs_Initial_Runs_2"
#   
# #  fileName <- "Outputs_R1F2_06-04-2015.RData"
#   
#   load(paste0(folder_outputs, "/", fileName))
#   
#   RunName <- outputs_list$paramlist$runname
#   
#   outputs_list$results %<>% mutate(FR_MA   = 100 * MA / exp(log(AL)),
#                                    B_PR    = 100 * B / PR, 
#                                    MA_PR   = 100 * MA / PR,
#                                    ERC_PR  = 100 * ERC / PR,
#                                    dERC_PR = ERC_PR - lag(ERC_PR),
#                                    ADC_PR  = 100 * ADC / PR, 
#                                    C_PR    = 100 * C / PR,
#                                    runname = RunName) %>% 
#                             select(runname, sim, year, everything())
#   
#   save(outputs_list, file = paste0(folder_outputs, "/", fileName))
#   
# }




rm(list = ls())
source("Functions.R")
folder_outputs <- "Outputs"

load(paste0(folder_outputs, "/", "Outputs_R4F1_06-06-2015.RData"))
draw_quantiles("R4F1", "FR", outputs_list$results)


load(paste0(folder_outputs, "/", "Outputs_R4F2_06-06-2015.RData"))
draw_quantiles("R4F2", "FR", outputs_list$results)


load(paste0(folder_outputs, "/", "Outputs_R4F3_06-06-2015.RData"))
draw_quantiles("R4F3", "FR", outputs_list$results)


