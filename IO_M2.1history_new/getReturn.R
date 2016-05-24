# Import returns and tax base for repeat of history

# df_inputs <- read_excel("IO_M2.1history_new/StressScenario(21).xlsx", sheet="Sheet1", skip = ) %>% 
#              filter(!is.na(i.r))

df_inputs <- read.csv("IO_M2.1history_new/StressScenario(21)1995.csv") %>% 
             filter(!is.na(i.r))

i.r <- matrix(df_inputs$i.r, nrow = Global_paramlist$nyear, ncol = Global_paramlist$nsim) 

