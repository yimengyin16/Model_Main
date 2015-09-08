# Import returns and tax base for repeat of history

df_inputs <- read_excel("IO_M2.1history/RepeatingHistoryScenarioData(5).xlsx", sheet="RData", skip=1) %>% 
             filter(!is.na(i.r))

i.r <- matrix(df_inputs$i.r, nrow = Global_paramlist$nyear, ncol = Global_paramlist$nsim) 

