# Thie script creat summary statistics of new prototypes. 
# 10/08/15


# Assume abratios
 # Average Plan: AZ-PERS(1000:500)
 # Mature Plan: LA-CERA(1000:600), OH-PERS(1000:600)
 # Immature Plan: WA-PERS2(1000:300)


# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.
var.display <- c( "runname", "year",
                 "FR",
                 "AL_PR", "AL.act_PR", "AL.ret_PR","AL.term_PR", 
                 "NC_PR", "NC.act_PR", "NC.term_PR", 
                 "SC_PR", "C_PR", "ERC_PR",
                 "MA_PR",   
                 "PR.growth", 
                 "ExF_PR"   
)




## Funding variables from the results
gen_table <- function (run_name) {
  # run_name <- "AZ-PERS"
  load(paste0("IO_Dev/Outputs_", run_name,".RData"))
  outputs_list$results %>% 
    filter(sim == 1, year %in% c(1, 15, 30, 50)) %>%
    mutate(ExF_PR = ExF_PR * 100) %>% 
    select(one_of(var.display)) %>% kable
}

gen_table("AZ-PERS")
gen_table("LA-CERA")
gen_table("OH-PERS")
gen_table("WA-PERS2")



## Age distribution of new entrants
gen_enDist <- function (run_name) {
  # run_name <- "AZ-PERS"
  load(paste0("IO_Dev/Outputs_", run_name,".RData"))
  data.frame(runname = outputs_list$paramlist$runname,  ea = 20:74, entrants_pct = outputs_list$entrant_dist) 
}



entrants_dist <- rbind(
gen_enDist("AZ-PERS"),
gen_enDist("LA-CERA"),
gen_enDist("OH-PERS"),
gen_enDist("WA-PERS2")
)

ggplot(entrants_dist, aes(x = ea, y = entrants_pct, color = runname)) + geom_line() + geom_point()

x <- gen_enDist("AZ-PERS")
x$entrants_pct %>% length


## Demographic statistics 

# Workforce statistics to be calculated.  
# Average age of workforce
# Average year of service of workforce
# Average entry age of workforce
# Average age of retirees
# Active-to-retiree reatio

gen_demo_summary <- function (run_name) {
 # run_name <- "AZ-PERS"
 load(paste0("IO_Dev/Outputs_", run_name,".RData"))
 outputs_list$demo_summary %>% filter(year %in% c(1, 15, 30, 50))
 # outputs_list$demo_summary %>% filter(year %in% c(1:50))
}

gen_demo_summary("AZ-PERS")
gen_demo_summary("LA-CERA")
gen_demo_summary("OH-PERS")
gen_demo_summary("WA-PERS2")




