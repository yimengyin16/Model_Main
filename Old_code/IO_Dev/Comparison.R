
# Note:
 # Need to modify the termination rates when compared with single retirement age with r.min = r.max = 65



options(digits = 4, scipen = 6)

# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.
var.display <- c("year",  "AL",    "AA",   "FR", "NC",    "SC", "UAAL",
                 "AL.act_PR", "AL.ret_PR","AL.term_PR", 
                 "NC.act_PR", "NC.term_PR", 
                 #"AL_PR", "NC_PR", "SC_PR", "C_PR", "ERC_PR", 
                  "PR",#
                 
                 # "ExF",   
                 "UAAL",  "EUAAL", "LG",    "NC",    "SC",    
                 #"ADC", "EEC", "ERC",  
                 "C", "B"     
                 # "I.r" ,   "I.e"
                 # "i",    "i.r"
                 #, "dERC_PR"
                 # "AM", "PR",
                 # "C_ADC"
)



# average plan
load("./IO_Dev/Outputs_O1.RData")
outputs_list$results %>% as.data.frame %>%filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_S1.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_M1.RData")
outputs_list$results %>% as.data.frame %>%filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))



# young plan
load("./IO_Dev/Outputs_O2.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_S2.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_M2.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))



# old plan
load("./IO_Dev/Outputs_O3.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_S3.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))

load("./IO_Dev/Outputs_M3.RData")
outputs_list$results %>% as.data.frame %>% filter(sim == 1, year %in% 1:30) %>% select(one_of(var.display))





