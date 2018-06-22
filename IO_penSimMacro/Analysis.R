# Actuarial Valuation in a simple setting
# Yimeng Yin
# 5/27/2015



# RIG colors and theme
RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function() {
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 9)
  )
}

RIG.themeLite <- function() {
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 9)
  )
}


color_PIT <- RIG.green
color_salesgen <- "blue"
color_salessel <- "deepskyblue2"
color_other <- RIG.purple
color_propertyLoc <- RIG.yellow.dark
color_GDP <- "gray50"



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "IO_penSimMacro/"
Outputs_folder  <- "IO_penSimMacro/"



#*****************************************************
##  Loading data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    # if("results.t7" %in% names(outputs_list)){
    #   df_out <- bind_rows(outputs_list$results,
    #                       outputs_list$results.t7,
    #                       outputs_list$results.xt7)
    #   return(df_out)
    # } else {
    #   return(outputs_list$results)
    # }
    
    return(outputs_list$results)
    
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder) %>% select(runname, sim, year, everything())

# results_all %>% filter(year == 1, sim == 1) %>% select(year, runname, AL, MA)


#*********************************************************************************************************
#  risk measures ####
#*********************************************************************************************************


df_all.stch <-
  results_all %>%
  filter(sim >= 0, year <= 30)


df_all.stch %<>%
  select(runname, sim, year, AL, MA, EEC, PR, ERC_PR) %>%
  group_by(runname, sim) %>%
  mutate(FR_MA     = 100 * MA / AL,
         FR40less   = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high   = cumany(ERC_PR >= 50),
         ERC_hike   = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>%
  group_by(runname, year) %>%
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),

            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),

            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)


  ) %>%
  ungroup()


df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_O30pA5_port70_30")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_C15d_port70_30" )
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_O30pA5_normal")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "A_C15d_normal" )


df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_O30pA5_port70_30")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_C15d_port70_30" )
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_O30pA5_normal")
df_all.stch %>% filter(year %in% c(1, 10, 20, 30), runname == "B_C15d_normal" )






#*********************************************************************************************************
#  figures ####
#*********************************************************************************************************


# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio" 
fig_FRdist <- 
  df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
                                        "B_O30pA5_port70_30", "B_C15d_port70_30"
                                        )) %>% 
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
         Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
         ) %>% 
  mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
  )) + theme_bw() + 
  facet_grid(DiscRate ~ Amort) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(40,160)) + 
  scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Year", y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_FRdist
fig_FRdist$data

df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30") )




# Distribution of ERC as % Payroll
fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_ERCdist <- 
  df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
                                        "B_O30pA5_port70_30", "B_C15d_port70_30"
  )) %>% 
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
         Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
  ) %>% 
  mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>% 
  
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  theme_bw() + 
  facet_grid(DiscRate ~ Amort) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,50)) + 
  scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Year", y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_ERCdist


# Risk of low funded ratio
fig.title <- "Probabilities of funded ratio below 75%, 60%, and 40% in any year up to the given year"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_FR40less <- 
  
  df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
                                        "B_O30pA5_port70_30", "B_C15d_port70_30"
  )) %>% 
  select(runname, year, FR40less) %>%
  gather(type, value, -runname, -year) %>% 
  mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
         Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
  ) %>% 
  mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>%
  
  # mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = Amort, shape = Amort)) + 
  # color = runname, shape = runname)) + 
  theme_bw() + 
  facet_grid(.~DiscRate) + 
  geom_point(size = 2) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = NULL) + 
  scale_shape_manual(values = c(17,16, 15),  name = NULL) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Year", y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_FR40less
fig_FR40less$data %>% filter(year == 2046)


# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Simulated investment returns of a 70/30 portfolio"
fig_ERChike <- 
  
  df_all.stch %>% filter(runname %in% c("A_O30pA5_port70_30", "A_C15d_port70_30",
                                        "B_O30pA5_port70_30", "B_C15d_port70_30"
  )) %>% 
  select(runname, year, ERC_hike) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(DiscRate = ifelse(str_detect(runname,"A_"), "Discount rate = 7.5%", "Discount rate = 6.0%"),
         Amort    = ifelse(str_detect(runname,"O30pA5"), "Slow repayment of UAAL", "Fast repayment of UAAL")
  ) %>% 
  mutate(runname = factor(runname, levels = c("A_O30pA5_port70_30", "A_C15d_port70_30"))) %>%
  
  ggplot(aes(x = year, y = value, color = Amort, shape = Amort)) + theme_bw() + 
  facet_grid(.~DiscRate) + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(seq(0, 30, 5))) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Year", y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_ERChike
fig_ERChike$data %>% filter(year == 2046)


#*********************************************************************************************************
#  Table  ####
#*********************************************************************************************************

df_all.stch %>% names

Table_risk_DC75 <- 
df_all.stch %>%
  select(runname, year, FR40less, ERC_hike, 
         FR.q10, FR.q25, FR.q50, FR.q75, FR.q90,    
         ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
  filter(year %in% c(1, 30), str_detect(runname, "A_"))


Table_risk_DC60 <-
df_all.stch %>%
  select(runname, year, FR40less, ERC_hike, 
         FR.q10, FR.q25, FR.q50, FR.q75, FR.q90,    
         ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
  filter(year %in% c(1, 30), str_detect(runname, "B_"))





#*********************************************************************************************************
#  Table  ####
#*********************************************************************************************************

dir_out <- "IO_penSimMacro/FigTab/"

write.xlsx2(Table_risk_DC60, file = paste0(dir_out, "Table_risk.xlsx"), sheetName = "DC60")
write.xlsx2(Table_risk_DC75, file = paste0(dir_out, "Table_risk.xlsx"), sheetName = "DC75", append = TRUE)

ggsave(fig_FRdist, file = paste0(dir_out, "fig_FRdist.png"), width = 8, height = 7 )
ggsave(fig_ERCdist, file = paste0(dir_out, "fig_ERCdist.png"), width = 8, height = 7 )

ggsave(fig_FR40less, file = paste0(dir_out, "fig_FR40less.png"), width = 10, height = 5 )
ggsave(fig_ERChike,  file = paste0(dir_out,  "fig_ERChike.png"), width = 10, height = 5 )




fig_FR40less
fig_ERChike
fig_FRdist
fig_ERCdist














