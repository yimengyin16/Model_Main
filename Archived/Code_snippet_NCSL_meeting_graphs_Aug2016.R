# Code snippet for NCSL meeting presentation graphs (Aug, 2016) from Yimeng 


#**************************************
## "RIG" colors ####
#**************************************

RIG.blue  <-  "#003598"
RIG.red   <-  "#A50021"
RIG.green <-  "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829" 



#**************************************
## Quantile plot for funded ratio ####
#**************************************

# Result file in Dropbox
# "~/Pension simulation project/01_Model_Outputs/Outputs_A1F075_O30pA5.RData"
# In the following snippet, loaded runs are stored in the data frame "df". 


# Code snippet
df_fig <- df %>% filter(runname == "A1F075_O30pA5", year <= 30) %>% 
  select(runname, sim, year, FR_MA) %>% 
  group_by(year) %>% 
  summarize(fr_q75 = quantile(FR_MA, 0.75),
            fr_q50 = median(FR_MA),
            fr_q25 = quantile(FR_MA, 0.25)) %>% 
  ungroup %>% 
  gather(qtl, value, -year) %>% 
  mutate(qtl = factor(qtl, levels = c("fr_q75", "fr_q50", "fr_q25"),
                      labels = c("75th percentile", "median", "25th percentile")))

fig12 <- 
  df_fig %>% 
  ggplot(aes(x = year, y = value, color = qtl, shape = qtl)) + theme_bw() + 
  geom_line(size = 1.2) + 
  geom_point(size = 1.8) + 
  geom_hline(yintercept = c(75, 100), linetype = c(2, 1), size  = 0.5)+
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue)) + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  labs(x = "Year", y = "Funded ratio (%)",
       title = "Funded ratio quartiles with common funding policy \n30-year level-percent open, 5-year asset smoothing \n(Initial funded ratio of 75%)") + 
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.key=element_rect(size=4, color="white"),
        legend.key.size = unit(1.6, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
# text = element_text(family = "Arial"))
# axis.text  = element_text(family = "Arial"),
# axis.title = element_text(family = "Arial"))  



#******************************************************
## Risk graphs with 8%, 12% 16% standard deviation ####
#******************************************************

# Result file in Dropbox
# "~/Pension simulation project/01_Model_Outputs/Outputs_I8F075-2.RData"  # For sd = 8%
# "~/Pension simulation project/01_Model_Outputs/Outputs_I8F075-3.RData"  # for sd = 12%
# "~/Pension simulation project/01_Model_Outputs/Outputs_I8F075-4.RData"  # for sd = 16%
# in the following snippet, all loaded runs are stored in data frame "results_all".


runs_all2 <- paste0("I8F075-", 2:4)

df_plot2 <- results_all  %>% 
  filter(runname %in% runs_all2, sim > 0, year <= 30 )

df_dc75 <- df_plot2 %>% filter(runname == runs_all2[1]) %>% select(sim, year, AL75 = AL) # Not necessary here since all 3 runs use 7.5% discount rate. 


df_plot2 <- df_plot2 %>% 
  left_join(df_dc75) %>% 
  select(runname, sim, year, AL75, MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL75,
         FR40less  = cumany(FR_MA <= 40),
         ERC_high  = cumany(ERC_PR >= 30), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 4) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            FR.med   = median(FR_MA),
            ERC_PR.med= median(ERC_PR)
  ) %>% 
  #left_join(runs_all_labels) %>% 
  ungroup() %>% 
  mutate(runname = factor(runname, levels = runs_all2, 
                          labels = c("Standard deviation = 8%", 
                                     "Standard deviation = 12%",
                                     "Standard deviation = 16%")))



p.FR40less <- 
  ggplot(df_plot2, aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,50, 5)) + 
  labs(x = "Year",
       y = "Probability (%)",
       title = "Probability of funded ratio falling below 40% \nat any time prior to and including the given year") + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
p.FR40less



p.ERC_hike <- 
  ggplot(df_plot2, aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 3) + geom_line() +
  coord_cartesian(ylim = c(0, 35)) + 
  scale_x_continuous(breaks = seq(0,30, 5))+ 
  scale_y_continuous(breaks = seq(0,100, 5)) + 
  labs(x = "Year",
       y = "Probability (%)",
       title = "Probability of employer contribution rising by more than \n10% of payroll at any time prior to and including the given year ") + 
  guides(col = guide_legend(title = NULL), shape = guide_legend(title = NULL)) + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))
p.ERC_hike




