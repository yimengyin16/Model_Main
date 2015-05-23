# This script import salary and retirement benefit data.

file_path <- paste0("Data/")

#*************************************************************************************************************
#                                      Import Salary scale from AV                                       #####                  
#*************************************************************************************************************

# Notes:
# 1. Given a salary scale and a salary table in year 1, we can infer salary for workers who enter the workforce between 
#    year 1 - (r.max - 1 - 20) (-43 when r.max = 65), and year 0. However, for workers entering the workforce later than year 1, 
#    we cannot infer their salary based on the salary scale. One possible way to do the inference is described below. 
#    1) Make assmption about how starting salary (ea = age) grows after year 1, one option is to use the inflation rate + real wage growth rate, or just inflation rate. 
#       as the growth rates of starting salary.
#    2) With the assumed growth rates of starting salary, calculate the starting salary in year 2 to year nyear based on the starting salary in year 1.
#    3) Based on the starting calculated in 2), calculate complete salary table (ea by age) for year 2 to year nyear using the salary scale. 


## Salary scale type 1: Growth only depends on age
# Example: PA-PSERS

SS <- read.xlsx2(paste0(file_path, "PA-PSERS.xlsx"), sheetName = "SalaryGrowth", colClasses = "numeric", startRow = 3, stringsAsFactors = FALSE)
SS %<>%  rename(age.match = age) %>% right_join(data.frame(age = 20:70) %>% mutate(age.match = floor(age/10)*10)) %>% 
  select(-age.match) %>% 
  mutate(growth = cton(growth)/100)

# Salary scale for all starting year
SS.all <- expand.grid(start.year = -89:nyear, ea = range_ea, age = 20:(r.max - 1)) %>% 
  filter(age >= ea, start.year + r.max - 1 - ea >= 1 ) %>%
  arrange(start.year, ea, age) %>%
  left_join(SS) %>% 
  group_by(start.year, ea) %>% 
  mutate(year = start.year + (age - ea),
         growth.start = (1 + infl)^(start.year - 1), # assume starting salary grows at the rate of inflation for all entry ages 
         scale = cumprod(ifelse(age == ea, 1, lag(1 + growth))),
         scale = ifelse(start.year <= 1, scale/scale[year == 1],
                        scale * growth.start)
  )

## Salary scale type 2: Growth depends on yos and year.
# Example: NJ-TPAF

#*************************************************************************************************************
#                                      Import Initial Salary table from AV                               #####                  
#*************************************************************************************************************

## Read in an example of salary table: PA-PSERS

# actives - numbered p.34 ####
# age x yos
# age 25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 Over 64
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.mid <- c(25, seq(27, 62, 5), 66)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format 
df <- readWorksheetFromFile(paste0(file_path, "PA-PSERS.xlsx"), sheet="PA-PSERS", header=FALSE, region="A5:L24")
names(df) <- c("order", "tabletype", "agegrp", yos.mid)

avgpay <- df %>% filter(tabletype=="avgpay") %>% 
  arrange(order) %>%
  mutate(age=age.mid) %>%
  select(-order, -tabletype, -agegrp) %>%
  gather(yos, avgpay, -age) %>%
  filter(!is.na(avgpay)) %>%
  mutate(yos = f2n(yos)) %>% 
  splong("age", method = "natural") %>%
  splong("yos", method = "natural")

avgpay <- (expand.grid(age = 20:70, yos = 2:42) %>% mutate(age.match = ifelse(age < 25, 25,ifelse(age>66, 66,age)))) %>% 
  left_join(avgpay %>% rename(age.match = age))

avgpay <- (expand.grid(age = 20:70, yos = 0:50) %>% mutate(yos.match = ifelse(yos < 2, 2, ifelse(yos>42, 42, yos)))) %>% 
  left_join(avgpay %>% rename(yos.match = yos))

avgpay %<>% select(-age.match, -yos.match) %>% 
  filter(age - yos >= 20) %>% 
  mutate(ea = age - yos)


# Display in matrix form  
# avgpay %<>% select(-yos) %>% 
#   spread(age, avgpay, fill = 0)
# rownames(avgpay) <- avgpay$ea
# avgpay



#*************************************************************************************************************
#                                        Create complete salary history                                  #####                  
#*************************************************************************************************************

salary <- SS.all %>% left_join(avgpay) %>% 
  group_by(start.year, ea) %>% 
  mutate(sx = ifelse(start.year <= 1, avgpay[year == 1]* scale, 
                     avgpay[age == ea]* scale)) %>% 
  select(start.year, ea, age, year, sx)

# Check the growth of starting salary before year 1
fn <- function(x) (x[length(x)]/x[1])^(1/(length(x) - 1)) - 1

# starting salary over time
start.pay <- salary %>% filter(age == ea, start.year <=1) %>% ungroup %>%  arrange(ea, year) %>% 
  select(ea, year, sx)
start.pay %>%  kable
# start.pay %>% ggplot(aes(x = year + 2012, y = sx, colour = factor(ea))) + geom_line(size = 1)


# average growht rates of starting salary 
salary %>% filter(age == ea, start.year <=1) %>% ungroup %>% group_by(ea) %>%  arrange(ea, year) %>% 
  select(ea, year, sx) %>% filter(year <= -12 ) %>% summarize(g = fn(sx) )%>% kable


# 1. Starting salary generally has in rising trend, while drops after the Great Recession.(The rising trend in the end may be due to the imputation)
# 2. The growth rates before 40 are around 3% ~ 4%, which is approximately the average inflation rate
# 3. The growth rates of starting salary are much lower for ages greater 40, and even become negative in 50s.
#    Even after taking into account that starting salary at higher entry ages have shorter history and hencea are
#    affected by the recent recession more, their growth curves are still flatter than those of lower entry ages.
# 4. Need to be carefull about the observations above since the imputed salary table is used.
# 5. If the observations above are real, we need to make different assumptions about the growth rate of starting
#    salary for different entry ages. The current assumption of a flat growth rate over all entry ages may be inappropriate.


#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************

## Read in an example of retirement benefit table: PA-PSERS

## WARNING: # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)

# repeat for retirees p.35 ####
# age x yos
# age has different groupings than for actives, of course; yos groupings are the same
# age <50 50-54 55-59 60-64 65-69 70-74 75-79 80-84 85-89 Over 89
# yos 0-4	 5-9	 10-14	15-19	20-24	25-29	30-34	35-39	40+
age.mid <- c(48, seq(52, 87, 5), 92)
yos.mid <- c(seq(2, 37, 5), 42)

# convert to the ea x age format 
df <- readWorksheetFromFile(paste0(file_path, "PA-PSERS.xlsx"), sheet = "PA-PSERS", header = FALSE, region = "A30:L49")
names(df) <- c("order", "tabletype", "agegrp", yos.mid)

avgben <- df %>% filter(tabletype=="bens") %>% 
  arrange(order) %>%
  mutate(age=age.mid) %>%
  select(-order, -tabletype, -agegrp) %>% 
  gather(yos, avgben, -age) %>%
  mutate(yos = f2n(yos)) %>% 
  splong("age", method = "natural") %>%
  splong("yos", method = "natural")
  #%>% spread(yos, avgben) %>% print

avgben <- (expand.grid(age = 48:110, yos = 2:42) %>% mutate(age.match = ifelse(age > 92, 92, age))) %>% 
  left_join(avgben %>% rename(age.match = age))

avgben <- (expand.grid(age = 48:110, yos = 0:50) %>% mutate(yos.match = ifelse(yos < 2, 2, ifelse(yos>42, 42, yos)))) %>% 
  left_join(avgben %>% rename(yos.match = yos))

avgben %<>% 
  mutate(ea = 70 - yos,
         year = 1) %>%    
  filter(ea >= 20, 
         age >= r.max) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  select(-age.match, -yos.match, -yos)

# Notes:
# 1. With single retirement age, entry age is assumed retirement age minus yos. Note that "assumed retirement age" is
#    not necessarily equal to r.max. But this is only a example of retirement benefit table used to develop the main model,
#    all we want to make sure is that its format is compatible with the model. When we want to model the a prototype plan,
#    we need to make the table consistent with the parameters in the main model. 
# 2. With multiple retirement ages and vested terms, we can simple assume they retire/quite at age r.max, since 
#    when they retire/quite does not affect the calculation 
# 3. Actually all we need is average salary by age group. I don't think we care about the ea and yos of retirees. 
#    When merged to the main model we can assign any proper ea to the initial retirees. 

  
# Display in matrix form  
# avgben %<>% 
#   spread(age, avgben, fill = 0)
# rownames(avgben) <- avgben$ea
# avgben

# there are negative values at the upper left corner. But it should be ok if we assume there is no retirees under age 52. 






