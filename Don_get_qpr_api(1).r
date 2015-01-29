# Don Boyd
# 1/27/2015
# Get data from the Census quarterly public retirement systems survey

# following items are built into my system but Yimeng needs them
# censusapikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"  # djb API key; you may want to get your own
# mdyfn <-  function(m, d, y) as.Date(ISOdate(y, m, d)) # pass numeric month, day, year, get back a full date
# qy <- function(q, y) mdyfn(q * 3 - 2, 1, y) # pass numeric quarter and year to this fn, get back first date in the quarter
# you will need jsonlite and maybe rcurl if you don't have them

pre <- "http://api.census.gov/data/eits/qpr?key="
post <- "&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data"
url <- paste0(pre, censusapikey, post) # note that a censusapikey is needed
system.time(qprdat <- jsonlite::fromJSON(url)) # return results as a matrix # about 40-50 secs

df <- data.frame(qprdat)
ht(df)
names(df) <- t(df[1, ])
df <- df[-1, ]
str(df)
ht(df)
count(df, time_slot_name)
count(df, seasonally_adj) # all no, ok to delete
count(df, geo_level_code) # US only, so ok to delete
count(df, category_code)
count(df, data_type_code) %>% data.frame
count(df, error_data) # if no errors, ok to delete

q <- as.numeric(substr(df$time_slot_name, 2, 2))
y <- as.numeric(substr(df$time_slot_name, 3, 6))
df$date <- qy(q, y)
filter(df, date==max(df$date)) # look at latest data
count(df, date, time_slot_name) %>% data.frame

df$time_slot_name <- NULL  # use date instead
df$seasonally_adj <- NULL
df$geo_level_code <- NULL
df$error_data <- NULL
df$value <- cton(df$cell_value)
df$cell_value <- NULL
ht(df)

# get short names
df %<>% rename(catcode=category_code, datacode=data_type_code)

# look at the data
df %>% filter(grepl("RV", datacode)) %>% count(date, datacode) %>% spread(datacode, n) %>% data.frame
count(df, catcode, datacode) %>% spread(catcode, n) %>% data.frame

# get major cash flow items as % of assets
df2 <- df %>% filter(datacode %in% c("HLDTOT", "RVCNTG", "RVCNTE", "RVCNTT", "RVPMTS")) %>%
  select(-catcode) %>% 
  spread(datacode, value) %>%
  mutate(RVCNTT=ifelse(is.na(RVCNTT), RVCNTG + RVCNTE, RVCNTT), # total contributions were missing in early years, so compute them
         cgpct=RVCNTG / HLDTOT *100,
         ctpct=RVCNTT / HLDTOT * 100, # total contributions
         bpct=RVPMTS / HLDTOT * 100, # total payments (primarily benefits)
         xcf=RVCNTT - RVPMTS, # external cash flow - i.e., before investment income
         xcfpct=xcf / HLDTOT * 100)

title <- "Large public pension fund quarterly cash flows as % of assets\nMultiply by 4 for approx annual rates"
df2 %>% filter(year(date)>=1974) %>%
  select(date, ctpct, bpct, xcfpct) %>% 
  gather(variable, value, -date) %>%  
  qplot(date, value, data=., colour=variable, geom=c("point", "line"), main=title) + geom_hline(y=0)


