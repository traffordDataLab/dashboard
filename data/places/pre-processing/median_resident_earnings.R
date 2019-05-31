# Median resident earnings, all employees #

# Source: Annual Survey of Hours and Earnings - resident analysis, ONS
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=30
# Licence: Open Government Licence v3.0

library(tidyverse)

df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_30_1.data.csv?geography=E92000001,E11000001,E08000009&date=latestMINUS9-latest&sex=7&item=2&pay=7&measures=20100,20701&select=date_name,geography_name,geography_code,sex_name,pay_name,item_name,measures_name,obs_value") %>% 
  filter(MEASURES_NAME == "Value") %>%
  mutate(area_name = case_when(GEOGRAPHY_NAME == "Greater Manchester (Met County)" ~ "Greater Manchester", TRUE ~ GEOGRAPHY_NAME),
         indicator = "Median resident earnings, all employees",
         measure = "Median",
         unit = "Persons") %>% 
  select(area_code = GEOGRAPHY_CODE,
         area_name,
         indicator, 
         period = DATE_NAME, 
         measure, unit,
         value = OBS_VALUE)

write_csv(df, "../median_resident_earnings.csv")
