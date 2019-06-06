# Employees earning below the Real Living Wage #

# Source: Annual Survey of Hours and Earnings, ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018
# Licence: Open Government Licence

library(tidyverse) ; library(readxl)

url <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/009211annualsurveyofhoursandearningsasheestimatesofthenumberandproportionofemployeejobswithhourlypaybelowthelivingwagebyworkgeographylocalauthorityandparliamentaryconstituencyukapril2017andapril2018/20172018livingwagebyworkgeographyv2.zip"
download.file(url, dest = "20172018livingwagebyworkgeographyv2.zip")
unzip("20172018livingwagebyworkgeographyv2.zip", exdir = ".")
file.remove("20172018livingwagebyworkgeographyv2.zip")

df_2017 <- read_xls("Work Geography LW Table 7.1a   lpmgx 2017.xls", sheet = 2, skip = 4) %>% 
  filter(Description %in% c("England", "Greater Manchester Met County", "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  mutate(period = 2017) %>% 
  select(area_code = 2, area_name = 1, period, value = 4)

df_2018 <- read_xls("Work Geography LW Table 7.1a   lpmgx 2018.xls", sheet = 2, skip = 4) %>% 
  filter(Description %in% c("England", "Greater Manchester Met County", "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  mutate(period = 2018,) %>% 
  select(area_code = 2, area_name = 1, period, value = 4)

df <- bind_rows(df_2017, df_2018) %>% 
  mutate(area_name = case_when(area_name == "Greater Manchester Met County" ~ "Greater Manchester", TRUE ~ area_name),
         indicator = "Employees earning below the Real Living Wage",
         measure = "Percentage",
         unit = "Employees",
         value = round(as.numeric(value), 1)) %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

write_csv(df, "../real_living_wage.csv")
  