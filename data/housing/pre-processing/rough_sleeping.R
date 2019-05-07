# Rough sleeping #

# Source: MHCLG
# URL: https://www.gov.uk/government/statistics/rough-sleeping-in-england-autumn-2017
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/682004/Tables_1_and_2a_2b_2c_-_revised.xlsx",
          write_disk(tmp))

df <- read_xlsx(tmp, sheet = 3, skip = 3) %>% 
  filter(`Local authority / Region` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", 
                                           "Salford", "Tameside", "Stockport", "Trafford", "Wigan")) %>%
  select(area_code = `ONS code`, area_name = "Local authority / Region",
         4:11) %>%  
  gather(period, value, -c(area_code, area_name)) %>% 
  mutate(indicator = "Rough sleeping estimate",
         measure = "Count",
         unit = "Persons") %>% 
  select(area_code, area_name, 
         indicator, period, measure, unit, value)

write_csv(df, "../rough_sleeping.csv")
