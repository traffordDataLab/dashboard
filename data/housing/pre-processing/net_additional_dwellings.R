# Net additional dwellings #

# Source: MHCLG
# URL: https://www.gov.uk/government/statistical-data-sets/live-tables-on-net-supply-of-housing
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)

tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/756094/Live_Table_122.xls",
    write_disk(tmp))

df <- read_xls(tmp, skip = 3) %>% 
  filter(`Current\nONS code` %in% c("E08000009", "E11000001")) %>% 
  bind_rows(filter(read_xls(tmp, skip = 3), `...1` == "England")) %>% 
  mutate(area_code = `Current\nONS code`,
         area_code =
           case_when(is.na(area_code) ~ "E92000001",
                     TRUE ~ area_code),
         area_name =
           case_when(area_code == "E08000009" ~ "Trafford",
                     area_code == "E11000001" ~ "Greater Manchester",
                     TRUE ~ "England"),
         indicator = "Net additional dwellings",
         measure = "Count",
         unit = "Dwellings") %>% 
  select(area_code, area_name, indicator, measure, unit, 8:24) %>% 
  gather(period, value, -c(area_code, area_name, indicator, measure, unit)) %>% 
  mutate(period = str_remove_all(period, "P"),
         value = round(value, 0))
         
write_csv(df, "../net_additional_dwellings.csv")
