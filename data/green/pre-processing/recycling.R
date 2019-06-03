# Recycling #

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables
# Licence: Open Government Licence

library(tidyverse) ; library(httr) ; library(readxl)

tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/766014/LA_and_Regional_Spreadsheet_201718_rev2.xlsx",
    write_disk(tmp))
df <- read_xlsx(tmp, sheet = 8, skip = 3) 

england <- read_xlsx(tmp, sheet = 8, skip = 3) %>% 
  filter(Region == "Total England") %>% 
  mutate(area_code = "E92000001", 
         area_name = "England") %>% 
  select(area_code, area_name,
         period = Year, 
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

df <- read_xlsx(tmp, sheet = 8, skip = 3) %>% 
  filter(`ONS code` %in% c("E50000005", "E08000009")) %>% 
  mutate(area_code = case_when(`ONS code`== "E50000005" ~ "E47000001", TRUE ~ `ONS code`),
         area_name = case_when(area_code == "E47000001" ~ "Greater Manchester", 
                               area_code == "E08000009" ~ "Trafford")) %>% 
  select(area_code, 
         area_name,
         period = Year, 
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`) %>% 
  bind_rows(england) %>% 
  mutate(indicator = "Reuse, recycling or composting of household waste",
         measure = "Percentage",
         unit = "Waste") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value) %>% 
  arrange(period)
  
write_csv(df, "../recycling.csv")
  
  
