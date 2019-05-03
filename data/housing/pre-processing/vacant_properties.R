# Long term vacant proprties #

# Source: MHCLG
# URL: https://www.gov.uk/government/statistical-data-sets/live-tables-on-dwelling-stock-including-vacants
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)

# vacant dwellings by local authority district
tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/784593/LT_615.xls",
          write_disk(tmp))
vacant <- read_xls(tmp, sheet = 3, range = "D6:U390") %>% 
  filter(row_number() == 2 | 
         `New ONS code` %in% c("E08000009", "E11000001")) %>% 
  select(area_code = `New ONS code`,
         area_name = `Local Authority Name`,
         4:18) %>% 
  mutate(area_code = case_when(is.na(area_code) ~ "E92000001", TRUE ~ area_code),
         area_name = case_when(area_code == "E92000001" ~ "England",
                               area_code == "E11000001" ~ "Greater Manchester",
                               TRUE ~ area_name)) %>% 
  gather(period, vacant, -c(area_code, area_name)) %>% 
  mutate(vacant = as.integer(vacant)) %>% 
  filter(period >= "2004", period < "2018") 

# dwellings by local authority district
tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/710193/LT_125.xls",
          write_disk(tmp))
dwellings <- read_xls(tmp, sheet = 1, range = "A6:U79") %>% 
  filter(row_number() == 2 | 
           `New ONS code` %in% c("E08000009", "E11000001")) %>% 
  select(area_code = `New ONS code`,
         area_name = 4,
         5:21) %>% 
  mutate(area_code = case_when(is.na(area_code) ~ "E92000001", TRUE ~ area_code),
         area_name = case_when(area_code == "E92000001" ~ "England",
                               area_code == "E11000001" ~ "Greater Manchester",
                               TRUE ~ area_name)) %>% 
  gather(period, dwellings, -c(area_code, area_name)) %>% 
  mutate(dwellings = as.integer(dwellings),
         period = substring(period, 1, 4)) %>% 
  filter(period >= "2004") %>% 
  select(-area_code)
  
# match data
df <- left_join(dwellings, vacant, by = c("area_name", "period")) %>% 
  mutate(value = vacant / dwellings,
         indicator = "Long term vacant proprties",
         measure = "Percentage",
         unit = "Dwellings") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)
  
write_csv(df, "../vacant_properties.csv")
