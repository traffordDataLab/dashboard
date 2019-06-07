# Licensed vehicles #

# Source: Department for Transport and Driver and Vehicle Licensing Agency 
# URL: https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr);  library(readODS)

# Diesel vehicles
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/794433/veh0105.ods",
    write_disk(tmp))

sheets <- tmp %>%
  ods_sheets() %>%
  set_names() %>% 
  map_df(~ read_ods(path = tmp, sheet = .x, 
                    col_names = TRUE, col_types = NA, skip = 7), .id = "sheet")

diesel <- sheets %>% 
  filter(`Region/Local Authority` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  mutate(value = as.numeric(`Diesel Cars`)+as.numeric(`Diesel Vans`),
         value = value*1000,
         group = "Diesel vehicles") %>% 
  select(area_code = `ONS LA Code`, 
         area_name = `Region/Local Authority`, 
         period = sheet, 
         value, 
         group)

# Electric vehicles
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/794447/veh0131.ods",
    write_disk(tmp))

electric <- read_ods(tmp, skip = 6)  %>%  
  filter(`Region/Local Authority` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  rename(area_code = `ONS LA Code`, area_name = `Region/Local Authority`) %>% 
  gather(period, value, -area_code, -area_name) %>% 
  mutate(period = factor(parse_number(period)),
         value = as.numeric(na_if(value, "c"))) %>% 
  group_by(period, area_code, area_name) %>% 
  summarise(value = sum(value)) %>% 
  mutate(group = "Electric vehicles")

# Ultra low emission vehicles  
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/794448/veh0132.ods",
    write_disk(tmp))

ultra_low <- read_ods(tmp, skip = 6)  %>%  
  filter(`Region/Local Authority` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  rename(area_code = `ONS LA Code`, area_name = `Region/Local Authority`) %>% 
  gather(period, value, -area_code, -area_name) %>% 
  mutate(period = factor(parse_number(period)),
         value = as.numeric(na_if(value, "c"))) %>% 
  group_by(period, area_code, area_name) %>% 
  summarise(value = sum(value)) %>% 
  mutate(group = "Ultra low emission vehicles")

df <- bind_rows(diesel, electric, ultra_low) %>% 
  mutate(indicator = "Vehicles",
         measure = "Frequency",
         unit = "Vehicles") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value, group)

write_csv(df, "../licensed_vehicles.csv")
