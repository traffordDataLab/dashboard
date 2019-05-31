# Flytipping #

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env24-fly-tipping-incidents-and-actions-taken-in-england
# Licence: Open Government Licence

library(tidyverse) ; library(readxl)

url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/765449/fly_tipping_datasets_in_Excel_rev_3.zip"
download.file(url, dest = "fly_tipping_datasets_in_Excel_rev_3.zip")
unzip("fly_tipping_datasets_in_Excel_rev_3.zip", exdir = ".")
file.remove("fly_tipping_datasets_in_Excel_rev_3.zip")

england <- read_xlsx("Flytipping_incidents_actions_taken_reported_by_LAs_in_England_201213_to_201718_Dec_rev.xlsx", sheet = 3, skip = 2) %>%
  filter(Region == "*Total England") %>% 
  mutate(area_code = case_when(`ONS Code`== "*Total" ~ "E92000001", TRUE ~ `ONS Code`),
         area_name = case_when(`LA Name`== "*Total" ~ "England", TRUE ~ `LA Name`),
         value = as.integer(`Total Incidents`)) %>% 
  select(area_code, area_name,
         period = Year,
         value) 

gm <- read_xlsx("Flytipping_incidents_actions_taken_reported_by_LAs_in_England_201213_to_201718_Dec_rev.xlsx", sheet = 3, skip = 2) %>%
  filter(`ONS Code` %in% paste0("E0", seq(8000001, 8000010, 1))) %>% 
  select(area_code = `ONS Code`, 
         area_name = `LA Name`,
         period = Year,
         value = `Total Incidents`) %>% 
  mutate(value = as.integer(value)) %>% 
  spread(period, value) %>% 
  janitor::adorn_totals(where = "row", fill = "Greater Manchester", na.rm = TRUE, name = "E47000001") %>% 
  gather(period, value, -area_code, -area_name) %>% 
  filter(area_name %in% c("Greater Manchester", "Trafford"))

df <- bind_rows(england, gm) %>% 
  mutate(indicator = "Fly-tipping incidents",
         measure = "Count",
         unit = "Incidents") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(df, "../flytipping.csv")

