# CO2 emissions #

# Source: Department for Business, Energy & Industrial Strategy
# URL: https://www.gov.uk/government/collections/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics
# Licence: Open Government Licence

library(tidyverse) ; library(httr) ; library(readxl)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/812142/2005-17_UK_local_and_regional_CO2_emissions_tables.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 2, skip = 1) %>% 
  filter(LAD14NM == "Trafford") %>% 
  select(area_code = LAD14NM,
         area_name = LAD14CD,
         period = Year, 
         `Industrial and commercial` = `Industry and Commercial Total`,
         Domestic = `Domestic Total`,
         Transport = `Transport Total`,
         Total = `Grand Total`) %>% 
  gather(group, value, -area_code, -area_name, -period) %>% 
  mutate(indicator = "CO2 emissions",
         measure = "CO2",
         unit = "kt") %>% 
  select(area_code, area_name, 
         indicator, period, measure, unit, value, group)

write_csv(df, "../co2_emissions.csv")
         
