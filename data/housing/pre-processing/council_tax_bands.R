# New properties by council tax band, 2010-19 #

# Source: Valuation Office Agency
# URL: https://www.gov.uk/government/statistics/council-tax-stock-of-properties-2019
# Licence: Open Government Licence v3.0

library(tidyverse)

df <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/832578/Table_CTSOP4_0_2019.csv") %>% 
  select(area_code = ECODE, area_name = AREA_NAME, band = BAND, BP_2010_2019) %>% 
  filter(area_name %in% c("ENGLAND", "Greater Manchester (Met County)", "Trafford") & band != "All")  %>% 
  mutate(area_name = case_when(area_name == "ENGLAND" ~ "England",
                               area_name == "Greater Manchester (Met County)" ~ "Greater Manchester", 
                               area_name == "Trafford" ~ "Trafford")) %>% 
  group_by(area_name) %>% 
  mutate(BP_2010_2019 = as.integer(str_remove(BP_2010_2019, ",")),
         value = BP_2010_2019/sum(BP_2010_2019),
         indicator = "New properties by council tax band",
         period = "2010 to 2019",
         measure = "Percentage",
         unit = "Properties") %>% 
  select(area_code, area_name, indicator, period, measure, unit, band, value)
  
write_csv(df, "../council_tax_bands.csv")
