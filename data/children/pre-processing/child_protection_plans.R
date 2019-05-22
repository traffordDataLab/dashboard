# Children on child protection plans: Rate per 10,000 children <18 #

# Source: PHE Fingertips (Child and Maternal Health)
# URL: https://fingertips.phe.org.uk/profile/child-health-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 90886) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 90886, AreaTypeID = 102, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

counties <- fingertips_data(IndicatorID = 90886, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"),
         Sex == "Persons")

df <- bind_rows(gm, counties) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Children on child protection plans: Rate per 10,000 children <18",
         measure = "per 10,000",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../child_protection_plans.csv")

