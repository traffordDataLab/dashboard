# Children in care: rate per 10,000 population aged < 18 #

# Source: PHE Fingertips (Child and Maternal Health)
# URL: https://fingertips.phe.org.uk/profile/child-health-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 90803) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 90803, AreaTypeID = 102, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

counties <- fingertips_data(IndicatorID = 90803, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"))

df <- bind_rows(gm, counties) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Children in care: rate per 10,000 population aged < 18",
         measure = "per 10,000",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../children_in_care.csv")

