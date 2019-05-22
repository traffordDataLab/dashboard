# Proportion of five year old children free from dental decay #

# Source: PHE Fingertips (PHOF 4.02)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 92441) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 92441, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

districts <- fingertips_data(IndicatorID = 92441, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons")

df <- bind_rows(gm, districts) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Proportion of five year old children free from dental decay",
         measure = "Percentage",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../dental_decay.csv")

