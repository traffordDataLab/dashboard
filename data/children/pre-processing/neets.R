# 16-17 year olds not in education, employment or training (NEET) or whose activity is not known #

# Source: PHE Fingertips (PHOF 1.05)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 93203) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 93203, AreaTypeID = 102, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

counties <- fingertips_data(IndicatorID = 93203, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA (pre 4/19)"),
         Sex == "Persons")

df <- bind_rows(gm, counties) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Proportion of 16-17 year olds not in education, employment or training (NEET)",
         measure = "Percentage",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../neets.csv")
