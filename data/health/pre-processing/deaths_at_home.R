# Percentage of deaths in usual place of residence #

# Source: PHE Fingertips (End of Life Care Profiles)
# URL: https://fingertips.phe.org.uk/profile/end-of-life
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 92727) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 92727, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

districts <- fingertips_data(IndicatorID = 92727, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"))

df <- bind_rows(gm, districts) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Percentage of deaths in usual place of residence among people aged 65 years and over",
         measure = "Percentage",
         unit = "Deaths",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../deaths_at_home.csv")
