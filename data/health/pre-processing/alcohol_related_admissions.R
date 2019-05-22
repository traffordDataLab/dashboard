# Alcohol related admissions (narrow measure) #

# Source: PHE Fingertips (Local Alcohol Profiles for England)
# URL: https://fingertips.phe.org.uk/profile/local-alcohol-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 91414) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 91414, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

districts <- fingertips_data(IndicatorID = 91414, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons")

df <- bind_rows(gm, districts) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Alcohol related admissions (narrow measure)",
         measure = "Standardised rate per 100,000",
         unit = "Persons",
         value = round(value, 0)) %>% 
  select(-significance, everything()) 

write_csv(df, "../alcohol_related_admissions.csv")



