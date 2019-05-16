# Alcohol related admissions (narrow measure) #

# Source: PHE Fingertips (Local Alcohol Profiles for England)
# URL: https://fingertips.phe.org.uk/profile/local-alcohol-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve correspondiong metadata
indicator_metadata(IndicatorID = 91414) %>% formattable::formattable()
z
df <- fingertips_data(IndicatorID = 91414, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"), 
         Sex == "Persons") %>% 
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



