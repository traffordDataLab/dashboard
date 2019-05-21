# Hospital admissions as a result of self-harm (10-24 years) #

# Source: PHE Fingertips (Child and Maternal Health)
# URL: https://fingertips.phe.org.uk/profile/child-health-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 90813) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 90813, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"),
         Sex == "Persons") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Hospital admissions as a result of self-harm (10-24 years)",
         measure = "Age standardised rate per 100,000",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../admissions_self_harm_young_people.csv")
