# Smoking Prevalence in adults in routine and manual occupations (18-64) - current smokers (APS) #

# Source: PHE Fingertips (Local Tobacco Control Profiles)
# URL: https://fingertips.phe.org.uk/profile/tobacco-control
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve correspondiong metadata
indicator_metadata(IndicatorID = 92445) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 92445, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Age == "18-64 yrs") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Smoking prevalence in adults in routine and manual occupations (18-64) - current smokers",
         measure = "Proportion",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../smoking_adults_manual.csv")
