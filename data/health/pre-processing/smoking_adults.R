# Smoking prevalence in adults - current smokers (APS) #

# Source: PHE Fingertips (PHOF 2.14)
# URL: https://fingertips.phe.org.uk/profile/local-alcohol-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 92443) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 92443, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons", Age == "18+ yrs") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Smoking Prevalence in adults - current smokers (APS)",
         measure = "Proportion",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../smoking_adults.csv")
