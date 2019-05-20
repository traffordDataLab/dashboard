# Excess under 75 mortality rate in adults with serious mental illness #

# Source: PHE Fingertips (PHOF 2.24i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 91096) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 91096, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"),
         Sex == "Persons") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Excess under 75 mortality rate in adults with serious mental illness",
         measure = "Indirectly standardised ratio",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../mortality_serious_mental_illness.csv")
