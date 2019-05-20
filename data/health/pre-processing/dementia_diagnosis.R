# Estimated dementia diagnosis rate (aged 65+) #

# Source: PHE Fingertips (PHOF 2.13ii)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 92949) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 92949, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA")) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Estimated dementia diagnosis rate (aged 65 and over)",
         measure = "Proportion",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../dementia_diagnosis.csv")
