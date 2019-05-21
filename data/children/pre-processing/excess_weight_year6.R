# Child excess weight in 10-11 year olds #

# Source: PHE Fingertips (PHOF 2.06i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 20602) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 20602, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Children aged 10-11 years who have excess weight",
         measure = "Percentage",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../excess_weight_year6.csv")
