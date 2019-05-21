# Children in low income families (under 16 years) #

# Source: PHE Fingertips (PHOF 1.01i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 10101) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 10101, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA")) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Children in low income families",
         measure = "Percentage",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../low_income_families.csv")
