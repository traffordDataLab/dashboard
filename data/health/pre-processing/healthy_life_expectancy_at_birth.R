# Healthy life expectancy at birth #

# Source: PHE Fingertips (PHOF 0.1i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0
library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 90362) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 90362, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA")) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         group = Sex,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(period = str_replace_all(period, fixed(" "), ""),
         indicator = "Healthy life expectancy at birth",
         measure = "Life expectancy",
         unit = "Years",
         value = round(value, 1)) %>% 
  select(-group, -significance, everything()) 

write_csv(df, "../healthy_life_expectancy_at_birth.csv")
