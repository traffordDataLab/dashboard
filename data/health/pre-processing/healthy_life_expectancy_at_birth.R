# Healthy life expectancy at birth #

# Source: PHE Fingertips (PHOF 0.1i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve correspondiong metadata
indicator_metadata(IndicatorID = 90362) %>% formattable::formattable()
# retrieve statistical neighbours for Trafford
neighbours <- nearest_neighbours(AreaCode = "E08000009", 
                                 AreaTypeID = 102, measure = "CIPFA")

df <- fingertips_data(IndicatorID = 90362, AreaTypeID = 102) %>%
  filter(AreaCode %in% c("E08000009", neighbours)) %>%
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         group = Sex) %>%
  mutate(period = str_replace_all(period, fixed(" "), ""),
         indicator = "Healthy life expectancy at birth",
         measure = "Life expectancy",
         unit = "Years") %>%
  select(area_code, area_name, indicator, period, measure, unit, value, group)

write_csv(df, "../healthy_life_expectancy_at_birth.csv")



