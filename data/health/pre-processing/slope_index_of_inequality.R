# Slope index of inequality in life expectancy #

# Source: PHE Fingertips (PHOF 0.2vi)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve correspondiong metadata
indicator_metadata(IndicatorID = 92901) %>% formattable::formattable()
# retrieve statistical neighbours for Trafford
neighbours <- nearest_neighbours(AreaCode = "E08000009", 
                                 AreaTypeID = 102, measure = "CIPFA")

df <- fingertips_data(IndicatorID = 92901, AreaTypeID = 102) %>%
  filter(AreaCode %in% c("E08000009", neighbours)) %>%
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         group = Sex) %>%
  mutate(period = str_replace_all(period, fixed(" "), ""),
         indicator = "Inequality in life expectancy at birth",
         measure = "Slope Index of Inequality",
         unit = "Years") %>%
  select(area_code, area_name, indicator, period, measure, unit, value, group) %>% 
  filter(period >= "2010-12")

write_csv(df, "../slope_index_of_inequality.csv")



