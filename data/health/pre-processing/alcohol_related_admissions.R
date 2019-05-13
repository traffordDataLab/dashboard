# Alcohol related admissions (narrow measure) #

# Source: Public Health England (Fingertips)
# URL: https://fingertips.phe.org.uk/
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve correspondiong metadata
indicator_metadata(IndicatorID = 91414) %>% formattable::formattable()
# retrieve statistical neighbours for Trafford
neighbours <- nearest_neighbours(AreaCode = "E08000009", AreaTypeID = 101)

df <- fingertips_data(IndicatorID = 91414, AreaTypeID = 101) %>%
  filter(AreaCode %in% c("E08000009", neighbours), Sex == "Persons") %>%
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value) %>%
  mutate(indicator = "Alcohol related admissions (narrow measure)",
         measure = "Directly standardised rate",
         unit = "Persons") %>%
  select(area_code, area_name, indicator, period, measure, unit, value)

write_csv(df, "../alcohol_related_admissions.csv")



