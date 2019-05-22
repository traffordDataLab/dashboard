# Slope index of inequality in life expectancy #

# Source: PHE Fingertips (PHOF 0.2vi)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 92901) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 92901, AreaTypeID = 102, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

counties <- fingertips_data(IndicatorID = 92901, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"))

df <- bind_rows(gm, counties) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         group = Sex,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(period = str_replace_all(period, fixed(" "), ""),
         indicator = "Inequality in life expectancy at birth",
         measure = "Slope Index of Inequality",
         unit = "Years",
         value = round(value, 1)) %>% 
  select(-group, -significance, everything()) %>% 
  filter(period >= "2010-12")

write_csv(df, "../slope_index_of_inequality.csv")
