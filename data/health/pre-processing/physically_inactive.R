# Percentage of physically inactive adults #

# Source: PHE Fingertips (PHOF 2.13ii)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 93015) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 93015, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons", Age == "19+ yrs") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

districts <- fingertips_data(IndicatorID = 93015, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA (pre 4/19)"),
         Sex == "Persons", Age == "19+ yrs")

df <- bind_rows(gm, districts) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Percentage of physically inactive adults",
         measure = "Proportion",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../physically_inactive.csv")
