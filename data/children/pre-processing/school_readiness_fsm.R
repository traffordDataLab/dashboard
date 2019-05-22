# School readiness: Good level of development at age 5 with free school meal status #

# Source: PHE Fingertips (Public Health England Health Improvement)
# URL: https://fingertips.phe.org.uk/profile/child-health-profiles
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 90632) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 90632, AreaTypeID = 102, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

counties <- fingertips_data(IndicatorID = 90632, AreaTypeID = 102, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "County & UA"),
         Sex == "Persons")

df <- bind_rows(gm, counties) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Children with free school meal status achieving a good level of development at the end of reception",
         measure = "Percentage",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../school_readiness_fsm.csv")
