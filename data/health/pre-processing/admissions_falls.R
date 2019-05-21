# Emergency admissions due to falls in people aged 65 and over #

# Source: PHE Fingertips (PHOF 2.24i)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 22401) %>% formattable::formattable()

gm <- fingertips_data(IndicatorID = 22401, AreaTypeID = 101, ParentAreaTypeID = 126, rank = TRUE) %>% 
  filter(AreaCode == "E47000001",
         Sex == "Persons") %>% 
  mutate(AreaName = str_replace(AreaName, "CA-Greater Manchester", "Greater Manchester"))

districts <- fingertips_data(IndicatorID = 22401, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons")

df <- bind_rows(gm, districts) %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Emergency hospital admissions due to falls in people aged 65 and over",
         measure = "Age standardised rate per 100,000",
         unit = "Persons",
         value = round(value, 0)) %>% 
  select(-significance, everything()) 

write_csv(df, "../admissions_falls.csv")
