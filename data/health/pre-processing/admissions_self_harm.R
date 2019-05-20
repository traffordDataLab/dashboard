# Emergency hospital admissions for intentional self-harm #

# Source: PHE Fingertips (PHOF 2.10ii)
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(fingertipsR)

# retrieve indicators
select_indicators()
# retrieve corresponding metadata
indicator_metadata(IndicatorID = 21001) %>% formattable::formattable()

df <- fingertips_data(IndicatorID = 21001, AreaTypeID = 101, rank = TRUE) %>% 
  filter(AreaType %in% c("England", "District & UA"),
         Sex == "Persons") %>% 
  select(area_code = AreaCode,
         area_name = AreaName,
         period = Timeperiod,
         value = Value,
         significance = ComparedtoEnglandvalueorpercentiles) %>%
  mutate(indicator = "Emergency Hospital Admissions for Intentional Self-Harm",
         measure = "Age standardised rate per 100,000",
         unit = "Persons",
         value = round(value, 1)) %>% 
  select(-significance, everything()) 

write_csv(df, "../admissions_self_harm.csv")
