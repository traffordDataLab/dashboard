# Primary schools rated by OFSTED at most recent inspection #

# Source: Ofsted
# URL: https://www.gov.uk/government/statistics/state-funded-schools-inspections-and-outcomes-as-at-31-december-2018
# Licence: Open Government Licence v3.0

library(tidyverse)

df <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/789898/State_funded_schools_inspections_and_outcomes_as_at_31_December_2018.csv") %>% 
  filter(`Local authority` == "Trafford" & 
         `Phase of education` == "Primary",
         `Does the latest full inspection relate to the URN of the current school?` == "Yes") %>% 
  mutate(url = str_c("https://reports.ofsted.gov.uk/provider/21/", URN),
         inspection_date = as.Date(`Inspection end date`, format = "%d/%m/%Y")) %>% 
  select(name = `School name at time of latest full inspection`,
         type = `Type of education`,
         inspection_date,
         rating = `Overall effectiveness`,
         url,
         postcode = `Postcode`) %>% 
  mutate(rating = case_when(
    rating == "1" ~ "Outstanding",
    rating == "2" ~ "Good",
    rating == "3" ~ "Satisfactory / Requires improvement",
    rating == "4" ~ "Inadequate",
    TRUE ~ "NA"
  ))

postcodes <- read_csv("https://www.trafforddatalab.io/spatial_data/postcodes/trafford_postcodes.csv") %>% 
  select(postcode, lon, lat)

left_join(df, postcodes, by = "postcode") %>% 
  write_csv("../primary_schools.csv")
