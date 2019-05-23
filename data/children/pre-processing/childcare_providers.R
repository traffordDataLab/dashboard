# Childcare providers rated by OFSTED at most recent inspection #

# Source: Ofsted
# URL: https://www.gov.uk/government/statistics/childcare-providers-and-inspections-as-at-31-december-2018
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(readODS)

raw <- read_ods("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/788933/Childcare_provider_level_data_as_at_31_December_2018.ods", sheet = 4) %>% 
  filter(`Provider status` == "Active" & 
           `Local authority` == "Trafford" & 
           `Provider type` == "Childcare on non-domestic premises") %>% 
  mutate(url = str_c("http://www.ofsted.gov.uk/inspection-reports/find-inspection-report/provider/CARE/", `Provider URN`),
         inspection_date = as.Date(`Most recent full: Inspection date`, format = "%d/%m/%Y")) %>% 
  select(name = `Provider name`,
         inspection_date,
         rating = `Most recent full: Overall effectiveness`,
         url,
         postcode = `Postcode`) %>% 
  mutate(rating = case_when(
    rating == "1" ~ "Outstanding",
    rating == "2" ~ "Good",
    rating == "3" ~ "Satisfactory / Requires improvement",
    rating == "4" ~ "Inadequate",
    TRUE ~ "NA"
  ))

# Latest postcode centroids #

# Source: ONS Postcode Directory
# Licence: Open Government Licence v3.0
postcodes <- selectread_csv("https://www.trafforddatalab.io/spatial_data/postcodes/trafford_postcodes.csv") %>% 
  select(postcode, lon, lat)

df <- left_join(raw, postcodes, by = "postcode")

write_csv(df, "../childcare_providers.csv")
