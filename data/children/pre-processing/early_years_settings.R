# Early Years Settings Rated "Good" or “Outstanding" by OFSTED at most recent inspection #

# Source: Ofsted
# URL: https://www.gov.uk/government/statistics/childcare-providers-and-inspections-as-at-31-august-2018
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl) ; library(janitor)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/761773/Childcare_providers_and_inspections_charts_and_tables_as_at_31_August.xlsx",
    write_disk(tmp))

raw <- read_xlsx(tmp, sheet = 19, range = "B9:H33",
                col_names = c("area_name", "providers", "inspected",
                              "Outstanding", "Good", "Requires improvement",
                              "Inadequate"))
                              
df <- raw %>% 
  filter(area_name %in%
           c("Bolton", "Bury", "Manchester", "Oldham", 
             "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  adorn_totals("row", name = "Greater Manchester") %>% 
  bind_rows(., filter(raw, area_name == "All England")) %>% 
  mutate(area_name = str_replace(area_name, "All England", "England")) %>% 
  filter(area_name %in% c("Trafford", "Greater Manchester", "England")) %>% 
  select(-providers) %>% 
  gather(group, n, -area_name,  -inspected) %>% 
  group_by(area_name, group) %>%
  mutate(area_code = 
           case_when(
             area_name == "Trafford" ~ "E08000009",
             area_name == "Greater Manchester" ~ "E47000001",
             area_name == "England" ~ "E92000001"
             ),
         period = "2018-08",
         value = round((n / inspected),3),
         indicator = "Early Years Settings Rated 'Good' or 'Outstanding' by Ofsted",
         measure = "Percentage",
         unit = "Early Years Settings") %>% 
  select(area_code, area_name, period, value, indicator, measure, unit, group)

write_csv(df, "../early_years_settings.csv")


