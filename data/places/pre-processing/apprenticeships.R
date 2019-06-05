# Apprenticeship starts #

# Source: Department for Education
# URL: https://www.gov.uk/government/statistical-data-sets/fe-data-library-apprenticeships
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)

lookup <- tibble(
  area_name = c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"),
  area_code = paste0("E0", seq(8000001, 8000010, 1))
)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/694149/201314_201617_Apprenticeship_starts_by_SSA_gender_geography_and_age.xlsx",
    write_disk(tmp))

Y1314 <- read_xlsx(tmp, sheet = 2, range = anchored("B990", dim = c(327, 40)), 
                    col_names = FALSE, na = "-") %>% select(area_name = 1, value = 40) %>% 
  filter(area_name %in% lookup$area_name) %>% 
  mutate(period = "2013-14") %>% select(area_name, period, value)

Y1415 <- read_xlsx(tmp, sheet = 3, range = anchored("B991", dim = c(327, 40)), 
                    col_names = FALSE, na = "-") %>% select(area_name = 1, value = 40) %>% 
  filter(area_name %in% lookup$area_name) %>%
  mutate(period = "2014-15") %>% select(area_name, period, value)

Y1516 <- read_xlsx(tmp, sheet = 4, range = anchored("B991", dim = c(327, 39)), 
                    col_names = FALSE, na = "-") %>% select(area_name = 1, value = 39) %>% 
  filter(area_name %in% lookup$area_name) %>%
  mutate(period = "2015-16") %>% select(area_name, period, value)

Y1617 <- read_xlsx(tmp, sheet = 5, range = anchored("B990", dim = c(327, 42)), 
                    col_names = FALSE, na = "-") %>% select(area_name = 1, value = 42) %>% 
  filter(area_name %in% lookup$area_name) %>%
  mutate(period = "2016-17") %>% select(area_name, period, value)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/765570/Apprenticeship_starts_by_delivery_provider_home_away_201718.xlsx",
    write_disk(tmp))

Y1718 <- read_xlsx(tmp, sheet = 2, skip = 5, na = "-") %>% 
  filter(`Local Authority District Where Learning is Delivered` %in% lookup$area_name &
           UKPRN != "Total") %>% 
  select(area_name = `Local Authority District Where Learning is Delivered`,
         value = Starts...5) %>% 
  group_by(area_name) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  mutate(period = "2017-18") %>%
  select(area_name, period, value)

bind_rows(Y1314, Y1415, Y1516, Y1617, Y1718) %>% 
  left_join(lookup, by = "area_name") %>% 
  mutate(indicator = "Apprenticeship starts",
         measure = "Count",
         unit = "Starts") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value) %>% 
  write_csv("../apprenticeships.csv")


