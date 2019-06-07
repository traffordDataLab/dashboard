# Gross Value Added (Balanced) #

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/regionalgrossvalueaddedbalancedbylocalauthorityintheuk
# Licence: Open Government Licence

library(tidyverse) ; library(httr) ; library(raedxl)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/regionalgrossvalueaddedbalancedbylocalauthorityintheuk/current/regionalgvabbylainuk.xlsx",
    write_disk(tmp))

abde <- read_xlsx(tmp, sheet = 6, skip = 1) %>%  filter(`LA name` == "Trafford")
c <- read_xlsx(tmp, sheet = 7, skip = 1) %>%  filter(`LA name` == "Trafford")
f <- read_xlsx(tmp, sheet = 8, skip = 1) %>%  filter(`LA name` == "Trafford")
ghi <- read_xlsx(tmp, sheet = 9, skip = 1) %>%  filter(`LA name` == "Trafford")
j <- read_xlsx(tmp, sheet = 10, skip = 1) %>%  filter(`LA name` == "Trafford")
k <- read_xlsx(tmp, sheet = 11, skip = 1) %>%  filter(`LA name` == "Trafford")
l <- read_xlsx(tmp, sheet = 12, skip = 1) %>%  filter(`LA name` == "Trafford")
mn <- read_xlsx(tmp, sheet = 13, skip = 1) %>%  filter(`LA name` == "Trafford")
opq <- read_xlsx(tmp, sheet = 14, skip = 1) %>%  filter(`LA name` == "Trafford")
rst <- read_xlsx(tmp, sheet = 15, skip = 1) %>%  filter(`LA name` == "Trafford")
total <- read_xlsx(tmp, sheet = 3, skip = 1) %>%  filter(`LA name` == "Trafford")

bind_rows(abde, c, f, ghi, j, k, l, mn, opq, rst, total) %>% 
  select(area_code = `LAU1 code`,
         area_name = `LA name`,
         group = `SIC07 code`,
         6:24) %>%
  mutate(group = case_when(
    group == "ABDE" ~ "Agriculture, mining and utilities",
    group == "C" ~ "Manufacturing",
    group == "F" ~ "Construction",
    group == "GHI" ~ "Distribution, transport, accommodation and food",
    group == "J" ~ "Information and communication",
    group == "K" ~ "Finance",
    group == "L" ~ "Real estate",
    group == "MN" ~ "Professional and business support services",
    group == "OPQ" ~ "Public services",
    group == "RST" ~ "Recreation and other services",
    group == "All" ~ "All industries",)) %>% 
  gather(period, value, -area_code, -area_name, -group) %>% 
  mutate(indicator = "Gross Value Added",
         measure = "Value",
         unit = "£") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value, group) %>%
  write_csv("../gva.csv")


