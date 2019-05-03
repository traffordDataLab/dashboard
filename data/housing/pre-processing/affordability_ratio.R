# Median affordability ratio #

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(readxl)

# vacant dwellings by local authority district
tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian/current/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian.xls",
          write_disk(tmp))

trafford <- read_xls(tmp, sheet = 19, skip = 5) %>% 
  filter(Name == "Trafford") %>% 
  select(area_code = Code, 
         area_name = Name,
         5:27) %>% 
  gather(period, value, -c(area_code, area_name)) %>% 
  filter(period >= 2000) %>% 
  mutate(value = as.double(value))

gm_house_price <- read_xls(tmp, sheet = 17, range = "D32:Z41", col_names = c("area_name", seq(1997, 2018, 1))) %>% 
  gather(period, house_price, -area_name) %>% 
  filter(period >= 2000) %>% 
  mutate(house_price = as.double(house_price))

gm_earnings <- read_xls(tmp, sheet = 18, skip = 6) %>% 
  filter(Name %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale",
         "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  select(area_name = Name, 5:6, 8:27) %>% 
  gather(period, earnings, -area_name) %>% 
  filter(period >= 2000) %>% 
  mutate(earnings = as.double(earnings))

gm <- left_join(gm_house_price, gm_earnings, by = c("area_name", "period")) %>% 
  mutate(period = factor(period)) %>% 
  group_by(period) %>% 
  summarise(house_price = sum(house_price),
         earnings = sum(earnings),
         value = house_price / earnings) %>% 
  add_column(area_code = "E11000001",
             area_name = "Greater Manchester") %>% 
  select(area_code, area_name, period, value)

england <- read_xls(tmp, sheet = 7, skip = 5) %>% 
  filter(Name == "England") %>% 
  select(area_code = Code, 
         area_name = Name,
         3:25) %>% 
  gather(period, value, -c(area_code, area_name)) %>% 
  filter(period >= 2000)

df <- bind_rows(trafford, gm, england) %>% 
  mutate(indicator = "Median affordability ratio",
         measure = "Ratio",
         unit = "House price / earnings") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value)

write_csv(df, "../affordability_ratio.csv")
