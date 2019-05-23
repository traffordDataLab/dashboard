# Crime Severity Score #

# Source: Home Office
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/crimeseverityscoreexperimentalstatistics
# Licence: Open Government Licence v3.0

library(tidyverse); library(readxl)

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/crimeseverityscoreexperimentalstatistics/current/datatool.xls",
    write_disk(tmp))

trafford <- read_xls(tmp, sheet = 13) %>% 
  select(c(1:3, 5:22)) %>% 
  set_names(.[1, ]) %>%
  slice(-1) %>% 
  filter(`Community safety partnership` == "Trafford") %>% 
  select(area_code = Code,
         area_name = `Community safety partnership`,
         group = `Offence group`,
         everything()) %>% 
  gather(period, value, -area_code, -area_name, -group) %>% 
  mutate(period = case_when(
    period == "Apr '08 to \nMar '09" ~ "2008/09",
    period == "Apr '09 to \nMar '10" ~ "2009/10",
    period == "Apr '10 to \nMar '11" ~ "2010/11",
    period == "Apr '11 to \nMar '12" ~ "2011/12",
    period == "Apr '12 to \nMar '13" ~ "2012/13",
    period == "Apr '13 to \nMar '14" ~ "2013/14",
    period == "Apr '14 to \nMar '15" ~ "2014/15",
    period == "Apr '15 to \nMar '16" ~ "2015/16",
    period == "Apr '16 to \nMar '17" ~ "2016/17",
    period == "Apr '17 to \nMar '18" ~ "2017/18",
    TRUE ~ "exclude"),
    indicator = "Crime Severity Score",
    measure = "Score",
    unit = "Offences"
  ) %>% 
  filter(period != "exclude") %>% 
  select(-group, everything())

gm_and_england <- read_xls(tmp, sheet = 12) %>% 
  select(1:21) %>% 
  set_names(.[1, ]) %>%
  slice(-1) %>% 
  filter(Name %in% c("ENGLAND", "Greater Manchester")) %>% 
  select(area_code = Code,
         area_name = Name,
         group = `Offence group`,
         everything()) %>% 
  gather(period, value, -area_code, -area_name, -group) %>% 
  mutate(area_name = str_to_title(area_name),
         period = case_when(
           period == "Apr '08 to \nMar '09" ~ "2008/09",
           period == "Apr '09 to \nMar '10" ~ "2009/10",
           period == "Apr '10 to \nMar '11" ~ "2010/11",
           period == "Apr '11 to \nMar '12" ~ "2011/12",
           period == "Apr '12 to \nMar '13" ~ "2012/13",
           period == "Apr '13 to \nMar '14" ~ "2013/14",
           period == "Apr '14 to \nMar '15" ~ "2014/15",
           period == "Apr '15 to \nMar '16" ~ "2015/16",
           period == "Apr '16 to \nMar '17" ~ "2016/17",
           period == "Apr '17 to \nMar '18" ~ "2017/18",
           TRUE ~ "exclude"),
         indicator = "Crime Severity Score",
         measure = "Score",
         unit = "Offences"
         ) %>% 
  filter(period != "exclude") %>% 
  select(-group, everything())

df <- bind_rows(trafford, gm_and_england)

write_csv(df, "../crime_severity.csv")

