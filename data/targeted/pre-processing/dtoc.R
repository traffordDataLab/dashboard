# Daily DTOC beds attributable to adult social care  #

# Source: LG Inform / NHS England
# URL: https://standards.esd.org.uk/?uri=metricType%2F8499
# Licence: Open Government Licence

library(tidyverse) ;  library(httr) ; library(stringr) ; library(anytime)

tmp <- tempfile(fileext = ".csv")
GET(url = "http://webservices.esd.org.uk/data.csv?", 
               query = list(
                 ApplicationKey = "wUrlNGdzBDgSWArbbktkWghTHIOSfmCWnPvehEdO",
                 value.valueType = "raw",
                 metricType = "8499",
                 area = paste(c("E08000009", "E08000001", "E08000002", "E08000003", "E08000004", "E08000005", "E08000006", "E08000007", "E08000008", "E08000010"), collapse = ','),
                 period = "latest:36",
                 columnGrouping = "period",
                 rowGrouping = "area",
                 summary.summaryType = "mean", # mean for Greater Manchester districts
                 summary.summaryFor = "area"),
    write_disk(tmp))

df <- read_csv(tmp) %>% 
  rename(area_code = 1) %>%
  gather(period, value, -area_code) %>% 
  filter(area_code %in% c("E08000009", "[summary][mean][area]")) %>% 
  mutate(area_code = 
           case_when(
             area_code == "[summary][mean][area]" ~ "E47000001",
             TRUE ~ "E08000009"),
         area_name = 
           case_when(
             area_code == "E47000001" ~ "Greater Manchester",
             TRUE ~	"Trafford"),
         period = str_extract_all(period, "(?<=_)(.+)(?=\\}\\{m)")) %>% 
  unnest(period) %>% 
  mutate(period = anydate(period),
         indicator = "Daily DTOC beds attributable to adult social care ",
         measure = "Average daily",
         unit = "Days") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value) 

write_csv(temp, "../dtoc.csv")
