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
                 period = "latest:62",
                 columnGrouping = "period",
                 rowGrouping = "area"),
    write_disk(tmp))

df <- read_csv(tmp) %>% 
  rename(area_code = 1) %>%
  gather(period, value, -area_code) %>% 
  mutate(area_name = 
           case_when(
             area_code == "E08000001" ~ "Bolton", 
             area_code == "E08000002" ~ "Bury", 
             area_code == "E08000003"	~ "Manchester",
             area_code == "E08000004" ~ "Oldham", 
             area_code == "E08000005" ~ "Rochdale", 
             area_code == "E08000006" ~ "Salford",
             area_code == "E08000007" ~ "Stockport", 
             area_code == "E08000008"	~ "Tameside", 
             area_code == "E08000009" ~	"Trafford",
             area_code == "E08000010" ~ "Wigan"),
         period = str_extract_all(period, "(?<=_)(.+)(?=\\}\\{m)"),
         indicator = "Daily DTOC beds attributable to adult social care ",
         measure = "Average daily",
         unit = "Days") %>% 
  unnest(period) %>% 
  mutate(period = anydate(period)) %>% 
  select(area_code, area_name, period, indicator, measure, unit, value) 

write_csv(df, "../dtoc.csv")
