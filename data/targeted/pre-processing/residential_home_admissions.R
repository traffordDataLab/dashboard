# Residential home admissions #

# Source: LG Inform / NHS Digital
# URL: https://standards.esd.org.uk/?uri=metricType%2F4282
# Licence: Open Government Licence

library(tidyverse) ;  library(httr) 

tmp <- tempfile(fileext = ".csv")
GET(url = "http://webservices.esd.org.uk/data.csv?", 
    query = list(
      ApplicationKey = "wUrlNGdzBDgSWArbbktkWghTHIOSfmCWnPvehEdO",
      value.valueType = "raw",
      metricType = "4282",
      area = paste(c("E08000009", "E08000001", "E08000002", "E08000003", "E08000004", "E08000005", "E08000006", "E08000007", "E08000008", "E08000010"), collapse = ','),
      period = "latest:5",
      columnGrouping = "period",
      rowGrouping = "area"),
    write_disk(tmp))

df <- read_csv(tmp) %>% 
  select(area_code = 1,
         `2014/15` = 2,
         `2015/16` = 3,
         `2016/17` = 4,
         `2017/18` = 5,
         `2018/19` = 6) %>% 
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
         indicator = "Residential home admissions",
         measure = "Rate per 100,000 population",
         unit = "Persons") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(df, "../residential_home_admissions.csv")
