# Older people still at home 91 days after discharge #

# Source: LG Inform / NHS Digital
# URL: https://standards.esd.org.uk/?uri=metricType%2F1101
# Licence: Open Government Licence

library(tidyverse) ;  library(httr) 

tmp <- tempfile(fileext = ".csv")
GET(url = "http://webservices.esd.org.uk/data.csv?", 
               query = list(
                 ApplicationKey = "wUrlNGdzBDgSWArbbktkWghTHIOSfmCWnPvehEdO",
                 value.valueType = "raw",
                 metricType = "1101",
                 area = paste(c("E08000009", "E08000001", "E08000002", "E08000003", "E08000004", "E08000005", "E08000006", "E08000007", "E08000008", "E08000010"), collapse = ','),
                 period = "latest:8",
                 columnGrouping = "period",
                 rowGrouping = "area"),
    write_disk(tmp))

df <- read_csv(tmp) %>% 
  select(area_code = 1,
         `2010/11` = 2,
         `2011/12` = 3, 
         `2012/13` = 4,
         `2013/14` = 5,
         `2014/15` = 6,
         `2015/16` = 7,
         `2016/17` = 8,
         `2017/18` = 9) %>% 
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
         indicator = "Older people still at home 91 days after discharge",
         measure = "Proportion",
         unit = "Persons") %>% 
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(df, "../home_after_discharge.csv")
