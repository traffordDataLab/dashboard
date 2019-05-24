# Neighbourhood environmental problems #

# Source: fixmystreet.com
# URL: https://github.com/mysociety/fms_geographic_data
# Licence: Open Data Commons Attribution License

library(tidyverse)

lookup <- read_csv("https://github.com/mysociety/fms_meta_categories/raw/master/SHEF_C.csv") %>% 
  select(SHEF_C, SHEF_C_CODE) %>% 
  distinct()

urls <- c("https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2007.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2008.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2009.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2010.csv", 
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2011.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2012.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2013.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2014.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2015.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2016.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2017.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_C/2018.csv"
)

raw <- map_dfr(urls, read_csv) %>% 
  filter(LAD %in% paste0("E0", seq(8000001, 8000010, 1))) %>% 
  left_join(., lookup, by = "SHEF_C_CODE") %>% 
  select(area_code = LAD, period, reports = report_count, SHEF_C)

# mid-year population estimates (ONS)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2017detailedtimeseries/ukdetailedtimeseries2001to2017.zip"
download.file(url, dest = "ukdetailedtimeseries2001to2017.zip")
unzip("ukdetailedtimeseries2001to2017.zip")
file.remove("ukdetailedtimeseries2001to2017.zip")

pop <- read_csv("MYEB1_detailed_population_estimates_series_UK_(0117).csv") %>% 
  filter(lad2014_code %in% paste0("E0", seq(8000001, 8000010, 1)),
         Age %in% c(16:74)) %>% 
  select(-country, -sex, -Age) %>% 
  group_by(lad2014_code) %>% 
  select_if(is.numeric) %>% 
  summarise_all(funs(sum)) %>% 
  gather(period, population, -lad2014_code) %>% 
  mutate(period = str_replace(period, "population_", ""),
         period = as.integer(period)) %>% 
  rename(area_code = lad2014_code)

df <- left_join(raw, pop, by = c("area_code", "period")) %>% 
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
         indicator = "Neighbourhood environmental problems reported to fixmystreet.com",
         measure = "Rate per 100,000 population",
         unit = "Reports",
         value = (reports/population)*10000) %>% 
  select(area_code, area_name, period,
         indicator, measure, unit,
         value, group = SHEF_C)

write_csv(df, "../environmental_reports.csv")


