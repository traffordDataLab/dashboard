# Flytipping #

# Source: fixmystreet.com
# URL: https://github.com/mysociety/fms_geographic_data
# Licence: Open Data Commons Attribution License
# NB for category lookup: https://github.com/mysociety/fms_meta_categories

urls <- c("https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2007.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2008.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2009.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2010.csv", 
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2011.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2012.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2013.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2014.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2015.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2016.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2017.csv",
          "https://github.com/mysociety/fms_geographic_data/raw/master/LAD/fixmystreet/SHEF_A/2018.csv"
)

df <- map_dfr(urls, read_csv) %>% 
  filter(SHEF_A_CODE == "A29",
         LAD %in% paste0("E0", seq(8000001, 8000010, 1))) %>% 
  mutate(area_name = 
           case_when(
             LAD == "E08000001" ~ "Bolton", 
             LAD == "E08000002" ~ "Bury", 
             LAD == "E08000003"	~ "Manchester",
             LAD == "E08000004" ~ "Oldham", 
             LAD == "E08000005" ~ "Rochdale", 
             LAD == "E08000006" ~ "Salford",
             LAD == "E08000007" ~ "Stockport", 
             LAD == "E08000008"	~ "Tameside", 
             LAD == "E08000009" ~	"Trafford",
             LAD == "E08000010" ~ "Wigan"),
         indicator = "Reports of flytipping on FixMyStreet.com",
         measure = "Count",
         unit = "Reports") %>% 
  select(area_code = LAD, area_name, period, indicator, measure, unit,
       value = report_count, group = SHEF_A_CODE)

write_csv(df, "../flytipping.csv")



