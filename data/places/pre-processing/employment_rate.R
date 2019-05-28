# Employment rate - aged 16-64 #

# Source: Annual Population Survey, Office for National Statistics
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=17
# Licence: Open Government Licence v3.0

df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=E92000001,E11000001,E08000009&date=latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=45&measures=20599,21001,21002,21003&select=date_name,geography_name,geography_code,variable_name,measures_name,obs_value") %>% 
  filter(MEASURES_NAME == "Variable") %>%
  mutate(area_name = case_when(GEOGRAPHY_NAME == "Greater Manchester (Met County)" ~ "Greater Manchester", TRUE ~ GEOGRAPHY_NAME),
         period = str_sub(DATE_NAME, start = 10),
         period = as.Date(paste("01", period), format = "%d %b %Y"),
         indicator = "Employment rate - aged 16-64",
         measure = "Percentage",
         unit = "Persons") %>% 
  select(area_code = GEOGRAPHY_CODE,
         area_name,
         indicator, period, measure, unit,
         value = OBS_VALUE)

write_csv(df, "../employment_rate.csv")
