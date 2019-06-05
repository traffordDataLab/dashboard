# (Alternative) Claimant count #

# Source: ONS and DWP
# URL: https://www.gov.uk/government/publications/alternative-claimant-count-statistics-great-britain-january-2013-to-november-2018/alternative-claimant-count-statistics-great-britain-january-2013-to-november-2018
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(jsonlite) ; library(janitor)

# Claimant count (NOMIS, ONS)
# https://www.nomisweb.co.uk/datasets/ucjsa
claimant_count <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=E11000001,E08000009&date=latestMINUS75-latest&gender=0&age=0&measure=1&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measure_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(area_name = case_when(GEOGRAPHY_NAME == "Greater Manchester (Met County)" ~ "Greater Manchester", TRUE ~ GEOGRAPHY_NAME),
         indicator = "Claimant count",
         period = as.Date(paste('01', DATE_NAME), format='%d %B %Y'),
         measure = "Count",
         unit = "Persons",
         group = "Claimant count") %>% 
  select(area_code = GEOGRAPHY_CODE,
         area_name, indicator, period, 
         measure, unit,
         value = OBS_VALUE,
         group)

# Alternative claimant count (Stat-Xplore, DWP)
# https://stat-xplore.dwp.gov.uk/webapi/opendatabase?id=ACC
api_key <- "65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d686c626e4a354c6e4268636e52796157526e5a554230636d466d5a6d39795a43356e62335975645773694c434a70595851694f6a45314e4455774e4445344d7a6373496d46315a434936496e4e30636935765a47456966512e5a547758556872566e57787164554969457357383977356c6b33656e764d66686b5537367264344667416b"
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"
query <- list(database = unbox("str:database:ACC"),
              measures = "str:count:ACC:V_F_ACC",
              dimensions = c("str:field:ACC:V_F_ACC:OA11",
                             "str:field:ACC:F_ACC_DATE_new:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:ACC:F_ACC_DATE_new:DATE_NAME` = list(
                  map = as.list(paste0("str:value:ACC:F_ACC_DATE_new:DATE_NAME:C_ACC_DATE:", 
                                       format(as.yearmon(seq(ymd('2013-01-01'), ymd('2019-02-1'), by = '1 month')), "%Y%m")))),
                `str:field:ACC:V_F_ACC:OA11` = list(
                  map = as.list(paste0("str:value:ACC:V_F_ACC:OA11:V_C_MASTERGEOG11_LA_TO_REGION:E0", seq(8000001, 8000010, 1)))))) %>% 
  toJSON()

request <- POST(url = path, body = query, config = add_headers(APIKey = api_key), encode = "json")
response <- fromJSON(content(request, as = "text"), flatten = TRUE)
dimnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- dimnames

alternative_claimant_count <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  set_names(c(response$fields$label,"value")) %>% 
  spread(`National - Regional - LA - OAs`, value) %>% 
  mutate(`Greater Manchester` = rowSums(.[2:11])) %>% 
  select(Month, Trafford, `Greater Manchester`) %>% 
  gather(area_name, value, -`Month`) %>% 
  mutate(area_code = case_when(area_name == "Greater Manchester" ~ "E11000001", 
                               area_name == "Trafford" ~ "E08000009"),
         indicator = "Claimant count",
         period = as.Date(paste('01', Month), format='%d %B %Y'),
         measure = "Count",
         unit = "Persons",
         group = "Alternative claimant count") %>% 
  select(area_code, area_name, indicator, period, measure, unit, value, group)

bind_rows(claimant_count, alternative_claimant_count) %>% 
  write_csv("../claimant_count.csv")
  

                      
                      
                      