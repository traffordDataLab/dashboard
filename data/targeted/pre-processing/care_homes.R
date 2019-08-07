# Care homes #

# Source: Care Quality Commission
# URL: http://www.cqc.org.uk/about-us/transparency/using-cqc-data
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(httr) ; library(jsonlite)

# retrieve ids of locations in Trafford
request <- GET(url = "https://api.cqc.org.uk/public/v1/locations?", 
    query = list(careHome = "Y", localAuthority = "Trafford"))

response <- content(request, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) 

ids <- as.data.frame(response[['locations']]) %>% pull(locationId)

### retrieve matching location information and ratings
results <- map(ids, ~{
  
  request <- GET(url = paste0("https://api.cqc.org.uk/public/v1/locations/", .x))
  
  response <- content(request, as="text", encoding="UTF-8") %>% 
    fromJSON(flatten = TRUE)  
  
})

# extract elements from list and convert to a dataframe
df <- results %>% {
  tibble(
    id = map_chr(., "locationId"),
    name = map_chr(., "name"),
    status = map_chr(., "registrationStatus"),
    rating = map(., c("currentRatings", "overall", "rating")),
    inspection_date = map(., c("lastInspection", "date")),
    lon = map_dbl(., "onspdLongitude"),
    lat = map_dbl(., "onspdLatitude")
  )
}  %>% 
  filter(status == "Registered") %>% 
  distinct(name, .keep_all = TRUE) %>% 
  select(-status)

# write data
write_csv(df, "../care_homes.csv")
