# Licensed Houses in Multiple Occupation #

# Source: Trafford Council
# URL: https://www.trafford.gov.uk/residents/housing/Houses-in-Multiple-Occupation-HMO/HMO-Licensing-Register.aspx
# Licence: Open Government Licence v3.0
# Notes: extracted table from PDF using tabula (https://tabula.technology/)

library(tidyverse) ; library(sf)

# load postcode centroids (ONS Open Geography Portal)
postcodes <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/ONS_Postcode_Directory_Latest_Centroids/MapServer/0/query?where=UPPER(oslaua)=%27", "E08000009", "%27&outFields=pcds,lat,long&outSR=4326&f=geojson")) %>% 
  select(postcode = pcds, lon = long, lat) %>% 
  st_set_geometry(NULL)

df <- read_csv("HMO-Register-2019.csv") %>% 
  unite(address, c("HMO Address", "HMO Address_1"), sep = ", ") %>% 
  select(address, postcode = `HMO Postcode`, households = `No of Households`, expiry = `Expiry Date`) %>% 
  left_join(., postcodes, by = "postcode") %>% 
  filter(!is.na(postcode))
  
write_csv(df, "../licensed_hmos.csv")
