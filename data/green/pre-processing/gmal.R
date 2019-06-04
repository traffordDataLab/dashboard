# Greater Manchester Accessibility Levels (GMAL) #

# Source: Transport for Greater Manchester
# URL: http://odata.tfgm.com/opendata/downloads/GMAL/GMAL_TfGMOpenData.zip
# Licence: Open Government Licence

library(tidyverse) ; library(sf)

url <- "http://odata.tfgm.com/opendata/downloads/GMAL/GMAL_TfGMOpenData.zip"
download.file(url, dest = "GMAL_TfGMOpenData.zip")
unzip("GMAL_TfGMOpenData.zip", exdir = ".")
file.remove("GMAL_TfGMOpenData.zip")

gmal <- st_read("SHP-format/GMAL_grid_open.shp") %>% 
  st_transform(4326)

bdy <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2018_Boundaries_UK_BGC/MapServer/0/query?where=lad18nm%20=%20%27Trafford%27&outFields=lad18cd,lad18nm,long,lat&outSR=4326&f=geojson")

st_intersection(gmal, bdy) %>% 
  select(-lad18cd, -lad18nm, -long, -lat) %>% 
  st_write("gmal.geojson") 