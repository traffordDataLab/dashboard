library(shiny) 
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(ggiraph)
library(scales)
library(sf)
library(leaflet)
library(lubridate)
library(rvest)

# ggplot2 theme
theme_x <- function () { 
  theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "grey50", size = 8, hjust = 1, margin = margin(t = 15)),
      axis.title.x = element_text(size = 9, hjust = 1, margin = margin(t = 10)),
      axis.title.y = element_text(size = 9, angle = 90, hjust = 1, margin = margin(r = 10)),
      axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)),
      legend.position = "none"
    )
}

# Trafford local authority district boundary
boundary <- st_read("data/geospatial/trafford_local_authority.geojson")
gm_boundary <- st_read("data/geospatial/gm_combined_authority.geojson")
