##########################
# Map TFL infrastructure #
##########################

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(tmap)
library(mapview)
library(leafsync)
library(summarytools)
library(forcats)
library(geojsonsf)


# Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it

mapview(open_TFL_CR, zcol = "Label") # colours represent each cycle route (but there are many!) eg Q1
mapview(open_TFL_CR, zcol = "Programme") # colours represent each programme but using viridis

# Below code means I can specify colours of the different infrastructure
mapTFL = open_TFL_CR %>%
  select(geometry, Programme)
mapviewOptions(vector.palette = colorRampPalette(c("blue", 
                                                   "#007FFF", "cyan",
                                                   "#7FFF7F", "yellow", 
                                                   "#FF7F00", "red", "#7F0000")))
mapview(mapTFL)
