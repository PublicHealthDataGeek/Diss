#######################################
# Map TFL infrastructure and Boroughs #
#######################################

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


# TFL Cycle Route Infrastructure ------------------------------------------


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




# Mapping Boroughs --------------------------------------------------------

# Read in London Boroughs
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
mapping_boroughs = rename(boroughs, BOROUGH = NAME)
mapping_boroughs$SHORT = fct_recode(mapping_boroughs$BOROUGH, 
                              "KNS" = "Kensington and Chelsea",
                              "BAR" = "Barking and Dagenham",
                              "HMS" = "Hammersmith and Fulham",
                              "KNG" = "Kingston upon Thames",
                              "RCH" = "Richmond upon Thames",
                              "CTY" = "City of London",
                              "WTH" = "Waltham Forest",
                              "CRD" = "Croydon",
                              "BRM" = "Bromley",
                              "HNS" = "Hounslow",
                              "ELG" = "Ealing",
                              "HVG" = "Havering",
                              "HDN" = "Hillingdon",
                              "HRW" = "Harrow",
                              "BRT" = "Brent",
                              "BRN" = "Barnet",
                              "LAM" = "Lambeth",
                              "SWR" = "Southwark", 
                              "LSH" = "Lewisham",
                              "GRN" = "Greenwich",
                              "BXL" = "Bexley",
                              "ENF" = "Enfield",
                              "RDB" = "Redbridge",
                              "STN" = "Sutton",
                              "MRT" = "Merton",
                              "WNS" = "Wandsworth",
                              "WST" = "Westminster",
                              "CMD" = "Camden",
                              "TOW" = "Tower Hamlets",
                              "ISL" = "Islington",
                              "HCK" = "Hackney",
                              "HGY" = "Haringey",
                              "NWM" = "Newham")

mapped_boroughs1 = tm_shape(mapping_boroughs) +
  tm_fill(col = "ivory2") +
  tm_borders() +
  tm_text("SHORT", size = 0.7) +
  tm_layout(bg.color = "lightblue")

tmap_save(mapped_boroughs1, filename = "./Maps/mapped_boroughs1_map.png")

