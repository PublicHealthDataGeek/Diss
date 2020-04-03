####################################
# TFL CID - traffic calming        #
# Created 03/04/2020               #
####################################

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

# download CID data using the Cycle Infra Lnd package

traffic_calming = get_cid_points(type = "traffic_calming")
class(traffic_calming)
str(traffic_calming)

# convert certain columns to factors
f_variables = c("TRF_RAISED", "TRF_ENTRY", "TRF_CUSHI", "TRF_HUMP", "TRF_SINUSO",
                "TRF_BARIER", "TRF_NAROW", "TRF_CALM")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_traffic_calming = traffic_calming %>%
  mutate_at(f_variables, as.factor)
f_traffic_calming$BOROUGH = factor(traffic_calming$BOROUGH, exclude = NULL)

glimpse(f_traffic_calming) # check converted ok
levels(f_traffic_calming$BOROUGH) # have 33 and no NA value

# create new df without geommetry that enables faster analysis of data
non_geom_f_traffic_calming = st_drop_geometry(f_traffic_calming)
str(non_geom_f_traffic_calming)


# create summary of df
view(dfSummary(non_geom_f_traffic_calming))

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_traffic_calmingBYborough = non_geom_f_traffic_calming %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
# count_signalBYborough = count_signalBYborough[-c(34),]  NOT REQUIRED AS NO NA ROW

# join numbers to geometry
n_traffic_calmingBYborough = left_join(boroughs, count_traffic_calmingBYborough)

# plot counts of signals by Borough
qtm(n_traffic_calmingBYborough, "count")
  
Count_traffic_calming_borough_map = tm_shape(n_traffic_calmingBYborough) +
  tm_polygons("count", style = "fixed", palette = "Greens", 
              breaks = c(1, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("right","bottom"),
            legend.bg.alpha = 1)
  
tmap_save(Count_traffic_calming_borough_map, filename = "./Maps/Count_traffic_calming_borough_map.png")

# Overall map of signals
map_signals

map_traffic_calming = mapview(f_traffic_calming$geometry, color = "blue", cex = 0.5)
mapshot(map_traffic_calming, file = paste0(getwd(),"/Maps/","All_traffic_calming_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping segregated cycling infrastructure
# Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it

mapTFL = open_TFL_CR %>%
  select(geometry, Programme)
CSH = mapTFL %>%
  filter(Programme == "Cycle Superhighways")

# create subsets of traffic calming
raised_junction_locations =  f_traffic_calming %>%
  select(TRF_RAISED, geometry) %>%
  filter(TRF_RAISED == TRUE)

raised_side_road_locations =  f_traffic_calming %>%
  select(TRF_ENTRY, geometry) %>%
  filter(TRF_ENTRY == TRUE)

# Create overlapping map of various traffic calming measures and TFL cycle routes
# a) 
mapview(raised_junction_locations, color = "lightseagreen", cex = 1.5, legend = FALSE) +
  mapview(open_TFL_CR$geometry, lwd = 2)

# b)
mapview(raised_side_road_locations, color = "red", cex = 0.5, legend = FALSE ) +
  mapview(open_TFL_CR$geometry, lwd = 3) + 
  mapview(raised_junction_locations, color = "lightseagreen", cex = 0.5, legend = FALSE)


cushion_locations =  f_traffic_calming %>%
  select(TRF_CUSHI, geometry) %>%
  filter(TRF_CUSHI == TRUE)

hump_locations =  f_traffic_calming %>%
  select(TRF_HUMP, geometry) %>%
  filter(TRF_HUMP == TRUE)

mapview(cushion_locations, color = "red", cex = 0.5, legend = FALSE ) +
  mapview(hump_locations, color = "lightseagreen", cex = 0.5, legend = FALSE)
