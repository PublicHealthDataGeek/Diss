####################################
# TFL CID - restricted routes      #
# Created 01/043/2020               #
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
library(magrittr)

# download CID data using the Cycle Infra Lnd packages
restricted_route = get_cid_lines(type = "restricted_route")

class(restricted_route)
str(restricted_route)

# convert certain columns to factors
levels(restricted_route$RES_PEDEST) # => NULL

f_variables = c("RES_PEDEST", "RES_BRIDGE", "RES_TUNNEL", "RES_STEPS", "RES_LIFT")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_restricted_route = restricted_route %>%
  mutate_at(f_variables, as.factor)
f_restricted_route$BOROUGH = factor(restricted_route$BOROUGH, exclude = NULL)

glimpse(f_restricted_route) # check converted ok
levels(f_restricted_route$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# create new df without geommetry that enables faster analysis of data
non_geom_f_restricted_route = st_drop_geometry(f_restricted_route)
str(non_geom_f_restricted_route)
non_geom_f_restricted_route %>%
  count(RES_PEDEST)  

# create summary of df
view(dfSummary(non_geom_f_restricted_route))

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_restricted_routeBYborough = non_geom_f_restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
count_restricted_routeBYborough = count_restricted_routeBYborough[-c(34),]

# join numbers to geometry
n_restricted_routeBYborough = left_join(boroughs, count_restricted_routeBYborough)

# plot counts
qtm(n_restricted_routeBYborough, "count") # works!!!

# b) Map borough level data on infrastructure length
# add column for length of each line
f_restricted_route$length = st_length(f_restricted_route$geometry)

# create new df without geommetry that enables faster analysis of data
non_geom_length_restricted_route = st_drop_geometry(f_restricted_route)

length_restricted_routeBYborough = non_geom_length_restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 

# delete 'NA' row
length_restricted_routeBYborough = length_restricted_routeBYborough[-c(34),]

# join numbers to geometry
l_restricted_routeBYborough = left_join(boroughs, length_restricted_routeBYborough)

# plot sum of lengths by brough
qtm(l_restricted_routeBYborough, "length") # Need to alter legend 


Length_restricted_route_borough_map = tm_shape(l_restricted_routeBYborough) +
  tm_polygons("length", title = "Total length (m)", style = "pretty", palette = "Blues") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Length_restricted_route_borough_map, filename = "./Maps/Length_restricted_routes_borough_map.png")

#c) map borough level data for proportion of count
prop_restricted_routesBYborough = non_geom_f_restricted_route %>%
  group_by(BOROUGH) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 0)))

prop_restricted_routesBYborough$Percentage %<>% as.integer 
# above required to convert to integer as otherwise 10 and 11 appear after 1 so colour order is incorrect

# delete 'NA' row
prop_restricted_routesBYborough = prop_restricted_routesBYborough[-c(34),]

# join numbers to geometry
percent_restricted_routeBYborough = left_join(boroughs, prop_restricted_routesBYborough)

# plot % of cycle lanes/tracks by brough
qtm(percent_restricted_routeBYborough, "Percentage") # Need to alter legend 

Percent_restricted_route_borough_map = tm_shape(percent_restricted_routeBYborough) +
  tm_polygons("Percentage", style = "pretty", palette = "Oranges") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Percent_restricted_route_borough_map, filename = "./Maps/Percent_restricted_route_borough_map.png")


# Overall map of restricted routes
map_restricted_routes = mapview(f_restricted_route$geometry, color = "red")

# restricted routes on OSM so can show how they are in the parks.  
mapview(f_restricted_route$geometry, color = "red", map.types = "OpenStreetMap.DE")

# side by side comparison of restricted and cycle routes
x = mapview(f_restricted_route$geometry, color = "red", alpha = 0.7)
y = mapview(f_cycle_lane_track$geometry, color = "green", alpha = 0.7) 
sync(x, y, no.initial.sync = FALSE)   # maps of on and off carriageway with TFL CR     

# overlaid restricted and cycle routes
mapview(f_restricted_route$geometry, color = "red") + mapview(f_cycle_lane_track$geometry, color = "green")




