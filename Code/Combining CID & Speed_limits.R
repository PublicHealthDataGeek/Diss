library(osmdata)
library(mapview)
library(tidyverse)
library(sf)
library(ggmap)


# Get boundary box for Greater London
Westxy = getbb("Westminster, UK", format_out = "polygon")


# Plotting with just speed  - but this includes railways at these speeds!
Ten = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "10 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 19 osm lines

Twenty = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "20 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 567 osmlines

Thirty = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "30 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 2101 osmlines

Forty = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "40 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 115 osmlines

Fifty = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "50 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) #23 osm_lines

Sixty = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "60 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 0 osm_lines

Seventy = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "70 mph") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 0 osm_lines

river = opq(bbox = Westxy) %>%
  add_osm_feature(key ="waterway", value = "river") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

ggplot() +
  geom_sf(data = Ten$osm_lines, color = "#4daf4a") +
  geom_sf(data = Twenty$osm_lines, color = "#4daf4a") +
  geom_sf(data = Thirty$osm_lines, color = "#ff7f00") +
  geom_sf(data = Forty$osm_lines, color = "#e41a1c") +
  geom_sf(data = Fifty$osm_lines, color = "#e41a1c") +
  geom_sf(data = river$osm_lines, color ="#377eb8", size = 4)  # I think this includes railways

# get and plot railway data
Rail = opq(bbox = Westxy) %>%
  add_osm_feature(key ="railway", value = "rail") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

ggplot() +
  geom_sf(data = Rail$osm_lines, color = "#4daf4a") +
  geom_sf(data = river$osm_lines, color ="#377eb8", size = 4)

# get and plot highway data
Highway = opq(bbox = Westxy) %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

ggplot() +
  geom_sf(data = Highway$osm_lines, color = "#4daf4a") +
  geom_sf(data = river$osm_lines, color ="#377eb8", size = 4)


# Re obtain speed data but just for highways

FiveH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "5 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) #

TenH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "10 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 19 osm lines - no change

TwentyH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "20 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 503 osmlines ie less

ThirtyH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "30 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 2101 osmlines - NO CHANGE WITH HIGHWAY

FortyH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "40 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) # 19 osmlines

FiftyH = opq(bbox = Westxy) %>%
  add_osm_feature(key = "maxspeed", value = "50 mph") %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) #0 osm_lines ie none now

# below is a comparison to show the difference between the highways one and the railways one
ggplot() +
  geom_sf(data = FortyH$osm_lines, color = "#e41a1c") +
  geom_sf(data = river$osm_lines, color ="#377eb8", size = 4)
ggplot() +
  geom_sf(data = Forty$osm_lines, color = "#e41a1c") +
  geom_sf(data = river$osm_lines, color ="#377eb8", size = 4)



# Now plot the highways only speed limtis
ggplot() +
  geom_sf(data = TenH$osm_lines, color = "#4daf4a") +
  geom_sf(data = TwentyH$osm_lines, color = "#4daf4a") +
  geom_sf(data = ThirtyH$osm_lines, color = "#ff7f00") +
  geom_sf(data = FortyH$osm_lines, color = "#e41a1c") +
  geom_sf(data = river$osm_lines, color ="#a6cee3", size = 4) +
  theme_void()


# get parks data
Parks = opq(bbox = Westxy) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy) 

# create sf object of the bounding box - but it looks crap on map.
Boundary = Westxy %>%
  as.data.frame %>%
  sf::st_as_sf(coords = c(1,2), crs = 4326)

ggplot() +
  geom_sf(data = FiveH$osm_lines, color = "#4daf4a") +
  geom_sf(data = TenH$osm_lines, color = "#33a02c") +
  geom_sf(data = TwentyH$osm_lines, color = "#33a02c") +
  geom_sf(data = ThirtyH$osm_lines, color = "#ff7f00") +
  geom_sf(data = FortyH$osm_lines, color = "#e31a1c") +
  geom_sf(data = hyde_park$osm_polygons, color = "#e1f9c1", fill = "#e1f9c1") +
  geom_sf(data = river$osm_lines, color ="#a6cee3", size = 4) +
  theme_void() +
  geom_sf(data = Boundary)


# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")
West_bound = boroughs %>%
  filter(BOROUGH == "Westminster")


# Best map so far
ggplot() +
  geom_sf(data = West_bound) +
  geom_sf(data = FiveH$osm_lines, color = "#33a02c") +
  geom_sf(data = TenH$osm_lines, color = "#33a02c") +
  geom_sf(data = TwentyH$osm_lines, color = "#33a02c") +
  geom_sf(data = ThirtyH$osm_lines, color = "#ff7f00") +
  geom_sf(data = FortyH$osm_lines, color = "#e31a1c") +
  geom_sf(data = river$osm_lines, color ="#a6cee3", size = 5) +
  theme_void() 

# mapview object
mapview(TwentyH$osm_lines, color = "#33a02c", layer.name = "20 mph or under") +
  mapview(TenH$osm_lines, color = "#33a02c", legend = FALSE) +
  mapview(ThirtyH$osm_lines, color = "#ff7f00", layer.name = "30 mph") +
  mapview(FortyH$osm_lines, color = "#e31a1c", layer.name = "40 mph") +
  mapview(West_bound, col.regions = "beige", legend = FALSE)


# data wrangling to get speed limits into one data frame so can then have a legend
Five = FiveH$osm_lines
Ten = TenH$osm_lines
Twen = TwentyH$osm_lines
Thir = ThirtyH$osm_lines
Four = FortyH$osm_lines
Five = Five %>%
  select(osm_id, maxspeed, geometry)
Five$maxspeed = str_replace_all(Five$maxspeed, "5", "20")
Ten = Ten %>%
  select(osm_id, maxspeed, geometry)
Ten$maxspeed = str_replace_all(Ten$maxspeed, "10", "20")
Twen = Twen %>%
  select(osm_id, maxspeed, geometry)
Thir = Thir %>%
  select(osm_id, maxspeed, geometry)
Four = Four %>%
  select(osm_id, maxspeed, geometry)
Speed_limits = rbind(Five, Ten, Twen, Thir, Four)




mapviewOptions(vector.palette = colorRampPalette(c("#33a02c", "#ff7f00", "#e31a1c")), 
               map.types = ??? )
mapview(Speed_limits, zcol = "maxspeed", layer.name = "Maximum speed limit") +
  mapview(West_bound, col.regions = "beige", legend = FALSE)
