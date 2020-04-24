library(osmdata)
library(mapview)
library(tidyverse)
library(sf)
library(ggmap)

# Get CID data
calming = get_cid_points(type = "traffic_calming")
lane_track = get_cid_lines(type = "cycle_lane_track")
asl = get_cid_lines(type = "advanced_stop_line")

lane_track = lane_track%>%
  filter(BOROUGH == "Westminster") # 589

calming = calming %>%
  filter(BOROUGH == "Westminster") # 716

asl = asl %>%
  filter(BOROUGH == "Westminster") # 229

# Get boundary box for Greater London
Westxy = getbb("Westminster, UK", format_out = "polygon")

# Get OSM speed limit data for highways in Westminster ------------------------------
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

# Plot the highways speed limtis
ggplot() +
  geom_sf(data = TenH$osm_lines, color = "#4daf4a") +
  geom_sf(data = TwentyH$osm_lines, color = "#4daf4a") +
  geom_sf(data = ThirtyH$osm_lines, color = "#ff7f00") +
  geom_sf(data = FortyH$osm_lines, color = "#e41a1c") +
  theme_void()

# Get river data
river = opq(bbox = Westxy) %>%
  add_osm_feature(key ="waterway", value = "river") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

# Read in London Boroughs 
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)

# Create Westminster boundary shape
West_bound = boroughs %>%
  filter(BOROUGH == "Westminster")

# Do ggplot of the speed lilmits, rvier and boundaries of Westminster
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


# Data wrangling to get speed limits into one data frame so can then have a legend in mapview
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

# Create mapview of highways in Westminster colour coded by max speed limit
mapviewOptions(vector.palette = colorRampPalette(c("#33a02c", "#ff7f00", "#e31a1c")))
mapview(Speed_limits, zcol = "maxspeed", layer.name = "Maximum speed limit") +
  mapview(West_bound, col.regions = "beige", legend = FALSE)



# Initial data analysis of OSM data ---------------------------------------

# Plotting with just speed  - 
# but after plotting discovered this includes railway lines that have these speeds!
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

ggplot() +
  geom_sf(data = Ten$osm_lines, color = "#4daf4a") +
  geom_sf(data = Twenty$osm_lines, color = "#4daf4a") +
  geom_sf(data = Thirty$osm_lines, color = "#ff7f00") +
  geom_sf(data = Forty$osm_lines, color = "#e41a1c") +
  geom_sf(data = Fifty$osm_lines, color = "#e41a1c")  # I think this includes railways

# get and plot railway data
Rail = opq(bbox = Westxy) %>%
  add_osm_feature(key ="railway", value = "rail") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

ggplot() +
  geom_sf(data = Rail$osm_lines, color = "#4daf4a")


# get and plot highway data
Highway = opq(bbox = Westxy) %>%
  add_osm_feature(key ="highway") %>%
  osmdata_sf() %>%
  trim_osmdata(Westxy)

ggplot() +
  geom_sf(data = Highway$osm_lines, color = "#4daf4a")

