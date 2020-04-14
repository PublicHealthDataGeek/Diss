######################
# Combining datasets #
# Created 14/4/2020  #
######################

# load packages

library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(tmap)
library(mapview)
library(leafsync)
library(summarytools)





accidents = stats19::get_stats19(year = 2018, type = "accidents", ask = FALSE) # n = 122635
casualties = stats19::get_stats19(year = 2018, type = "cas", ask = FALSE) # n = 160597
cyclist_cas = casualties %>% filter(casualty_type == "Cyclist") # n = 17550
cyclist_acc = left_join(cyclist_cas, accidents)


head(cyclist_acc)

# Select London Boroughs
# Create object of all London Boroughs
Lon_LAD = c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", 
            "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", 
            " Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", 
            "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", 
            "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", 
            "Wandsworth", "Westminster")

# Filter on collisions occuring on highways within London Boroughs
Lon_cyc_acc = cyclist_acc %>% 
  filter(local_authority_district %in% Lon_LAD)
dim(Lon_cyc_acc) # 4711 collisons with cyclist casualties in London in 2018


view(freq(Lon_cyc_acc$local_authority_district, report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE, order = "freq")) # Westminster has the highest number = 380

# Select collisions that occur in Westminster
West_cyc_acc = Lon_cyc_acc %>% 
  filter(local_authority_district == "Westminster")

# create sf of this
West_cyc_acc_sf = stats19::format_sf(West_cyc_acc, lonlat = TRUE) # all have coordinates

# get CID data
asl = get_cid_lines(type = "advanced_stop_line")
cross = get_cid_lines(type = "crossing")
lane_track = get_cid_lines(type = "cycle_lane_track")
rr = get_cid_lines(type = "restricted_route")

signal = get_cid_points(type = "signal")
parking = get_cid_points(type = "cycle_parking")
rp = get_cid_points(type = "restricted_point")
signage = get_cid_points(type = "signage")
calming = get_cid_points(type = "traffic_calming")

asl = asl %>%
  filter(BOROUGH == "Westminster") # 229
cross = cross %>%
  filter(BOROUGH == "Westminster") # 47
lane_track = lane_track%>%
  filter(BOROUGH == "Westminster") # 589
rr = rr %>%
  filter(BOROUGH == "Westminster") # 65
signal = signal %>%
  filter(BOROUGH == "Westminster") # 96
parking = parking %>%
  filter(BOROUGH == "Westminster") # 1603
signage = signage %>%
  filter(BOROUGH == "Westminster") # 2976
calming = calming %>%
  filter(BOROUGH == "Westminster") # 716

# Map collisions
mapview(West_cyc_acc_sf$geometry, legend = FALSE, cex = 3)

# create buffer around ASL
buffer_size = 5
asl_buffer = stplanr::geo_projected(asl, st_buffer, dist = buffer_size)
asl_buffer_union = st_union(asl_buffer)

lane_buffer = stplanr::geo_projected(lane_track, st_buffer, dist = buffer_size)
lane_buffer_union = st_union(lane_buffer)

mapview(asl_buffer_union)
mapview(lane_buffer_union)

# Identify collisions within the asl buffer 
asl_coll = West_cyc_acc_sf[asl_buffer_union, ] # n = 4, 2 casualties at the same ASL
x = West_cyc_acc_sf[asl, ]  # dont get any if dont apply buffer

# plot these
mapview(asl_coll)

# Identify collisions within the lane buffer 
lane_coll = West_cyc_acc_sf[lane_buffer_union, ] # n = 45, 

# plot these
mapview(lane_coll)

# Plot cycle lanes and tracks with collisions on them.
mapview(lane_coll, legend = FALSE, cex = 3) + mapview(lane_track, color ='red', legend = FALSE)
# output called Cyc_coll_lanes_West


