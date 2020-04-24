#######################################
# Combining datasets: CID and Stats19 #
# Updated ted 24/4/2020               #
#######################################

# load packages

library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(tmap)
library(mapview)
library(leafsync)
library(summarytools)


# Obtain stats19 data for accidents and casualties
accidents = stats19::get_stats19(year = 2018, type = "accidents", ask = FALSE) # n = 122635
casualties = stats19::get_stats19(year = 2018, type = "cas", ask = FALSE) # n = 160597

# Identify cyclist casualties
cyclist_cas = casualties %>% filter(casualty_type == "Cyclist") # n = 17550

# Identify collisions where there are cyclist casualties
cyclist_acc = left_join(cyclist_cas, accidents)

# Create object of all London Boroughs
Lon_LAD = c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", 
            "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", 
            "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", 
            "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", 
            "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", 
            "Wandsworth", "Westminster")

# Obtain collisions occuring on highways within London Boroughs that result in cyclist casualties
Lon_cyc_acc = cyclist_acc %>% 
  filter(local_authority_district %in% Lon_LAD)
dim(Lon_cyc_acc) # 4751 collisons with cyclist casualties in London in 2018

# View frequency by Borough
view(freq(Lon_cyc_acc$local_authority_district, report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE, order = "freq")) # Westminster has the highest number = 380

# Select collisions that occur in Westminster
West_cyc_acc = Lon_cyc_acc %>% 
  filter(local_authority_district == "Westminster")

# create sf of this
West_cyc_acc_sf = stats19::format_sf(West_cyc_acc, lonlat = TRUE) # all have coordinates

# Map collisions with borough boundary
mapview(West_cyc_acc_sf$geometry, legend = FALSE, color = 'red', cex = 2) + 
  mapview(West_bound, col.regions = "beige", alpha.regions = 0, legend = FALSE)

# Read in London Boroughs 
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")
# Create Westminster boundary shape
West_bound = boroughs %>%
  filter(BOROUGH == "Westminster")

# get CID cycle lane and track data
lane_track = get_cid_lines(type = "cycle_lane_track")
lane_track = lane_track%>%
  filter(BOROUGH == "Westminster") # limit to Westminster Borough = 589

# create buffer around cycle lanes/tracks
buffer_size = 5
lane_buffer = stplanr::geo_projected(lane_track, st_buffer, dist = buffer_size)
lane_buffer_union = st_union(lane_buffer)

# map the cycle lanes/tracks buffers
mapview(lane_buffer_union)

x = mapview(lane_buffer_union, legend = FALSE) 
y = mapview(lane_buffer_union, legend = FALSE)
sync(x, y, sync = "none")  # this enables the same map to be compared at differnt zooms

# Identify collisions within the lane buffer 
lane_coll = West_cyc_acc_sf[lane_buffer_union, ] # n = 45, 

# plot these
mapview(lane_coll)

# Plot cycle lanes and tracks with collisions on them.
mapview(lane_coll, legend = FALSE, cex = 2, color = 'red') + 
  mapview(lane_track, color = 'blue', legend = FALSE) +
  mapview(West_bound, col.regions = "beige", alpha.regions = 0, legend = FALSE)




