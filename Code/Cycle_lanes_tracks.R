####################################
# TFL CID - cycle lanes and tracks #
# Created 30/03/2020               #
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
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976

class(cycle_lane_track)
str(cycle_lane_track)

# check completeness of variables
unique(cycle_lane_track$FEATURE_ID) # 24976 unique variables
unique(cycle_lane_track$BOROUGH) # 33 Boroughs plus a NA group
unique(cycle_lane_track$SVDATE) # 345 unique survey dates, all of which are valid date
date$SVDATE = cycle_lane_track$SVDATE

# the below all have just true and false except where stated
unique(cycle_lane_track$CLT_CARR)
unique(cycle_lane_track$CLT_SEGREG)
unique(cycle_lane_track$CLT_STEPP)
unique(cycle_lane_track$CLT_PARSEG)
unique(cycle_lane_track$CLT_SHARED) # "FALSE" "TRUE"  "TCB" 
unique(cycle_lane_track$CLT_MANDAT) # "FALSE" "TRUE"  "TCB" 
unique(cycle_lane_track$CLT_ADVIS)
unique(cycle_lane_track$CLT_PRIORI) # FALSE" "TRUE"  "TRE"
unique(cycle_lane_track$CLT_CONTRA)
unique(cycle_lane_track$CLT_BIDIRE)
unique(cycle_lane_track$CLT_CBYPAS)
unique(cycle_lane_track$CLT_BBYPAS)
unique(cycle_lane_track$CLT_PARKR)
unique(cycle_lane_track$CLT_WATERR)
unique(cycle_lane_track$CLT_PTIME)
unique(cycle_lane_track$CLT_ACCESS) # NA plus 724 other unique text responses 
unique(cycle_lane_track$CLT_COLOUR) # "NONE"        "GREEN"       "RED"         "BUFF/YELLOW" "BLUE"        "OTHER"       "BUFF" 

# convert certain columns to factors
levels(cycle_lane_track$CLT_CARR) # => NULL

f_variables = c("CLT_CARR", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", 
                "CLT_ADVIS", "CLT_PRIORI", "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS",
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_cycle_lane_track = cycle_lane_track %>%
  mutate_at(f_variables, as.factor)
f_cycle_lane_track$BOROUGH = factor(cycle_lane_track$BOROUGH, exclude = NULL)

glimpse(f_cycle_lane_track) # check converted ok
levels(f_cycle_lane_track$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# create new df without geommetry that enables faster analysis of data
non_geom_f_cycle_lane_track = st_drop_geometry(f_cycle_lane_track)
str(non_geom_f_cycle_lane_track)
count_borough = non_geom_f_cycle_lane_track %>%
  count(BOROUGH)  

# create summary of df
view(dfSummary(non_geom_f_cycle_lane_track))

# examine URL data
count_photo1 =  non_geom_f_cycle_lane_track %>%
  count(PHOTO1_URL) # 588 have no asset photo 1
count_photo2 =  non_geom_f_cycle_lane_track %>%
  count(PHOTO2_URL) # 605 have no asset photo 2

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_cycle_lanesBYborough = non_geom_f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(Count = n())

# delete 'NA' row
count_cycle_lanesBYborough = count_cycle_lanesBYborough[-c(34),]

# count number of lanes by borough
summary(count_cycle_lanesBYborough$Count)
sd(count_cycle_lanesBYborough$Count)


# join numbers to geometry
n_cycle_lanesBYborough = left_join(boroughs, count_cycle_lanesBYborough)

# plot counts
qtm(n_cycle_lanesBYborough, "Count") # works!!!

count_cycle_lane_map = tm_shape(n_cycle_lanesBYborough) +
  tm_polygons("Count", style = "fixed", palette = "Greens",
              breaks = c(1, 200, 400, 600, 800, 1000, 1200, 1400)) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)  

tmap_save(count_cycle_lane_map, filename = "./Maps/Cycle lanes/Count_cycle_lane_map.png")





# b) Map borough level data on infrastructure length
# add column for length of each line
f_cycle_lane_track$length = st_length(f_cycle_lane_track$geometry)

# get some summary data
summary(f_cycle_lane_track$length)
sd(f_cycle_lane_track$length)
sum(f_cycle_lane_track$length) # = 2906338

# create new df without geommetry that enables faster analysis of data
non_geom_length_cycle_lane_track = st_drop_geometry(f_cycle_lane_track)



length_cycle_lanesBYborough = non_geom_length_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 

# delete 'NA' row
length_cycle_lanesBYborough = length_cycle_lanesBYborough[-c(34),]

# join numbers to geometry
l_cycle_lanesBYborough = left_join(boroughs, length_cycle_lanesBYborough)

# plot sum of lengths by brough
qtm(l_cycle_lanesBYborough, "length") # Need to alter legend 


Length_lanes_borough_map = tm_shape(l_cycle_lanesBYborough) +
  tm_polygons("length", title = "Total length (m)", style = "pretty", palette = "Blues") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Length_lanes_borough_map, filename = "./Maps/Length_lanes_borough_map.png")

#c) map borough level data for proportion of count
prop_cycle_lanesBYborough = non_geom_f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 0), '%'))
  
# delete 'NA' row
prop_cycle_lanesBYborough = prop_cycle_lanesBYborough[-c(34),]

# join numbers to geometry
percent_cycle_lanesBYborough = left_join(boroughs, prop_cycle_lanesBYborough)

# plot % of cycle lanes/tracks by brough
qtm(percent_cycle_lanesBYborough, "Percentage") # Need to alter legend 

Percent_lanes_borough_map = tm_shape(percent_cycle_lanesBYborough) +
  tm_polygons("Percentage", style = "pretty", palette = "Oranges") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Percent_lanes_borough_map, filename = "./Maps/Percent_lanes_borough_map.png")


# Overall map of cycling infrastructure
map_cycle_lanes = mapview(f_cycle_lane_track$geometry, colour = "blue")
mapshot(map_cycle_lanes, file = paste0(getwd(),"/Maps/","All_cycle_lanes_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping segregated cycling infrastructure
# a) get CID segregated infrastructure
seg_cycle_lane = f_cycle_lane_track %>%
  filter(CLT_SEGREG ==TRUE)

# ai) do some length calculations
total_length_seg = sum(seg_cycle_lane$length) # = 93704m

partial_seg = f_cycle_lane_track %>%
  filter(CLT_PARSEG == TRUE)
sum(partial_seg$length) # = 251143.8 [m]

stepped = f_cycle_lane_track %>%
  filter(CLT_STEPP == TRUE)
sum(stepped$length) # = 7736.878 [m]

shared = f_cycle_lane_track %>%
  filter(CLT_SHARED == TRUE)
sum(shared$length) # = 1898026 [m]


seg_cycle_lanes = mapview(seg_cycle_lane$geometry, color = "red") # map it

# b) Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it


# Create synchronised map of CID seg lanes v open TFL cycle routes 
sync(seg_cycle_lanes, map_open_TFL_CR, no.initial.sync = FALSE) 
# (have to take png via Rstudio as no code available for synced image saving)


# Mapping on/off carriageway cycling infrastructure
# a) get CID on/pff carriageways infrastructure
on_cycle_lane = f_cycle_lane_track %>%
  filter(CLT_CARR == TRUE)

off_cycle_lane = f_cycle_lane_track %>%
  filter(CLT_CARR == FALSE)

total_length_on = sum(on_cycle_lane$length) # = 944165.6m
total_length_off = sum(off_cycle_lane$length) # = 1962172m
total_length_all = sum(f_cycle_lane_track$length) # = 2906338

off_cycle_lanes = mapview(off_cycle_lane$geometry, color = "green") # map it
on_cycle_lanes = mapview(on_cycle_lane$geometry, color = "red") # map it
sync(on_cycle_lanes, off_cycle_lanes, no.initial.sync = FALSE) # maps of on and off carriageway


x = mapview(on_cycle_lane$geometry, color = "red") + mapview(open_TFL_CR$geometry, alpha = 0.8)
y = mapview(off_cycle_lane$geometry, color = "green") + mapview(open_TFL_CR$geometry, alpha = 0.8)
sync(x, y, no.initial.sync = FALSE)   # maps of on and off carriageway with TFL CR                           


# system time to check speed of actions on geog/nongeog datasets
system.time(non_geom_f_cycle_lane_track %>%
              count(BOROUGH)) # elapsed = 0.017
system.time(f_cycle_lane_track %>%
              count(BOROUGH)) # 10.446 


