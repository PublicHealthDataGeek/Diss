############################
# TFL CID
# Created 30/03/2020
######################

# install packages
library(tidyverse)
library(CycleInfraLnd)
library(sf)
library(tmap)
library(mapview)
library(leafsync)
library(summarytools)
library(forcats)

# download CID data using the Cycle Infra Lnd package
# lines
cycle_lane_track = get_cid_lines(type = "cycle_lane_track") # n = 24976
crossings = get_cid_lines(type = "crossing")
advanced_stop_line = get_cid_lines(type = "advanced_stop_line")
restricted_route = get_cid_lines(type = "restricted_route")

# points
signal = get_cid_points(type = "signal")
cycle_parking = get_cid_points(type = "cycle_parking")
restricted_point = get_cid_points(type = "restricted_point")
signage = get_cid_points(type = "signage")
traffic_calming = get_cid_points(type = "traffic_calming")

# A) cycle_lane_track
class(cycle_lane_track)
str(cycle_lane_track)

# convert certain columns to factors
levels(cycle_lane_track$CLT_CARR) # => NULL

f_variables = c("CLT_CARR", "CLT_SEGREG", "CLT_STEPP", "CLT_PARSEG", "CLT_SHARED", "CLT_MANDAT", 
                "CLT_ADVIS", "CLT_PRIORI", "CLT_CONTRA", "CLT_BIDIRE", "CLT_CBYPAS", "CLT_BBYPAS",
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_cycle_lane_track = cycle_lane_track %<>%
  mutate_at(f_variables, as.factor)
f_cycle_lane_track$BOROUGH = factor(cycle_lane_track$BOROUGH, exclude = NULL)

glimpse(f_cycle_lane_track) # check converted ok
levels(f_cycle_lane_track$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# create new df without geommetry that enables faster analysis of data
non_geom_f_cycle_lane_track = st_drop_geometry(f_cycle_lane_track)
str(non_geom_f_cycle_lane_track)
non_geom_f_cycle_lane_track %>%
  count(CLT_CARR)  

# create summary of df
view(dfSummary(non_geom_f_cycle_lane_track))

# add column for length of each line
f_cycle_lane_track$length = st_length(f_cycle_lane_track$geometry)



# Read in London Boroughs to add to map
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_cycle_lanesBYborough = non_geom_f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
count_cycle_lanesBYborough = count_cycle_lanesBYborough[-c(34),]

# join numbers to geometry
n_cycle_lanesBYborough = left_join(boroughs, count_cycle_lanesBYborough)

# plot counts
qtm(n_cycle_lanesBYborough, "count") # works!!!

# b) # map borough level data on infrastructure length
length_cycle_lanesBYborough = non_geom_f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 

# delete 'NA' row
length_cycle_lanesBYborough = length_cycle_lanesBYborough[-c(34),]

# join numbers to geometry
l_cycle_lanesBYborough = left_join(boroughs, length_cycle_lanesBYborough)

# plot sum of lengths by brough
qtm(l_cycle_lanesBYborough, "length") # Need to alter legend 

legend_length = expression("Total length (m)")
tm_shape(l_cycle_lanesBYborough) +
  tm_polygons("length", title = "Total length (m)", style = "pretty", palette = "Blues") +
  tm_layout("Total length of cycle track and lanes by borough",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

#c) map borough level data for proportion of count
prop_cycle_lanesBYborough = non_geom_f_cycle_lane_track %>%
  group_by(BOROUGH) %>%
  summarise(per = paste0(round(count/sum(count)*100, 2), "%"))

  mutate(countT = sum(count)) %>%
  group_by(type, add = TRUE) %>%
  mutate(per = paste0(round(100*count/countT, 2), "%"))
  
# delete 'NA' row
count_cycle_lanesBYborough = count_cycle_lanesBYborough[-c(34),]

# join numbers to geometry
n_cycle_lanesBYborough = left_join(boroughs, count_cycle_lanesBYborough)

# plot counts
qtm(n_cycle_lanesBYborough, "count") # works!!!


# Read in London Boroughs to add to map
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
#Simplify boroughs
borough_areas <- rmapshaper::ms_simplify(boroughs, keep=0.01)
# read in rivers
rivers <- st_read("./map_data/rivers_outer.json") %>% 
  st_transform(crs=27700) 




ggplot() +
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#ffffff", alpha=0.3, size=0.5) +
  geom_sf(data=rivers, fill="#E2EDF6",  colour="#E2EDF6", size=2.5)+
  coord_sf(crs=st_crs(rivers), datum=NA)+
  scale_size_continuous(range=c(0.1,0.9))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  theme_bw() +
  theme(axis.title = element_blank())


# Example of plot from internship work
ggplot()+
  geom_sf(data=borough_areas, fill="#d4d4d4",  colour="#ffffff", alpha=0.3, size=0.5)+
  geom_sf(data=motorways, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=rivers, fill="#E2EDF6",  colour="#E2EDF6", size=2.5)+
  geom_sf(data=TfLroads, colour="#d4c9c9", size=0.1)+
  geom_point(data=plot_data, aes(x=d_east, y=d_north, size=trip_count), colour="#b80413", fill="#b80413", alpha=0.3)+
  geom_sf(data=CS6NS, colour="#030000", size=0.2)  +
  coord_sf(crs=st_crs(rivers), datum=NA)+
  scale_size_continuous(range=c(0.1,1.5))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  scale_colour_distiller(palette="Reds", direction=1)+
  theme_bw() +
  theme(axis.title = element_blank()) +
  ggsave("./figures/PS_dest_CS6NS.png")