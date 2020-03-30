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
                "CLT_PARKR", "CLT_WATERR", "CLT_PTIME", "CLT_COLOUR", "BOROUGH")

# convert columns to factors
cycle_lane_track %<>%
  mutate_at(f_variables, as.factor)

# glimpse post-conversion
glimpse(cycle_lane_track)

# create new df without geommetry that enables analysis of data
non_geom_cycle_lane_track = st_drop_geometry(cycle_lane_track)

non_geom_cycle_lane_track %>%
  count(CLT_CARR)  

# create summary of df
view(dfSummary(non_geom_cycle_lane_track))


str(cycle_lane_track)
levels(cycle_lane_track$BOROUGH)

