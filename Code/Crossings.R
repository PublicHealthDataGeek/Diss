####################################
# TFL CID - crossings              #
# Created 31/03/2020               #
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
# lines
crossings = get_cid_lines(type = "crossing")


class(crossings) # => "sf"         "tbl_df"     "tbl"        "data.frame"
str(crossings) # 1687 obs, 11 variables

# convert certain columns to factors
levels(crossings$CRS_SIGNAL) # => NULL

f_variables = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_CYGAP", "CRS_PEDEST", "CRS_LEVEL")


# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_crossings = crossings %>%
  mutate_at(f_variables, as.factor)
f_crossings$BOROUGH = factor(f_crossings$BOROUGH, exclude = NULL)

glimpse(f_crossings) # check converted ok
levels(f_crossings$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# create new df without geommetry that enables faster analysis of data
non_geom_f_crossings = st_drop_geometry(f_crossings)
str(non_geom_f_crossings)
non_geom_f_crossings %>%
  count(CRS_SIGNAL)  

# create summary of df
view(dfSummary(non_geom_f_crossings))

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_crossingsBYborough = non_geom_f_crossings %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
count_crossingsBYborough = count_crossingsBYborough[-c(34),]

# join numbers to geometry
n_crossingsBYborough = left_join(boroughs, count_crossingsBYborough)

# plot counts
qtm(n_crossingsBYborough, "count") # works!!!

# b) Map borough level data on infrastructure length
# add column for length of each line
f_crossings$length = st_length(f_crossings$geometry)

# create new df without geommetry that enables faster analysis of data
non_geom_length_f_crossings = st_drop_geometry(f_crossings)

length_crossingsBYborough = non_geom_length_f_crossings %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 

# delete 'NA' row
length_crossingsBYborough = length_crossingsBYborough[-c(34),]

# join numbers to geometry
l_crossingsBYborough = left_join(boroughs,length_crossingsBYborough)

# plot sum of lengths by brough
qtm(l_crossingsBYborough, "length") # Need to alter legend 


Length_crossings_borough_map = tm_shape(l_crossingsBYborough) +
  tm_polygons("length", title = "Total length (m)", style = "pretty", palette = "Blues") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Length_crossings_borough_map, filename = "./Maps/Length_crossings_borough_map.png")

#c) map borough level data for proportion of count
prop_crossingsBYborough = non_geom_f_crossings %>%
  group_by(BOROUGH) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 0), '%'))

# delete 'NA' row
prop_crossingsBYborough = prop_crossingsBYborough[-c(34),]

# join numbers to geometry
percent_crossingsBYborough = left_join(boroughs, prop_crossingsBYborough)

# plot % of cycle lanes/tracks by brough
qtm(percent_crossingsBYborough, "Percentage") # Need to alter legend 

Percent_crossings_borough_map = tm_shape(percent_crossingsBYborough) +
  tm_polygons("Percentage", style = "pretty", palette = "Oranges") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Percent_crossings_borough_map, filename = "./Maps/Percent_crossings_borough_map.png")


# Overall map of cycling infrastructure
map_crossings = mapview(f_crossings$geometry, colour = "blue")
mapshot(map_crossings, file = paste0(getwd(),"/Maps/","All_crossings_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

