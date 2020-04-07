####################################
# TFL CID - advanced stop lines    #
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
advanced_stop_line = get_cid_lines(type = "advanced_stop_line")


class(advanced_stop_line) # "sf"         "tbl_df"     "tbl"        "data.frame"
str(advanced_stop_line)

# check completeness of variables
unique(advanced_stop_line$FEATURE_ID) # 3775 unique variables
unique(advanced_stop_line$BOROUGH) # 33 Boroughs plus a NA group
unique(advanced_stop_line$SVDATE) # 290 unique survey dates, all of which are valid date
# the below all have just true and false apart fro colour that has 6 options
unique(advanced_stop_line$ASL_FDR)
unique(advanced_stop_line$ASL_FDRLFT)
unique(advanced_stop_line$ASL_FDCENT)
unique(advanced_stop_line$ASL_FDRIGH)
unique(advanced_stop_line$ASL_SHARED)
unique(advanced_stop_line$ASL_COLOUR)


# convert certain columns to factors
levels(advanced_stop_line$ASL_FDR) # => NULL

f_variables = c("ASL_FDR", "ASL_FDRLFT", "ASL_FDCENT", "ASL_FDRIGH", 
                "ASL_SHARED", "ASL_COLOUR")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_advanced_stop_line = advanced_stop_line %>%
  mutate_at(f_variables, as.factor)
f_advanced_stop_line$BOROUGH = factor(advanced_stop_line$BOROUGH, exclude = NULL)

glimpse(f_advanced_stop_line) # check converted ok
levels(f_advanced_stop_line$BOROUGH) # check have 34 (33 actual boroughs plus 1 NA value)

# create new df without geommetry that enables faster analysis of data
non_geom_f_advanced_stop_line = st_drop_geometry(f_advanced_stop_line)
str(non_geom_f_advanced_stop_line)
count_borough = non_geom_f_advanced_stop_line %>%
  count(BOROUGH)  


# system time to check speed of actions on geog/nongeog datasets
system.time(non_geom_f_advanced_stop_line %>%
             count(BOROUGH)) # elapsed = 0.003
system.time(f_advanced_stop_line %>%
             count(BOROUGH)) # 0.559


# create summary of df
view(dfSummary(non_geom_f_advanced_stop_line))

# examine URL data
count_photo1 =  non_geom_f_advanced_stop_line %>%
  count(PHOTO1_URL) # 48 have no asset photo 1
count_photo2 =  non_geom_f_advanced_stop_line %>%
  count(PHOTO2_URL) # 51 have no asset photo 2

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_ASLBYborough = non_geom_f_advanced_stop_line %>%
  group_by(BOROUGH) %>%
  summarise(count = n())
summary(count_ASLBYborough$count)
sd(count_ASLBYborough$count)

# delete 'NA' row
count_ASLBYborough = count_ASLBYborough[-c(34),]

# join numbers to geometry
n_ASLBYborough = left_join(boroughs, count_ASLBYborough)

# plot counts
qtm(n_ASLBYborough, "count") # works!!!

# b) Map borough level data on infrastructure length
# add column for length of each line
f_advanced_stop_line$length = st_length(f_advanced_stop_line$geometry)
summary(f_advanced_stop_line$length)
sd(f_advanced_stop_line$length)
boxplot(f_advanced_stop_line$length)
hist(f_advanced_stop_line$length)


# create new df without geommetry that enables faster analysis of data
non_geom_length_advanced_stop_line = st_drop_geometry(f_advanced_stop_line)

length_ASLBYborough = non_geom_length_advanced_stop_line %>%
  group_by(BOROUGH) %>%
  summarise(length = sum(length)) 

# delete 'NA' row
length_ASLBYborough = length_ASLBYborough[-c(34),]

# join numbers to geometry
l_ASLBYborough = left_join(boroughs, length_ASLBYborough)

# plot sum of lengths by borough
qtm(l_ASLBYborough, "length")

Length_ASL_borough_map = tm_shape(l_ASLBYborough) +
  tm_polygons("length", title = "Total length (m)", style = "pretty", palette = "Blues") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Length_ASL_borough_map, filename = "./Maps/Length_ASL_borough_map.png")

#c) map borough level data for proportion of count
prop_ASLBYborough = non_geom_f_advanced_stop_line %>%
  group_by(BOROUGH) %>%
  summarise(n = n()) %>%
  mutate(Percentage = paste0(round(100 * n/sum(n), 0), '%'))
  
# delete 'NA' row
prop_ASLBYborough = prop_ASLBYborough[-c(34),]

# join numbers to geometry
percent_ASLBYborough = left_join(boroughs, prop_ASLBYborough)

# plot % of cycle lanes/tracks by brough
qtm(percent_ASLBYborough, "Percentage") # Need to alter legend 

Percent_ASL_map = tm_shape(percent_ASLBYborough) +
  tm_polygons("Percentage", style = "pretty", palette = "Oranges") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)

tmap_save(Percent_ASL_map, filename = "./Maps/Percent_ASL_map.png")

#########
# Overall map of cycling infrastructure
map_ASL = mapview(f_advanced_stop_line$geometry, color = "blue")
mapshot(map_ASL, file = paste0(getwd(),"/Maps/","All_ASL_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping ASL feeder lanes
# a) get CID feeder lane
feed_ASL = f_advanced_stop_line %>%
  filter(ASL_FDR ==TRUE)

total_length_feed_ASL = sum(feed_ASL$length) # = 8020.9 [m]

feed_ASL_map = mapview(feed_ASL$geometry, color = "red") # map it

# b) Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it


# Create synchronised map of ASL v open TFL cycle routes 
sync(map_ASL, map_open_TFL_CR, no.initial.sync = FALSE) 

x = mapview(f_advanced_stop_line, color = "red") + mapview(open_TFL_CR$geometry, alpha = 0.8) 

mapshot(x, file = paste0(getwd(),"/Maps/","ASLvTFLcr.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))# (have to take png via Rstudio as no code available for synced image saving)

