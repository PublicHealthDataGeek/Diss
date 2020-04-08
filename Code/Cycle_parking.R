####################################
# TFL CID - cycle parking                 #
# Created 02/04/2020               #
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
cycle_parking = get_cid_points(type = "cycle_parking")
class(cycle_parking)
str(cycle_parking)

# check completeness of variables
unique(cycle_parking$FEATURE_ID) # 23758 unique variables
unique(cycle_parking$BOROUGH) # 33 Boroughs no NAS
unique(cycle_parking$SVDATE) # 331 unique survey dates, all of which are valid date
# the below all have just true and false
unique(crossings$CRS_SIGNAL)
unique(crossings$CRS_CYGAP)
unique(crossings$CRS_LEVEL)
unique(crossings$CRS_PEDEST)
unique(crossings$CRS_SEGREG)



# convert certain columns to factors
f_variables = c("PRK_CARR", "PRK_COVER", "PRK_SECURE", "PRK_LOCKER", "PRK_SHEFF", "PRK_MSTAND",
                "PRK_PSTAND", "PRK_HOOP", "PRK_POST", "PRK_BUTERF", "PRK_WHEEL", "PRK_HANGAR",
                "PRK_TIER", "PRK_OTHER")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_cycle_parking = cycle_parking %>%
  mutate_at(f_variables, as.factor)
f_cycle_parking$BOROUGH = factor(cycle_parking$BOROUGH, exclude = NULL)

glimpse(f_cycle_parking) # check converted ok
levels(f_cycle_parking$BOROUGH) # have 33 and no NA value

# create new df without geommetry that enables faster analysis of data
non_geom_f_cycle_parking = st_drop_geometry(f_cycle_parking)
str(non_geom_f_cycle_parking)

# create summary of df
view(dfSummary(non_geom_f_cycle_parking))

# examine URL data
count_photo1 =  non_geom_f_cycle_parking %>%
  count(PHOTO1_URL) # 299 have no asset photo 1
count_photo2 =  non_geom_f_cycle_parking %>%
  count(PHOTO2_URL) # 298 have no asset photo 2

# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of number of parking facilities
count_cycle_parkingBYborough = non_geom_f_cycle_parking %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# join numbers to geometry
n_cycle_parkingBYborough = left_join(boroughs, count_cycle_parkingBYborough)

# plot counts of cycle counts by Borough
qtm(n_cycle_parkingBYborough, "count")
  
Cycle_parking_borough_map = tm_shape(n_cycle_parkingBYborough) +
  tm_polygons("count", style = "pretty", palette = "Greens") +
  tm_text("count", size = 0.75) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("right","bottom"),
            legend.bg.alpha = 1)
  
tmap_save(Cycle_parking_borough_map, filename = "./Maps/Count_cycle_parking_borough_map.png")

# Overall map of cycle parking

map_parking = mapview(f_cycle_parking$geometry, color = "blue", cex = 0.5)
mapshot(map_parking, file = paste0(getwd(),"/Maps/","All_parking_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping parking to TFL cycling infrastructure
# Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it


# Create synchronised map of CID signals v open TFL cycle routes 
sync(map_parking, map_open_TFL_CR, no.initial.sync = FALSE) 

# Create overlapping map of Signal and TFL cycle routes
mapview(f_cycle_parking$geometry, color = "blue", cex = 0.5) + mapview(open_TFL_CR$geometry, lwd = 3)


# SEcure parking by borough
secure_parkBYborough = non_geom_f_cycle_parking %>%
  filter(PRK_SECURE == "TRUE") %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# join numbers to geometry
secure_cycle_parkingBYborough = left_join(boroughs, secure_parkBYborough)

# plot counts of secure cycle parking by Borough
qtm(secure_cycle_parkingBYborough, "count")

Secure_parking_borough_map = tm_shape(secure_cycle_parkingBYborough) +
  tm_polygons("count", style = "fixed", palette = "Reds", 
              breaks = c(1, 10, 20, 30, 40),
              textNA = "Zero secure cycle parking",
              colorNA = "grey") +
  tm_text("count") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)
tmap_save(Secure_parking_borough_map, filename = "./Maps/Count_secure_parking_borough_map.png")
  


# Bike parking capacity by borough
# This code maps all boroughs correctly apart from Newham and Southwark where they are NA
wrong_capacity_cycle_parkingBYborough = non_geom_f_cycle_parking %>%
  select(BOROUGH, PRK_CPT) %>%
  group_by(BOROUGH) %>%
  summarise("Cycle capacity" = sum(PRK_CPT))

# join numbers to geometry - this is wehre the coding starts again once I figure out what is wrong
wrong_capacity_cycle_parkingBYborough = left_join(boroughs, wrong_capacity_cycle_parkingBYborough)

qtm(wrong_capacity_cycle_parkingBYborough, "Cycle capacity")

wrong_capacity_cycle_parkingBYborough = tm_shape(correct_capacity_cycle_parkingBYborough) +
  tm_polygons("Cycle capacity", style = "fixed", palette = "YlOrRd", 
              breaks = c(1, 2000, 4000, 6000, 8000, 10000, 12000, 14000)) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("right","bottom"),
            legend.bg.alpha = 1)
tmap_save(wrong_capacity_cycle_parkingBYborough, filename = "./Maps/WRONG_capacity_parking_borough_map.png")

# Investigating issue with Newham and Southwark
data = non_geom_f_cycle_parking %>%
  select(BOROUGH, PRK_CPT) 
str(data) # Borough is a factor with 33 levels, PRK_CPT is an integer

n_s = data %>%
  filter(BOROUGH == "Newham" | BOROUGH == "Southwark") # just get Newham and Southwark

unique(n_s$BOROUGH)  # Southwark, Newham but states 33 levels of factors.  
unique(n_s$PRK_CPT) # integers but there is a NA category.

wrong_n_s = n_s %>%
  group_by(BOROUGH) %>%
  summarise("Cycle capacity" = sum(PRK_CPT)) # => NA

# So address NAs
correct_n_s = n_s %>%
  mutate_if(is.integer, ~replace(PRK_CPT, is.na(PRK_CPT), 0))

unique(correct_n_s$PRK_CPT) # now there are no NAs


# Redo maps with correct results by sorting out NAs
correct_capacity_cycle_parkingBYborough = non_geom_f_cycle_parking %>%
  select(BOROUGH, PRK_CPT) %>%
  mutate_if(is.integer, ~replace(PRK_CPT, is.na(PRK_CPT), 0)) %>%
  group_by(BOROUGH) %>%
  summarise("Cycle capacity" = sum(PRK_CPT))

# join numbers to geometry 
correct_capacity_cycle_parkingBYborough = left_join(boroughs, correct_capacity_cycle_parkingBYborough)
qtm(correct_capacity_cycle_parkingBYborough, "Cycle capacity")  # Test map works

Correct_capacity_cycle_parkingBYborough = tm_shape(correct_capacity_cycle_parkingBYborough) +
  tm_polygons("Cycle capacity", style = "fixed", palette = "YlOrRd", 
              breaks = c(1, 2000, 4000, 6000, 8000, 10000, 12000, 14000)) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)
tmap_save(Correct_capacity_cycle_parkingBYborough, filename = "./Maps/CORRECT_capacity_parking_borough_map.png")




