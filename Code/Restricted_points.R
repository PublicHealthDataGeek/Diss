####################################
# TFL CID - restricted points      #
# Created 03/04/2020               #
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
restricted_points = get_cid_points(type = "restricted_point")
class(restricted_points)
str(restricted_points)

# check completeness of variables
unique(restricted_points$FEATURE_ID) # 180 unique variables
unique(restricted_points$BOROUGH) # 27 Boroughs, no NAS
unique(restricted_points$SVDATE) # 73 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(restricted_points$RST_STEPS)
unique(restricted_points$RST_LIFT)

# convert certain columns to factors
f_variables = c("RST_STEPS", "RST_LIFT")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_restricted_points = restricted_points %>%
  mutate_at(f_variables, as.factor)
f_restricted_points$BOROUGH = factor(f_restricted_points$BOROUGH, exclude = NULL)

glimpse(f_restricted_points) # check converted ok
levels(f_restricted_points$BOROUGH) # have 27 and no NA value

# create new df without geommetry that enables faster analysis of data
non_geom_f_restricted_points = st_drop_geometry(f_restricted_points)
str(non_geom_f_restricted_points)

# create summary of df
view(dfSummary(non_geom_f_restricted_points))
x = non_geom_f_restricted_points %>%
  count(BOROUGH)  

# examine URL data
count_photo1 =  non_geom_f_restricted_points %>%
  count(PHOTO1_URL) # 12 have no asset photo 1
count_photo2 =  non_geom_f_restricted_points %>%
  count(PHOTO2_URL) # 12 have no asset photo 2


# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of number of restricted points
count_restricted_pointsBYborough = non_geom_f_restricted_points %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# join numbers to geometry
n_restricted_pointsBYborough = left_join(boroughs, count_restricted_pointsBYborough)

# plot counts of restricted points by Borough
qtm(n_restricted_pointsBYborough, "count")
  
Restricted_points_borough_map = tm_shape(n_restricted_pointsBYborough) +
  tm_polygons("count", style = "fixed", palette = "Greens",
              breaks = c(1, 5, 10, 15, 20, 50),
              textNA = "Zero restricted points",
              colorNA = "grey") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)
  
tmap_save(Restricted_points_borough_map, filename = "./Maps/Count_restricted_points_borough_map.png")

# Overall map of restricted points
map_restricted_points = mapview(f_restricted_points$geometry, color = "blue", cex = 0.5)
mapshot(map_restricted_points, file = paste0(getwd(),"/Maps/","All_restricted_points_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping restricted points to TFL cycling infrastructure
# Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open


# Create overlapping map of Restriced points and TFL cycle routes
mapview(f_restricted_points$geometry, color = "red", cex = 1) + mapview(open_TFL_CR$geometry, lwd = 3)


# Create overlapping map of Signal and TFL cycle routes with one side zoomed in
x = mapview(f_restricted_points$geometry, color = "red", cex = 1) + mapview(open_TFL_CR$geometry, lwd = 3)
y = mapview(f_restricted_points$geometry, color = "red", cex = 1) + mapview(open_TFL_CR$geometry, lwd = 3)
sync(x, y, sync = "none") 
