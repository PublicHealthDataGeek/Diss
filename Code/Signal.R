####################################
# TFL CID - signal                 #
# Created 01/04/2020               #
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

signal = get_cid_points(type = "signal") # n = 443
class(signal)
str(signal)

# check completeness of variables
unique(signal$FEATURE_ID) # 443 unique variables
unique(signal$BOROUGH) # 23 Boroughs, no NAS
unique(signal$SVDATE) # 111 unique survey dates, all of which are valid dates

# the below all have just true and false
unique(signal$SIG_HEAD)
unique(signal$SIG_SEPARA)
unique(signal$SIG_EARLY)
unique(signal$SIG_TWOSTG)
unique(signal$SIG_GATE)


# convert certain columns to factors
f_variables = c("SIG_HEAD", "SIG_SEPARA", "SIG_EARLY", "SIG_TWOSTG", "SIG_GATE")

# convert columns to factors (BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values)
f_signal = signal %>%
  mutate_at(f_variables, as.factor)
f_signal$BOROUGH = factor(signal$BOROUGH, exclude = NULL)

glimpse(f_signal) # check converted ok
levels(f_signal$BOROUGH) # only have 23 and no NA value

# create new df without geommetry that enables faster analysis of data
non_geom_f_signal = st_drop_geometry(f_signal)
str(non_geom_f_signal)
x = non_geom_f_signal %>%
  count(BOROUGH)  

# create summary of df
view(dfSummary(non_geom_f_signal))

# examine URL data
count_photo1 =  non_geom_f_signal %>%
  count(PHOTO1_URL) # 8 have no asset photo 1
count_photo2 =  non_geom_f_signal %>%
  count(PHOTO2_URL) # 8 have no asset photo 2


# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_signalBYborough = non_geom_f_signal %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
# count_signalBYborough = count_signalBYborough[-c(34),]  NOT REQUIRED AS NO NA ROW

# join numbers to geometry
n_signalBYborough = left_join(boroughs, count_signalBYborough)

# plot counts of signals by Borough
qtm(n_signalBYborough, "count")
  
Count_signal_borough_map = tm_shape(n_signalBYborough) +
  tm_polygons("count", style = "fixed", palette = "Greens", 
              breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
              textNA = "Zero signals",
              colorNA = "grey") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("right","bottom"),
            legend.bg.alpha = 1)
  
tmap_save(Count_signal_borough_map, filename = "./Maps/Signals/Count_signal_borough_map.png")

# Overall map of signals
map_signals

map_signals = mapview(f_signal$geometry, color = "blue", cex = 1)
mapshot(map_signals, file = paste0(getwd(),"/Maps/","All_signals_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

# Mapping segregated cycling infrastructure
# Obtain TFL Cycle route data
urlCR = "https://cycling.data.tfl.gov.uk/CycleRoutes/CycleRoutes.json"
TFL_CR = geojson_sf(urlCR)

open_TFL_CR = TFL_CR %>%
  filter(Status == "Open") # limit to infrastructure that is open
map_open_TFL_CR = mapview(open_TFL_CR$geometry) # map it


# Create synchronised map of CID signals v open TFL cycle routes 
sync(map_signals, map_open_TFL_CR, no.initial.sync = FALSE) 

# Create overlapping map of Signal and TFL cycle routes
mapview(f_signal$geometry, color = "blue", cex = 1) + mapview(open_TFL_CR$geometry)

# Create overlapping map of Signal and TFL cycle routes with one side zoomed in
x = mapview(f_signal$geometry, color = "blue", cex = 1) + mapview(open_TFL_CR$geometry)
y = mapview(f_signal$geometry, color = "blue", cex = 1) + mapview(open_TFL_CR$geometry)
sync(x, y, sync = "none") 
