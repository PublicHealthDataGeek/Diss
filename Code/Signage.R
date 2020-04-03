####################################
# TFL CID - signage                 #
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

signage = get_cid_points(type = "signage")
class(signage)
str(signage)


# convert certain columns to factors
f_variables = c("SS_ROAD", "SS_PATCH", "SS_FACING", "SS_NOCYC", "SS_NOVEH",
                "SS_NOLEFT", "SS_NORIGH", "SS_LEFT", "SS_RIGHT", "SS_NOEXCE",
                "SS_DISMOU", "SS_END", "SS_CYCSMB", "SS_PEDSMB", "SS_BUSSMB",
                "SS_SMB", "SS_LNSIGN", "SS_ARROW", "SS_NRCOL", "SS_NCN",
                "SS_LCN", "SS_SUPERH", "SS_QUIETW", "SS_GREENW",
                "SS_DESTN", "SS_CIRC", "SS_EXEMPT")

# convert columns to factors CLT_ACCESS not converted as 721 different values)
f_signage = signage %>%
  mutate_at(f_variables, as.factor)
f_signage$BOROUGH = factor(signage$BOROUGH, exclude = NULL) # these variables are categorical done separately as have NAs
f_signage$SS_ROAD = factor(signage$SS_ROAD, exclude = NULL) 
f_signage$SS_COLOUR = factor(signage$SS_COLOUR, exclude = NULL)

x$SS_ROUTEN = factor(signage$SS_ROUTEN, exclude = NULL) # these 3 lines do not run but used for illustration
x$SS_ACCESS = factor(signage$SS_ACCESS, exclude = NULL)
x$SS_NAME = factor(signage$SS_NAME, exclude = NULL)

# recode SS_CYCSMB where values include "FALSE" and "FASLE"
non_geom_f_signage$SS_CYCSMB= fct_collapse(non_geom_f_signage$SS_CYCSMB, 
                                           "FALSE" = c("FALSE", "FASLE"))
  
glimpse(f_signage) # check converted ok

# examine levels - does not run for the ROUTEN, ACCESS and NAME ones
levels(f_signage$BOROUGH) # have 33 and NA value
levels(f_signage$SS_COLOUR) # have 4 colour categories plus "<Null>", "NONE" and NA
levels(f_signage$SS_ROAD) # have T, F and NA
levels(non_geom_f_signage$SS_CYCSMB) # now just true and false
levels(x$SS_ROUTEN) # 428 values plus NA
levels(x$SS_ACCESS) # 801 values plus NA
levels(x$SS_NAME) # 62 values plus "UNKNOWN", NA and " "

# create new df without geommetry that enables faster analysis of data
non_geom_f_signage = st_drop_geometry(f_signage)
str(non_geom_f_signage)

# create summary of df
view(dfSummary(non_geom_f_signage))





# Read in London Boroughs to add to map and code so can be joined to CID data
boroughs <- st_read("./map_data/London_Borough_Excluding_MHW.shp")
boroughs = rename(boroughs, BOROUGH = NAME)
boroughs$BOROUGH = fct_recode(boroughs$BOROUGH, "Kensington & Chelsea" = "Kensington and Chelsea", 
                              "Barking & Dagenham" = "Barking and Dagenham",
                              "Hammersmith & Fulham" = "Hammersmith and Fulham")


#a) map borough level data of count
count_signageBYborough = non_geom_f_signage %>%
  group_by(BOROUGH) %>%
  summarise(count = n())

# delete 'NA' row
count_signageBYborough = count_signageBYborough[-c(34),]

# join numbers to geometry
n_signageBYborough = left_join(boroughs, count_signageBYborough)

# plot counts of signals by Borough
qtm(n_signageBYborough, "count")
  
Count_signage_borough_map = tm_shape(n_signageBYborough) +
  tm_polygons("count", style = "fixed", palette = "Greens", 
              breaks = c(1, 2000, 4000, 6000, 8000, 10000, 12000)) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.position = c("left","bottom"),
            legend.bg.alpha = 1)
  
tmap_save(Count_signage_borough_map, filename = "./Maps/Count_signage_borough_map.png")

# Overall map of signals

map_signage = mapview(f_signage$geometry, color = "blue", cex = 0.5)
mapshot(map_signals, file = paste0(getwd(),"/Maps/","All_signage_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

