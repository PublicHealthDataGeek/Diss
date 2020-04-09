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

# check completeness of variables
unique(signage$FEATURE_ID) # 118833 unique variables RWG999275 is present twice. 
unique(signage$BOROUGH) # 33 Boroughs plus a NA group
unique(signage$SVDATE) # 349 unique survey dates, all of which are valid date
# the below all have just true and false
unique(signage$SS_ROAD) # TRUE FALSE NA
unique(signage$SS_PATCH)
unique(signage$SS_FACING)
unique(signage$SS_NOCYC)
unique(signage$SS_NOVEH)
unique(signage$SS_CIRC)
unique(signage$SS_EXEMPT)
unique(signage$SS_NOLEFT)
unique(signage$SS_NORIGH)
unique(signage$SS_LEFT)
unique(signage$SS_RIGHT)
unique(signage$SS_NOEXCE)
unique(signage$SS_DISMOU)
unique(signage$SS_END)
unique(signage$SS_CYCSMB) # TRUE FALSE FASLE
unique(signage$SS_PEDSMB)
unique(signage$SS_BUSSMB)
unique(signage$SS_SMB)
unique(signage$SS_LNSIGN)
unique(signage$SS_ARROW)
unique(signage$SS_NRCOL)
unique(signage$SS_NCN)
unique(signage$SS_LCN)
unique(signage$SS_SUPERH)
unique(signage$SS_QUIETW)
unique(signage$SS_GREENW)
unique(signage$SS_ROUTEN)  # 429 different names including NA
unique(signage$SS_DESTN)
unique(signage$SS_ACCESS) # 802 different names including NA
unique(signage$SS_NAME) # 65 unique labels including NA
unique(signage$SS_COLOUR) #  NONE GREEN RED BLUE NA <Null> BUFF/YELLOW



# examine RWG999275
RWG_duplicate = signage %>%
  filter(FEATURE_ID == "RWG999275")
mapview(RWG_duplicate)


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

# recode SS_CYCSMB where values include "FALSE" and "FASLE"
non_geom_f_signage$SS_COLOUR = fct_collapse(non_geom_f_signage$SS_COLOUR, 
                                           "NONE" = NA) 

# create new df without geommetry that enables faster analysis of data
non_geom_f_signage = st_drop_geometry(f_signage)
str(non_geom_f_signage)

# create summary of df
view(dfSummary(non_geom_f_signage))

# examine URL data
count_photo1 =  non_geom_f_signage %>%
  count(PHOTO1_URL) # 31 have no asset photo 1
count_photo2 =  non_geom_f_signage %>%
  count(PHOTO2_URL) # 32 have no asset photo 2


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

map_signage = mapview(f_signage$geometry, color = "blue", cex = 0.1)
mapshot(map_signage, file = paste0(getwd(),"/Maps/Signage/","All_signage_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))

system.time(non_geom_f_signage %>%
              count(BOROUGH)) # elapsed = 0.042s
system.time(f_signage %>%
              count(BOROUGH)) # 13.821s

system.time(mapview(f_signage$geometry, color = "blue", cex = 0.1)) # 15.324 
