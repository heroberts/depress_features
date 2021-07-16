library(tidyverse)
library(foreign)
library(lubridate)
library(stringr)
library(sf)

# Stitch all GPS files together ---------------------------

filenames <- list.files(path="data/gps/", pattern="*.csv")
fullpath = file.path("data/gps", filenames)
full_gps <- do.call("rbind", lapply(fullpath, FUN=function(files){ read.csv(files)}))

# Format dataset ----------------
# order by session ID and timestamp
full_gps <- full_gps[with(full_gps, order(full_gps$session_id, full_gps$timestamp)), ]

# device_id
full_gps$device_id <- as.character(full_gps$session_id)

# time
full_gps$timestamp <- as.POSIXct(strptime(full_gps$timestamp, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
full_gps$timestamp <- as.numeric(full_gps$timestamp) * 1000

# lat/long
full_gps$double_latitude <- full_gps$latitude
full_gps$double_longitude <- full_gps$longitude

# bearing
full_gps$double_bearing <- full_gps$bearing

# speed
full_gps$double_speed <- full_gps$speed

# altitude
full_gps$double_altitude <- full_gps$altitude

# provider
full_gps$provider <- as.character("gps")

# accuracy
full_gps$accuracy <- full_gps$horizontal_accuracy

# Remove participants who left NL -------
# read in border
nl_border <- st_read("https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&typeName=bestuurlijkegebieden:Landgebied&outputFormat=application%2Fjson") %>%
  st_cast(., "POLYGON") %>%
  st_transform(., crs= 4326)

# add geometry to gps points
full_gps_proj <- st_as_sf(full_gps, coords = c("longitude", "latitude"), crs=4326)
st_is_longlat(full_gps_proj)

# select points outside NL border
gps_outNL <- full_gps_proj[!lengths(st_intersects(full_gps_proj, nl_border)), ]

# remove participants that had points outside the border
full_gps_proj_nl <- subset(full_gps_proj, !cbs_id %in% gps_outNL$cbs_id)

# remove geometry
full_gps_proj_nl <- st_drop_geometry(full_gps_proj_nl)

# Remove outliers ---------
# remove points with extreme altitudes

full_gps_proj_nl <- full_gps_proj_nl[full_gps_proj_nl$altitude < 330 & full_gps_proj_nl$altitude > -7 , ]
#note: if altitude is not known, 0.0 is given

# Write data ---------------------------
write.csv(full_gps_proj_nl, "C:/Users/hanna/OneDrive - Universiteit Utrecht/NEEDS/Paper 4/depress_features/results/full_gps.csv")


