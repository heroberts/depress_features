library(tidyverse)
library(foreign)
library(lubridate)
library(stringr)
library(sf)

# Clean session dataset -------

sessions_file <- "data/sessions.csv" 
sessions <- read.csv(sessions_file)

# confirm which cbs_ids and session numbers are from testing phase
rm_ids <- subset(sessions, cbs_id < 9000000000) %>%
  select(id) %>%
  as.data.frame()

# visual check that rm_ids are session numbers 1 to 49; remove
rm(rm_ids)

# remove test phase sessions from sessions
keep_sessions <- sessions[-c(1:49),]

# check for multiple sessions
keep_sessions <- keep_sessions %>% 
  group_by(cbs_id) %>% 
  mutate(count= n()) %>%
  as.data.frame()

# Remove sessions with 0 or NA measurement time (measured in days) or no GPS permission -----
keep_sessions <- keep_sessions %>%
  filter(!cumulative_measurement_time == 0) %>%
  filter(!is.na(cumulative_measurement_time)) %>%
  filter(permissions_gps == TRUE)

# visual check option - no duplicates after removal of empty sessions - confirmed
# duplicated(keep_sessions$cbs_id)

rm(sessions, sessions_file)

# Stitch GPS files together -----------

filenames <- list.files(path="data/gps/", pattern="*.csv")
fullpath = file.path("data/gps", filenames)
full_gps <- do.call("rbind", lapply(fullpath, FUN=function(files){ read.csv(files)}))

# Remove participants without matching session -------

full_gps <- subset(full_gps, cbs_id %in% keep_sessions$cbs_id)

rm(sessions_file, sessions)

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

rm(gps_outNL, full_gps_proj, nl_border)

# Remove altitude outliers ---------

full_gps_proj_nl <- full_gps_proj_nl[full_gps_proj_nl$altitude < 330 & full_gps_proj_nl$altitude > -7 , ]
#note: if altitude is not known, 0.0 is given

# Format GPS dataset for export ----------------
# order by session ID and timestamp
full_gps_proj_nl <- full_gps_proj_nl[with(full_gps_proj_nl, order(full_gps_proj_nl$session_id, full_gps_proj_nl$timestamp)), ]

# device_id
full_gps_proj_nl$device_id <- as.character(full_gps_proj_nl$session_id)

# time
full_gps_proj_nl$timestamp <- as.POSIXct(strptime(full_gps_proj_nl$timestamp, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
full_gps_proj_nl$timestamp <- as.numeric(full_gps_proj_nl$timestamp) * 1000

# lat/long
full_gps_proj_nl$double_latitude <- full_gps_proj_nl$latitude
full_gps_proj_nl$double_longitude <- full_gps_proj_nl$longitude

# bearing
full_gps_proj_nl$double_bearing <- full_gps_proj_nl$bearing

# speed
full_gps_proj_nl$double_speed <- full_gps_proj_nl$speed

# altitude
full_gps_proj_nl$double_altitude <- full_gps_proj_nl$altitude

# provider
full_gps_proj_nl$provider <- as.character("gps")

# accuracy
full_gps_proj_nl$accuracy <- full_gps_proj_nl$horizontal_accuracy


# Write out GPS file ---------------------------
write.csv(full_gps_proj_nl, "C:/Users/hanna/OneDrive - Universiteit Utrecht/NEEDS/Paper 4/depress_features/results/gps.csv")

# Format sessions file for export

# Remove sessions of those not in GPS dataset
keep_sessions <- subset(keep_sessions, cbs_id %in% full_gps_proj_nl$cbs_id)

# add column names to fit RAPIDS (AWARE) format
keep_sessions$count <- NULL
keep_sessions$device_id <- keep_sessions$id
keep_sessions$fitbit_id <- NA
keep_sessions$empatica_id <- NA
keep_sessions$platform <- keep_sessions$operating_system
keep_sessions$label <- NA
keep_sessions$label <- as.character(keep_sessions$label)
keep_sessions$pid <- sprintf('p%0.3d', 1:561)
keep_sessions$platform <- as.character("android")

keep_sessions$start_date <- rep("2018-09-01",length(keep_sessions$id))
keep_sessions$end_date <- rep("2018-11-01",length(keep_sessions$id))

# Write data ---------------------------
write.csv(keep_sessions, "C:/Users/hanna/OneDrive - Universiteit Utrecht/NEEDS/Paper 4/depress_features/results/participant_file.csv")


