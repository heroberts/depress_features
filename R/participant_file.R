library(tidyverse)
library(foreign)
library(sjlabelled)
library(lubridate)
library(stringr)
library(sf)

# Load and format data ---------------------------
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

# remove sessions with 0 or NA cumulative time
# here we can adjust duration of necessary measurement time
keep_sessions <- keep_sessions %>%
  filter(!cumulative_measurement_time == 0) %>%
  filter(!is.na(cumulative_measurement_time))

# visual check option - no duplicates after removal of empty sessions - confirmed
# duplicated(keep_sessions$cbs_id)

# add column names to fit RAPIDS (AWARE) format
keep_sessions$count <- NULL
keep_sessions$device_id <- keep_sessions$id
keep_sessions$fitbit_id <- NA
keep_sessions$empatica_id <- NA
keep_sessions$platform <- keep_sessions$operating_system
keep_sessions$label <- NA
keep_sessions$label <- as.character(keep_sessions$label)
keep_sessions$pid <- sprintf('p%0.3d', 1:661)
keep_sessions$platform <- as.character("android")

keep_sessions$start_date <- rep("2018-09-01",length(keep_sessions$id))
keep_sessions$end_date <- rep("2018-11-01",length(keep_sessions$id))

# Write data ---------------------------
# write.csv(keep_sessions, "C:/Users/hanna/OneDrive - Universiteit Utrecht/NEEDS/Paper 4/depress_features/results/participant_file.csv")

# create test file with only first participant
#test_ppt <- keep_sessions[1,]
write.csv(keep_sessions, "C:/Users/hanna/OneDrive - Universiteit Utrecht/NEEDS/Paper 4/depress_features/results/participant_file.csv")

