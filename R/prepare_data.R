library(tidyverse)
library(foreign)
library(sjlabelled)
library(lubridate)
library(stringr)
library(sf)

# Load data ---------------------------

app_file <- "data/app_info.csv" 
sessions_file <- "data/sessions.csv" 
gps_file <- "data/gps.csv" 
bluetooth_file <- "data/bluetooth.csv" 
calls_file <- "data/calls.csv" 
msg_file <- "data/messages.csv" 
sm_file <- "data/social_media.csv"
surv_file <- "data/STE2018U_RESPONS_EN.sav"
id_file <- "data/merged_id_numbers.csv"

# read in sensor data
app_info <- read.csv(app_file)
sessions <- read.csv(sessions_file)
gps <- read.csv(gps_file)
bluetooth <- read.csv(bluetooth_file)
calls <- read.csv(calls_file)
msg <- read.csv(msg_file)
sm <- read.csv(sm_file)

# confirm which cbs_ids and session numbers are from testing phase
rm_ids <- subset(sessions, cbs_id < 9000000000) %>%
  select(id) %>%
  as.data.frame()

# visual check that rm_ids are session numbers 1 to 49; remove
rm(rm_ids)

# remove session IDs greater than 49 from all datasets
combo <- list("calls" = calls, "msg" = msg, "bluetooth" = bluetooth, "sm" = sm) %>%
  lapply(., subset, session_id > 49) %>%
  list2env(., envir=.GlobalEnv)

rm(combo)

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

# select IDS
keep_ids <- keep_sessions %>%
  select(c(id, cbs_id)) %>%
  as.data.frame()

# get cbs_ids that gave permission for msg data
msg_sessions <- keep_sessions %>%
  filter(permissions_messages == TRUE) %>%
  select(c(id, cbs_id, permissions_messages)) %>%
  as.data.frame() 

# join to msg data
msg <- msg %>%
  right_join(msg_sessions, by = c("session_id" = "id")) 

# get cbs_ids that gave permission for sm data
call_sessions <- keep_sessions %>%
  filter(permissions_calls == TRUE) %>%
  select(c(id, cbs_id, permissions_calls)) %>%
  as.data.frame() 

# join to call data
calls <- calls %>%
  right_join(call_sessions, by = c("session_id" = "id")) 

# get cbs_ids that gave permission for sm data
sm_sessions <- keep_sessions %>%
  filter(permissions_social_media == TRUE) %>%
  select(c(id, cbs_id, permissions_social_media)) %>%
  as.data.frame() 

# join to sm data
sm <- sm %>%
  right_join(sm_sessions, by = c("session_id" = "id")) 

# join session ids to bluetooth data
# left_join bc bluetooth scans of 0 are included (contrast to zero calls/messages) 
# NA means bluetooth not turned on, not that there was no exposure
bluetooth <- bluetooth %>%
  left_join(keep_ids, by = c("session_id" = "id")) 


# read in SPSS data, select PHQ9, age, sex, ID numbers
surv_data <- read.spss(surv_file, use.value.labels = F, to.data.frame =  T, trim.factor.names = T, trim_values = T) %>%
  select(WE_ID, Age, Sex, PHQ9_Sum27, PHQ9_1, PHQ9_2, PHQ9_3, PHQ9_4, PHQ9_5, PHQ9_6, PHQ9_7, PHQ9_8, PHQ9_9) %>%
  remove_all_labels(.)

# read in SPSS data, select social accounts data
sm_acc <- read.spss(surv_file, use.value.labels = T, to.data.frame =  T, trim.factor.names = T, trim_values = T) %>%
  select(WE_ID, Freq_Chat, Freq_Tel, Freq_Facebook, Freq_Twitter, Freq_Snapchat, Freq_Instagram, Freq_YouTube, AFL_TelSoc, Account1, Account2, Account3, Account4, Account5) %>%
  remove_all_labels(.)

# join survey data to id numbers
ids <- read.csv(id_file)

indiv_data <- ids %>%
  inner_join(surv_data, by = c("we_id" = "WE_ID")) 

sm_acc <- ids %>%
  inner_join(sm_acc, by = c("we_id" = "WE_ID")) 

# Compute message features ---------------------------
# formatting
msg$timestamp <- as.POSIXct(strptime(msg$timestamp, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
msg$timestamp_local <- format(msg$timestamp, tz="Europe/Amsterdam",usetz=TRUE)

# add weekday/weekend flag
msg$weekend = as.numeric(chron::is.weekend(msg$timestamp_local))

# count of received/sent/total messages
msg_features <- msg %>% 
  group_by(cbs_id) %>% 
  summarize(received_count = sum(str_count(traffic_direction, 'received')),
            sent_count = sum(str_count(traffic_direction, 'sent')),
            total_msg = received_count + sent_count,
            msg_contacts = max(phonenumber_id)) %>%
  select(cbs_id, total_msg, received_count, sent_count, msg_contacts)


# Compute call features ---------------------------

# formatting
calls$start_call <- as.POSIXct(strptime(calls$start_call, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
calls$start_call_local <- format(calls$start_call, tz="Europe/Amsterdam",usetz=TRUE)

calls$end_call <- as.POSIXct(strptime(calls$end_call, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
calls$end_call_local <- format(calls$end_call, tz="Europe/Amsterdam",usetz=TRUE)

# add weekday/weekend flag
calls$weekend = as.numeric(chron::is.weekend(calls$start_call_local))

# duration of each call
calls$duration <- round(difftime(calls$end_call_local, calls$start_call_local, units = "mins"), 2)

# sum total call time per participant
call_features <- calls %>% 
  group_by(cbs_id) %>% 
  summarize(total_calls = sum(duration))

# count of incoming/outgoing/total calls
count_calls <- calls %>% 
  group_by(cbs_id) %>% 
  summarize(incoming_count = sum(str_count(traffic_direction, 'incoming')),
            outgoing_count = sum(str_count(traffic_direction, 'outgoing')),
            calls_count = incoming_count + outgoing_count,
            call_contacts = max(phonenumber_id)) %>%
  select(cbs_id, calls_count, incoming_count, outgoing_count, call_contacts)

call_features <- left_join(call_features,  count_calls)

# add weekday split
calls_weekday <- calls %>% 
  filter(weekend == 0) %>%
  group_by(cbs_id) %>% 
  summarize(weekday_call = sum(duration))

call_features <- left_join(call_features,  calls_weekday)

# add weekend split
calls_weekend <- calls %>% 
  filter(weekend == 1) %>%
  group_by(cbs_id) %>% 
  summarize(weekend_call = sum(duration))

call_features <- left_join(call_features, calls_weekend)

rm(calls_weekday, calls_weekend, count_calls)

# Compute social media features ---------------------------

# formatting
sm$start_usage <- as.POSIXct(strptime(sm$start_usage, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
sm$start_usage_local <- format(sm$start_usage, tz="Europe/Amsterdam",usetz=TRUE)

sm$end_usage <- as.POSIXct(strptime(sm$end_usage, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
sm$end_usage_local <- format(sm$end_usage, tz="Europe/Amsterdam",usetz=TRUE)

# add weekday/weekend flag
sm$weekend = as.numeric(chron::is.weekend(sm$start_usage_local))

# duration of each use
sm$duration <- round(difftime(sm$end_usage_local, sm$start_usage_local, units = "mins"), 2)

# sum total social media duration per participant
sm_features <- sm %>% 
          group_by(cbs_id) %>% 
          summarize(total_sm_duration = sum(duration))

# add weekday split
sm_weekday_time <- sm %>% 
   filter(weekend == 0) %>%
   group_by(cbs_id) %>% 
   summarize(sm_weekday = sum(duration))

sm_features <- right_join(sm_features, sm_weekday_time)

# add weekend split
sm_weekend_time <- sm %>% 
  filter(weekend == 1) %>%
  group_by(cbs_id) %>% 
  summarize(sm_weekend = sum(duration))

sm_features <- right_join(sm_features, sm_weekend_time)

rm(sm_weekday_time, sm_weekend_time)

# join number of social media accounts (reported in survey)
sm_acc_num <- sm_acc %>%
  select(cbs_id, AFL_TelSoc)

sm_features <- left_join(sm_features, sm_acc_num) %>%
  rename(count_sm_accounts =  AFL_TelSoc)

# Compute bluetooth features ---------------------------
bluetooth$start_timestamp <- as.POSIXct(strptime(bluetooth$start_timestamp, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
bluetooth$end_timestamp <- as.POSIXct(strptime(bluetooth$end_timestamp, "%d-%m-%Y %H:%M:%S", tz ="Africa/Accra"), tz="Africa/Accra")
bluetooth$timestamp_local <- format(bluetooth$start_timestamp, tz="Europe/Amsterdam",usetz=TRUE)

# add weekday/weekend flag
bluetooth$weekend = as.numeric(chron::is.weekend(bluetooth$timestamp_local))

# mean device count
bluetooth_features <- bluetooth %>% 
  group_by(cbs_id) %>% 
  summarize(mean_devices = mean(device_count),
            total_devices = sum(device_count)) %>%
  select(cbs_id, mean_devices, total_devices)

# add weekday split
devices_weekday <- bluetooth %>% 
  filter(weekend == 0) %>%
  group_by(cbs_id) %>% 
  summarize(mean_devices_weekday = mean(device_count))

bluetooth_features <- left_join(bluetooth_features, devices_weekday)

# add weekend split
devices_weekend <- bluetooth %>% 
  filter(weekend == 1) %>%
  group_by(cbs_id) %>% 
  summarize(mean_devices_weekend = mean(device_count))

bluetooth_features <- left_join(bluetooth_features, devices_weekend)

rm(devices_weekday, devices_weekend)

# Join to make social features df ---------------------------

social_features <- list(msg_features, call_features, sm_features, bluetooth_features) %>% reduce(full_join, by = "cbs_id")

# To do: add zeros so that it is clear in the join who gave permission and who not


# Compute mobility features ---------------------------

# see RAPIDS

