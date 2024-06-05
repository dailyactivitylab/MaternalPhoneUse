library("extrafont")
library ("reshape")
library("ppcor")
library("ggpubr")
library("tidyverse")
library("tidyr")
library("psych")
library("plyr")
library("readr")
library("utils")
library("purrr")
library("stringr")
library("rstatix")
library("lubridate")
library("ggplot2")
library("data.table")
library(scales)
library("dplyr")
library(twosamples)
library(gridExtra)


##########################################################################################################
####  P A R T 1 : C O N V E R T   R A W   B E I W E  S C R E E N  D A T A  T O  T I M E S E R I E S  #####
##########################################################################################################

### Note: Beiwe timeseries outputs are in the Data_Binned Folder as Beiwe_binned_infants

# select paths for raw beiwe data and where to output the processed event data and time-series data
beiwe_raw_data_path <- "../Data_Raw/Beiwe_raw_infants"
beiwe_output_path <- "../Data_Binned/Beiwe_binned_infants"

# %notin% operator (used later)
`%notin%` <- Negate(`%in%`)

# make a list of the participant folders-- each one has the raw beiwe data (screen on/off events and times)
# note: there is 1 CSV for every hour during the recording time interval

P_folders <- list.dirs(beiwe_raw_data_path, recursive = FALSE)


## FUNCTION READS IN AND CLEANS POWER STATE CSV FILE AND ADDS A COLUMN THAT HAS THE START TIME OF THE FILE FOR EVERY ROW
## note: this allows us to keep track of which file the event is from when the event data is concatenated into one dataframe

power_state_clean <- function(power_state_path) {
  
  power_state_df <- read.csv(power_state_path)
  power_state_df <- power_state_df %>%
    # only keep rows with the specified events (turning screen on and off, ios and android have different wording)
    filter(event %in% c("Unlocked", "Locked", "Screen turned on", "Screen turned off")) %>%
    # create a new column called date_time_UTC which converts UNIX time to UTC time of each row
    mutate(date_time_UTC = as.POSIXct(timestamp/1000, origin = '1970-01-01', tz = "UTC")) %>%
    # create a new column called date_time_CDT which converts UTC time to CDT time of each row
    mutate(date_time_CDT = as.POSIXct(format(date_time_UTC, tz = "America/Chicago", usetz=TRUE)))
  
  # if there is at least one instance of a consecutive repeated event
  if (length(which(power_state_df$event == lag(power_state_df$event))) > 0) { 
    # then keep the first event row
    power_state_df <- power_state_df[-which(power_state_df$event == lag(power_state_df$event)), ] 
  }
  
  # get the hour that this file starts at and make a column of that value repeating so that we know which file that row came from
  file_start_datetime <- floor_date(as.POSIXct(format(power_state_df$date_time_CDT[1], tz = "America/Chicago", usetx=TRUE)), "hour")
  power_state_df$file_start_CDT <- rep(file_start_datetime, nrow(power_state_df))
  return(power_state_df)
}


## FUNCTION TO GET RAW SCREEN DATA FOR ONE PARTICIPANT-- RETURNS A DATAFRAME WITH ALL SCREEN ON/OFF EVENTS DURING PARTICIPANT'S RECORDING
## note: p_folder is the participant's folder with all of its raw Beiwe data (screen-use (power state) is one of the folders among the other raw data such as accelerometer and gps)

get_daily_screen_data <- function(p_folder) {
  # if participant has a power state folder (screen on/off data is in this folder)
  if (dir.exists(paste(p_folder, "/power_state/", sep = ""))) {
    # create a list of the path names of the participant's power state files
    power_state_paths <- list.files(paste(p_folder, "/power_state", sep=""), full.names = TRUE)
    # read in power state csv files using power_state_clean() and concatenate them into 1 dataframe
    power_state_df <- bind_rows(lapply(power_state_paths, power_state_clean))
    # sort the dataframe by datetime
    power_state_df <- arrange(power_state_df, date_time_CDT)

    # if screen_df is empty
    if (dim(power_state_df)[1] == 0) {
      # then return null
      return(NULL)
    }
    # otherwise return power state dataframe
    else {
      return(power_state_df)
    }
  }
  
  # if participant doesn't have a power state folder, then return NULL
  else {
    return(NULL)
  }
}


# create empty lists that will store the lengths of each screen-on and screen-off event and the corresponding event types (1 for screen-on and 0 for screen-off)
all_day_event_durs <- c()
all_day_events <- c()

# loop through each participant, get their power state event data, create a timeseries for their screen usage (on/off)
for (p_folder in P_folders[1:length(P_folders)]) {
  # call get_daily_screen_data() to get a dataframe with all of the screen on/off events for one participant; time converted to CDT
  screen_df <- get_daily_screen_data(p_folder)
  # if get_daily_screen_data returns null, that means that the power state file was either empty or did not exist
  if (is.null(screen_df)) {
    next
  }
  
  # create new column in screen_df with the datetime of each event rounded to the nearest minute
  screen_df$rounded_date <- round_date(screen_df$date_time_CDT, unit = "1 minute")

  # create a series of every minute within the time range of the participant's data
  date_time_CDT <- seq(from = range(screen_df$rounded_date)[1], to = (range(screen_df$date_time_CDT)[2]) - minutes(1), 60)

  # create an all-zero series for every minute in the time range (the screen-on minutes will be changed to 1 later)
  is_on <- rep(0, length(date_time_CDT))
  
  # create an all-zero series for every minute in the time range which will keep track of the duration of each phone-on and phone-off event
  event_dur <- rep(0, length(date_time_CDT))
  
  # create an all-zero series for every minute in the time range which will keep track of the event count
  event_count <- rep(0, length(date_time_CDT))
  
  # create a series for every minute in the time range which will keep track of consecutive rows where there is more than an hour between timestamps (used later when finding event durations)
  file_gap <- rep("no", length(date_time_CDT))
  
  # get the indeces of rows where the following row is not in the same event file and not the immediate next hour after the current event file (file start time is more than an hour after the current row's file start time)
  # note: this indicates that there was at least one missing power state file or file with no events in it
  event_time_diffs <- diff(as.numeric(screen_df$file_start_CDT))
  event_file_gaps <- which(event_time_diffs > 3600)
  
  
  # create timeseries dataframe with the datetime, is_on, event_dur, event_count, and file_gap series
  screen_timeseries_df <- data.frame(date_time_CDT, is_on, event_dur, event_count, file_gap)
  
  # NOTE/REMINDER:
  #  screen_df is the dataframe with all of the current participant's screen-on and screen-off events (1 row per event)
  #  screen_timeseries_df is the dataframe with a row for every minute between the participant's first screen on/off event and the last

  # first, change is_on to NA in screen_timeseries_df for rows/times that didn't have an associated power state file (by looping through the file gap indeces)
  # NA is assigned to every minute between the last event before a file gap and the first event after a file gap
  
  # EXAMPLE: the 7th row of P49's screen_df is a lock event at 11:41; the 8th row is an unlock event at 13:25; the index of the file gap is 7 (row #) and that is already stored in event_file_gaps;
  #          we are only missing the 12:00 power state file. but, we do not include 11:41-12:00 or 13:00-13:25 in the dataset because we cannot confirm that Beiwe stopped recording exactly at 12:00 or that it started recording again exactly at 13:00;
  #          11:41 is removed as well because this is a rounded time stamp, which means that we have less than 30 seconds of information about that minute. our criteria for a phone-on or phone-off minute is that 30 or more seconds of the minute have the screen on or off.
  
  for (ii in event_file_gaps) {
    
    screen_timeseries_df$is_on[which((screen_timeseries_df$date_time_CDT) >= (screen_df$rounded_date[[ii]]) &
                                     (screen_timeseries_df$date_time_CDT < screen_df$rounded_date[[ii+1]]))] <- NA
  }
  
  # remove NA values and sort by datetime
  screen_timeseries_df <- na.omit(screen_timeseries_df)
  screen_timeseries_df <- screen_timeseries_df[order(as.Date(screen_timeseries_df$date_time_CDT)),]
  
  ## loop through every screen-on event and assign is_on to 1 for each minute until a screen-off event
  
  # for every row in the screen event data frame that has a screen turned on event
  for (i in which(screen_df$event %in% c("Unlocked", "Screen turned on"))) {
    # initialize j as the index of the row immediately after the screen turned on event row
    j <- i + 1
    # if the unlock event is the last event in the participants' data, then the index j won't exist in the dataframe
    # remove the rows after the unlock event time (because we don't know exactly when the recording stopped) and exit the for loop
    if (i == nrow(screen_df)) {
      screen_timeseries_df$is_on[which(screen_timeseries_df$date_time_CDT >= screen_df$rounded_date[i])] <- NA
      #min_after_last_event_count <- min_after_last_event_count + nrow(screen_timeseries_df$is_on[which(screen_timeseries_df$date_time_CDT >= screen_df$rounded_date[i])])
      screen_timeseries_df <- na.omit(screen_timeseries_df)
      break
    }

    # make sure the next event is the phone being locked and not just another time point when the phone is unlocked
    while(screen_df$event[j] %notin% c("Locked", "Screen turned off") & j < nrow(screen_df)) {
      j <- j + 1
    }
    
    # difference in seconds between the actual (not rounded) event timestamps
    actual_diff <- as.numeric(difftime(screen_df$date_time_CDT[j], screen_df$date_time_CDT[i], units = "secs"))
    
    # if two consecutive events are rounded to the same timestamp and the actual difference between timestamps is more than 30 seconds, remove both of the minutes
    # note: at the one-minute time scale, there isn't a good way to account for 30-59 seconds of phone-use that span two 1-minute bins. so these edge cases are removed
    if (screen_df$rounded_date[i] == screen_df$rounded_date[j] & actual_diff >= 30) {
      screen_timeseries_df$is_on[which(screen_timeseries_df$date_time_CDT == floor_date(screen_df$date_time_CDT[i], unit = "minute"))] <- NA
      screen_timeseries_df$is_on[which(screen_timeseries_df$date_time_CDT == round_date(screen_df$date_time_CDT[j], unit = "minute"))] <- NA
    }
    screen_timeseries_df <- na.omit(screen_timeseries_df)
    
    # change on/off to 1 for the minutes between an unlock and a lock event
    # note: make sure that there is at least 30 seconds of continuous use (example where this is relevant: phone on at 10:30:28 is rounded to 10:30:00 and phone off at 10:30:32 is rounded to 10:31:00; the actual phone on time is 4 seconds, but because of the rounding, it would be counted as a minute of phone use if we don't check the actual time difference)
    if (actual_diff >= 30) {
      screen_timeseries_df$is_on[which(screen_timeseries_df$date_time_CDT >= screen_df$rounded_date[i] & screen_timeseries_df$date_time_CDT < screen_df$rounded_date[j])] <- 1
    }
  }

  # get the participant number (P##) from the path name (note: change this to match your file name structure)
  # note: participant IDs are either P## or P##.2 in our dataset
  if (str_sub(p_folder, -5, -5) == "P"){
    p_str <- str_sub(p_folder, -5, -1)
  }
  else {
    p_str <- str_sub(p_folder, -3, -1)
  }

  ## add phone-on and phone-off event durations to screen_timeseries_df (the event duration is the number of consecutive minutes that the phone has been on or off)
  ## note: each row/minute will have an event_duration value which indicates the length of the event that minute is in (example: if the screen is turned on at 11:03:05 and off at 11:04:55, then 11:03 and 11:04, will have an event_duration value of 2 minutes; and this will count as one event)
  
  # get the indeces of rows in screen_timeseries_df where the following row is not the next minute and therefore event duration won't be correct
  timeseries_gaps <- diff(as.numeric(screen_timeseries_df$date_time_CDT))
  # change file_gap value to "yes" when there is a file gap (note: 60 refers to 60 seconds)
  screen_timeseries_df$file_gap[which(timeseries_gaps > 60)] <- "yes"  
  
  # event_count keeps track of how many events each participant has
  event_count <- 1
  # pointer is used in for loop to determine event durations (this is needed because an event consists of consecutive minutes, but we might have gaps in timestamps due to missing files or any of the edge cases accounted for above)
  pointer <- 1
  
  # loop through each row of screen_timeseries_df
  for (row_num in 1:nrow(screen_timeseries_df)) {
    # if you're on the last row, then you can't check what the "next" row's is_on value is; so we can get the event count and duration and update the dataframe
    if (row_num == nrow(screen_timeseries_df)) { 
      curr_event_dur <- row_num - pointer + 1
      screen_timeseries_df$event_dur[pointer:row_num] <- curr_event_dur
      screen_timeseries_df$event_count[pointer:row_num] <- event_count
    }
    # if the next row has the same is_on value and there is no file gap
    else if (screen_timeseries_df$is_on[row_num] == screen_timeseries_df$is_on[row_num + 1] & 
             screen_timeseries_df$file_gap[row_num] == "no") {
      # then move on to the next row
      next
    }
    else {
      # if we're not on the last row of the dataframe AND the next row doesn't have the same is_on value OR there is a file gap, then the current event we're tracking is over and we can get the duration and count values to update the dataframe
      curr_event_dur <- row_num - pointer + 1
      screen_timeseries_df$event_dur[pointer:row_num] <- curr_event_dur
      screen_timeseries_df$event_count[pointer:row_num] <- event_count
      # set the pointer to the next row to track the duration of the next event and update the event count
      pointer <- row_num + 1
      event_count <- event_count + 1
    }
  }
  
  ## output the participant's beiwe timeseries (note: update to match your file structure)
  write.csv(screen_timeseries_df, paste(beiwe_output_path, "/Beiwe_binned_infants/", p_str, ".csv", sep = ""), row.names = FALSE)
}



######################################################################################
####  P A R T 2 : C O M B I N E  B E I W E,  L E N A,  A N D  S L E E P  D A T A  ####
######################################################################################

# Note: Part 2 uses binned outputs from Part 1. These outputs are also available in the Data_Binned folder
# Outputs include combined timeseries of Beiwe and LENA (infant sleep removed) for 1) each individual participant (one file per participant), 2) all participants (one file), and 3) all participants during daytime hours 7am-7pm (one file- used in analysis)


# FUNCTION TO REMOVE THE FIRST 5 MINUTES OF EACH LENA RECORDING SO THAT THERE'S A BUFFER THAT ACCOUNTS FOR THE TIME IT TAKES TO GET THE DEVICE IN THE BABY'S VEST AND ONTO THE BABY

LENA_remove_first5 <- function(lena_path) {
  # read in lena file
  curr_lena_df <- read.csv(lena_path)
  # only keep rows when lena was recording
  curr_lena_df <- curr_lena_df[curr_lena_df$recOnSeconds != 0, ]
  # remove first 5 minutes of recording as a buffer for potentially inflated speech while LENA is being turned on with the synch procedure and participant might be talking to a lab member
  curr_lena_df <- tail(curr_lena_df, -5)
  return(curr_lena_df)
}


## FUNCTION TO COMBINE BEIWE, LENA, AND SLEEP DATA INTO A TIME SERIES FOR EACH PARTICIPANT
## note: beiwe_path = folder path containing each participant's beiwe timeseries files (output in the section above)
##       lena_path = folder path containing each participant's lena timeseries files (output from ITStobin script)
##       sleep_path = folder path containing each participant's sleep episode files (output from Python Sadeh scripts) 
##       sleep_path_longit = same as sleep_path but naming format is different
##       output_path = folder path where the merged beiwe, lena, and sleep timeseries will output (one file per participant)
##       P_list = list of each participant's ID

beiwe_lena_sleep <- function(beiwe_path, lena_path, sleep_path, sleep_path_longit, output_path, P_list) {

  # loop through each participant
  for (P in P_list) {

    print(P)
    
    ## LENA ##

    # lena (update folder name where lena binned csvs are based on the bin size)
    lena_files <- list.files(paste(lena_path, "/", P, "/1mins_midnight_CRTRUE_seq_alldata/", sep = ""), full.names = TRUE)
    # read in all of the lena csvs for the participant and concatenate them into one dataframe
    lena_df <- rbindlist(lapply(lena_files, LENA_remove_first5))
    # convert all datetimes to POSIXct format and to CDT
    lena_df$date_time_UTC <- as.POSIXct(strptime(lena_df$dateTimeStart_UTC, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    lena_df$date_time_CDT <- as.POSIXct(format(lena_df$date_time_UTC, tz="America/Chicago", usetz=TRUE))
    lena_df$date_time_CDT <- as.POSIXct(format(lena_df$date_time_CDT), tz = "America/Chicago")
    # change last argument to recOn if using second bins (rather than minute bins)
    lena_df <- select(lena_df, subjID, recClockStart, recClockEnd, date_time_CDT, adultWordCnt, femaleAdultWordCnt, maleAdultWordCnt, childUttCnt, childUttLenScnds, childUttScnds, childCryScnds, childVfxScnds, convTurnCount, OLN, OLF, recOnSeconds)

    ## BEIWE ##

    # read in beiwe timeseries in one minute bins
    beiwe_df_min <- read.csv(paste(beiwe_path, "/", P, ".csv", sep = ""))
    # convert dates to POSIXct datetimes
    beiwe_df_min$date_time_CDT <- as.POSIXct(strptime(beiwe_df_min$date_time_CDT, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
    # select columns to keep
    beiwe_df <- select(beiwe_df_min, date_time_CDT, is_on, event_dur, event_count, file_gap)
    
    ## MERGE LENA & BEIWE ##

    merged_lena_beiwe <- inner_join(lena_df, beiwe_df, by = "date_time_CDT")
    merged_lena_beiwe$date_time_CDT <- as.POSIXct(merged_lena_beiwe$date_time_CDT)

    ## SLEEP ##

    ## FUNCTION TO ADD SLEEP COLUMN TO THE MERGED DATAFRAME OF BEIWE AND LENA DATA
    ## note: this function is specific to the output of our sleep scripts
    add_sleep_col <- function(min_start_end, beiwe_lena_df) {
      bracket_split <- strsplit(min_start_end, "],")
      bracket_vec <- unlist(bracket_split) # change list to vector for easier indexing
      for (i in bracket_vec) {
        comma_split <- str_split(i, ",")
        comma_split_vec <- unlist(comma_split)
        sleep_dur <- substr(comma_split_vec[1], 2, nchar(comma_split_vec))
        sleep_start <- substr(comma_split_vec[2], 3, nchar(comma_split_vec[2])-1)
        # the last element ends with a "']" instead of just "'", so the last character for sleep_end needs to be the second to
        # last character instead of the first to last which is the case for every other element in the split vector
        if (i == length(bracket_vec)) {
          sleep_end <- substr(comma_split_vec[3], 3, nchar(comma_split_vec[3])-2)
        }
        else {
          sleep_end <- substr(comma_split_vec[3], 3, nchar(comma_split_vec[3])-1)
        }
        sleep_start_CDT <- as.POSIXct(strptime(sleep_start, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
        sleep_end_CDT <- as.POSIXct(strptime(sleep_end, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
        
        # round sleep start and end to the nearest minute
        sleep_start_CDT <- round_date(sleep_start_CDT, unit = "1 minute")
        sleep_end_CDT <- round_date(sleep_end_CDT, unit = "1 minute")

        beiwe_lena_df[((beiwe_lena_df$date_time_CDT >= sleep_start_CDT) & (beiwe_lena_df$date_time_CDT <= sleep_end_CDT)), "baby_sleep"] <- 1
      }
      return(beiwe_lena_df)
    }

    # create column of all zeros for baby sleep
    merged_lena_beiwe <- cbind(merged_lena_beiwe, baby_sleep = 0)

    # if participant is not longitudinal, then its format is P## (as opposed to P##.2) and it is located in the regular sleep_path folder
    if (nchar(P) == 3) {
      sleep_baby_df <- read.csv(paste(sleep_path, "/", P, "_output_baby.csv", sep= ""))
    }

    else {
      sleep_baby_df <- read.csv(paste(sleep_path_longit, "/", P, "_output_baby.csv", sep= ""))
    }

    colnames(sleep_baby_df) <- c("date_time_start_CDT", "date_time_end_CDT", "beepboop", "sleep_episodes")
    sleep_baby_df <- select(sleep_baby_df, date_time_start_CDT, date_time_end_CDT, sleep_episodes)

    # get rid of the outer brackets of the string
    sleep_baby_df$sleep_episodes <- substr(sleep_baby_df$sleep_episodes,2,nchar(sleep_baby_df$sleep_episodes)-1)

    # run add_sleep_col function to change baby sleep values to 1 for minute bins when they are sleeping
    for (ii in 1:length(sleep_baby_df$sleep_episodes)) {
      if (is.na(sleep_baby_df$sleep_episodes[ii]) == TRUE) {
        next
      }
      if (sleep_baby_df$sleep_episodes[ii] != "") {
        merged_lena_beiwe <- add_sleep_col(sleep_baby_df$sleep_episodes[ii], merged_lena_beiwe)
      }
    }

    # check if child utterance count is > 0 during a detected sleep episode, and change the value of baby_sleep to 0 if it is
    # (note: this logic is assuming that 1. Sadeh may be over-predicting sleep or the axivity device was not being worn and this was counted as sleep. 2. LENA's child utterance count detection is accurate enough to use as a validity check)
    merged_lena_beiwe$baby_sleep[which((merged_lena_beiwe$baby_sleep == 1) & (merged_lena_beiwe$childUttCnt > 0))] <- 0
    
    # keep minutes when baby is awake
    merged_lena_beiwe_sleep <- merged_lena_beiwe[merged_lena_beiwe$baby_sleep == 0,]

    # export merged timeseries to csv for the participant
    write.csv(merged_lena_beiwe_sleep, paste(output_path, "/", P,".csv", sep = ""), row.names = FALSE)
  }
}


####  MAIN  ####


# select path to binned beiwe and lena data for all participants
beiwe_path <- "../Data_Binned/Beiwe_binned_infants"
lena_path <- "../Data_Binned/LENA_binned_infants"

# select paths to sleep episode data
sleep_path <- "../Data_Raw/Sleep_movi_infants/SleepEpisodeDay"
sleep_path_longit <- "../Data_Raw/Sleep_movi/SleepEpisodeDay_Longit"

# select output path
output_path <- "../Data_Ready_for_Analysis"

# make list of pids with binned lena data (use a substring from the lena folder names)
P_folders <- list.dirs(lena_path, recursive = FALSE)
P_list <- list()

for (P_folder in P_folders) {
  if (str_sub(P_folder, -3, -3) == "P") {
    P_str <- str_sub(P_folder, -3, -1)
    P_list <- c(P_list, P_str)
  }
  else if (str_sub(P_folder, -5, -5) == "P") {
    P_str <- str_sub(P_folder, -5, -1)
    P_list <- c(P_list, P_str)
  }
}


## RUN beiwe_lena_sleep FOR ALL PARTICIPANTS
beiwe_lena_sleep(beiwe_path, lena_path, sleep_path, sleep_path_longit, output_path, P_list)



## FUNCTION TO ADD A COLUMN WITH THE PARTICIPANT'S ID TO THEIR TIMESERIES DATAFRAME

read_add_P <- function(file_name) {
  df <- read_csv(paste0(output_path, "/", file_name))
  P_str <- str_sub(file_name, 1, 3)
  df$P <- rep(P_str, nrow(df))
  return(df)
}

## COMBINE ALL PARTICIPANTS' BINNED CSVS INTO ONE FILE AND ADD A COLUMN WITH THE PARTICIPANT NUMBER
## note: if all_df.csv exists in this directory already, need to delete it before running the following lines
output_files <- list.files(output_path)
all_df <- rbindlist(lapply(output_files, read_add_P))

# output compiled dataframe to csv (this dataframe contains all hours of the days)
write.csv(all_df, paste0(output_path, "/all_P.csv"), row.names = FALSE)


### CREATE A DATAFRAME WITH ONLY DAYTIME HOURS (7AM-7PM) AND MERGE WITH DEMOGRAPHICS FILE (this is what we will use for analyses)

all_df_7_19 <- all_df[hour(all_df$date_time_CDT) >= 7 & hour(all_df$date_time_CDT) < 19,]

# add column with label for the hour interval of the datetime
all_df_7_19$hour <- case_when(
  hour(all_df_7_19$date_time_CDT) >= 7 & hour(all_df_7_19$date_time_CDT) < 8 ~ "07_08",
  hour(all_df_7_19$date_time_CDT) >= 8 & hour(all_df_7_19$date_time_CDT) < 9 ~ "08_09",
  hour(all_df_7_19$date_time_CDT) >= 9 & hour(all_df_7_19$date_time_CDT) < 10 ~ "09_10",
  hour(all_df_7_19$date_time_CDT) >= 10 & hour(all_df_7_19$date_time_CDT) < 11 ~ "10_11",
  hour(all_df_7_19$date_time_CDT) >= 11 & hour(all_df_7_19$date_time_CDT) < 12 ~ "11_12",
  hour(all_df_7_19$date_time_CDT) >= 12 & hour(all_df_7_19$date_time_CDT) < 13 ~ "12_13",
  hour(all_df_7_19$date_time_CDT) >= 13 & hour(all_df_7_19$date_time_CDT) < 14 ~ "13_14",
  hour(all_df_7_19$date_time_CDT) >= 14 & hour(all_df_7_19$date_time_CDT) < 15 ~ "14_15",
  hour(all_df_7_19$date_time_CDT) >= 15 & hour(all_df_7_19$date_time_CDT) < 16 ~ "15_16",
  hour(all_df_7_19$date_time_CDT) >= 16 & hour(all_df_7_19$date_time_CDT) < 17 ~ "16_17",
  hour(all_df_7_19$date_time_CDT) >= 17 & hour(all_df_7_19$date_time_CDT) < 18 ~ "17_18",
  hour(all_df_7_19$date_time_CDT) >= 18 & hour(all_df_7_19$date_time_CDT) < 19 ~ "18_19"
)

# output file with timeseries data for all participants during daytime hours
write.csv(all_df_7_19, paste0(output_path, "/all_P_7_19.csv"), row.names = FALSE)

# read in demographics file
dem <- read_csv("../Data_Raw/Demographics.csv")
dem <- select(dem, "P", "Infant_Age_mos", "Baby Gender", "Caregiver Age", "Caregiver Race/Ethnicity", "Family Annual Income", "Education", "Other_Children_In_Home", "Phone")

# merge timeseries output with demographics
all_df_7_19_dem <- merge(all_df_7_19, dem, by = "P")

# output file with timeseries data for all participants during daytime hours with demographics
# this file will be used for all further analyses
write.csv(all_df_7_19_dem, paste0(output_path, "/all_P_day_dem.csv"), row.names = FALSE)

