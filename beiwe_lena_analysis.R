library("TMB")
library("glmmTMB")
library("bbmle") 
library("ggplot2")
library("vioplot")
library(stats)
library(twosamples)
library(gridExtra)
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
library("dplyr")
library("vioplot")
library(interactions)
library(effects)
library(ggeffects)
library(mountainplot)
library(latticeExtra)
library(performance)
library(sjPlot)
library(lattice)

# read in timeseries dataframe containing merged lena and beiwe (baby sleep removed) data for all participants during daytime hours (7am-7pm)
# note: this is the final output from the preprocessing script

all_df_7_19 <- read_csv(paste("../Data_Ready_for_Analysis/all_P_day_dem.csv", sep = ""))
all_df_7_19$date_time_CDT <- as.POSIXct(format(all_df_7_19$date_time_CDT), tz = "America/Chicago")

# round word counts because the mixed model needs integer values
all_df_7_19$femaleAdultWordCnt <- round(all_df_7_19$femaleAdultWordCnt)
all_df_7_19$childUttCnt <- round(all_df_7_19$childUttCnt)
all_df_7_19$maleAdultWordCnt <- round(all_df_7_19$maleAdultWordCnt)

# change off event durations to 0
no_off_event_values_df <- 
  all_df_7_19 %>% mutate(event_dur = ifelse(is_on == 0, 0, event_dur))

# list of strings for hour intervals
time_intervals <- c("07:00-08:00", "08:00-09:00", "09:00-10:00", "10:00-11:00", "11:00-12:00", "12:00-13:00", "13:00-14:00",
                    "14:00-15:00", "15:00-16:00", "16:00-17:00", "17:00-18:00", "18:00-19:00")
time_intervals_abbr <- c("7_8", "8_9", "9_10", "10_11", "11_12", "12_13", "13_14", "14_15", "15_16", "16_17", "17_18", "18_19")
time_intervals_abbr2 <- c("7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19")

# make list of participant IDs using a substring from folder names
P_folders <- list.dirs("../Data_Binned/LENA_binned_infants", recursive = FALSE)
P_list <- list()

for (P_folder in P_folders) {
  if (str_sub(P_folder, -3, -3) == "P") {
    P_str <- str_sub(P_folder, -3, -1)
    P_list <- c(P_list, P_str)
  }
  else if (str_sub(P_folder, -5, -5) == "P") {
    P_str <- str_sub(P_folder, -5, -3)
    P_list <- c(P_list, P_str)
  }
}

# dataframe containing only phone-on minutes
on_df <- all_df_7_19[all_df_7_19$is_on == 1,]

# event duration percentile values for splitting data for short, medium, long event models
# note: event_dur is the same for each row with the same event_count and we just want the event_dur of each individual phone on event
phone_event_durs <- on_df %>%
  group_by(P,event_count) %>%
  dplyr::summarise(Event_Duration = max(event_dur))

quantile(phone_event_durs$Event_Duration, probs = c(1/3, 2/3, 1))
# 33.33333%   66.66667%      100% 
#     2           6           91



#########################
#### A N A L Y S I S ####
#########################

###  ZERO-INFLATED NEGATIVE BINOMIAL GENERALIZED LINEAR MIXED MODELS  ###

## OVERALL MODEL
fit_zinbinom_overall <- glmmTMB(femaleAdultWordCnt ~ is_on + hour + maleAdultWordCnt + Infant_Age_mos + Education + Other_Children_In_Home + childUttCnt + (1|P),
                         data=all_df_7_19,
                         ziformula=~.,
                         family=nbinom2())

summary(fit_zinbinom_overall)



## TIME OF DAY INTERACTION MODEL
fit_zinbinom_inter <- glmmTMB(femaleAdultWordCnt ~ is_on*hour + maleAdultWordCnt + Infant_Age_mos + Education + Other_Children_In_Home + childUttCnt + (1|P),
                         data=all_df_7_19,
                         ziformula=~.,
                         family=nbinom2())

summary(fit_zinbinom_inter)


# Direction of interaction effects for TOD model
library(emmeans)

int_effects <- emmeans(fit_zinbinom_inter, ~ is_on*hour)


##### Event analyses based on percentile values

# Create Dataframe with Short Phone-On Events (all "off-events")
# 1-2 minutes
day_1to2_df <- no_off_event_values_df[no_off_event_values_df$event_dur <= 2,]

# Create Dataframe with Medium Phone-On Events (all "off-events")
# 3-6 minutes
day_3to6_df <- no_off_event_values_df[(no_off_event_values_df$event_dur == 0) | (no_off_event_values_df$event_dur >= 3 & no_off_event_values_df$event_dur <= 6),]

# Create Dataframe with Long Phone-On Events (all "off-events")
# 7+ minutes
day_7plus_df <- no_off_event_values_df[(no_off_event_values_df$event_dur == 0) | no_off_event_values_df$event_dur >= 7,]


## SHORT EVENT MODEL
fit_zinbinom_1to2 <- glmmTMB(femaleAdultWordCnt ~ is_on + hour + maleAdultWordCnt + Infant_Age_mos + Education + Other_Children_In_Home + childUttCnt + (1|P),
                             data=day_1to2_df,
                             ziformula=~.,
                             family=nbinom2())

summary(fit_zinbinom_1to2)

## MEDIUM EVENT MODEL
fit_zinbinom_3to6 <- glmmTMB(femaleAdultWordCnt ~ is_on + hour + maleAdultWordCnt + Infant_Age_mos + Education + Other_Children_In_Home + childUttCnt + (1|P),
                              data=day_3to6_df,
                              ziformula=~.,
                              family=nbinom2())

summary(fit_zinbinom_3to6)

## LONG EVENT MODEL
fit_zinbinom_7plus <- glmmTMB(femaleAdultWordCnt ~ is_on + hour + maleAdultWordCnt + Infant_Age_mos + Education + Other_Children_In_Home + childUttCnt  + (1|P),
                               data=day_7plus_df,
                               ziformula=~.,
                               family=nbinom2())

summary(fit_zinbinom_7plus)
