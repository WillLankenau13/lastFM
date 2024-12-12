library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")

test <- one_day_tracks %>% 
  filter(date == "2024-05-12")

distinct_last_seven_artists <- seven_days_artists %>% 
  distinct(artist, .keep_all = TRUE)
distinct_last_seven_tracks <- seven_days_tracks %>% 
  distinct(track, .keep_all = TRUE)


distinct_last_thirty_artists <- thirty_days_artists %>% 
  distinct(artist, .keep_all = TRUE)

distinct_last_thirty_tracks <- thirty_days_tracks %>% 
  distinct(track, .keep_all = TRUE)

distinct_last_ninety_tracks <- ninety_days_tracks %>% 
  distinct(track, .keep_all = TRUE)

distinct_last_ninety_artists <- ninety_days_artists %>% 
  distinct(artist, .keep_all = TRUE)

distinct_last_oneeighty_artists <- oneeighty_days_artists %>% 
  distinct(artist, .keep_all = TRUE)

