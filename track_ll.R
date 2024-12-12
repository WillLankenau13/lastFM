library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")


ll_one_day_tracks <- one_day_tracks %>% 
  select(track, artist, date, scrobbles) %>% 
  arrange(date) %>% 
  pivot_wider(names_from = date, values_from = scrobbles)
ll_one_day_tracks[is.na(ll_one_day_tracks)] <- 0


ll_seven_days_tracks <- seven_days_tracks %>% 
  select(track, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_seven_days_tracks[is.na(ll_seven_days_tracks)] <- 0
ll_seven_days_tracks <- ll_seven_days_tracks %>% 
  arrange(track)%>% 
  filter(track != "placeholder")


ll_thirty_days_tracks <- thirty_days_tracks %>% 
  select(track, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_thirty_days_tracks[is.na(ll_thirty_days_tracks)] <- 0
ll_thirty_days_tracks <- ll_thirty_days_tracks %>% 
  arrange(track)%>% 
  filter(track != "placeholder")


ll_ninety_days_tracks <- ninety_days_tracks %>% 
  select(track, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_ninety_days_tracks[is.na(ll_ninety_days_tracks)] <- 0
ll_ninety_days_tracks <- ll_ninety_days_tracks %>% 
  arrange(track)%>% 
  filter(track != "placeholder")


cumulative_scrobbles_by_track <- ll_one_day_tracks
cumulative_scrobbles_by_track <- cumulative_scrobbles_by_track %>% 
  filter(track != "placeholder") %>% 
  arrange(track)
track_ll <- cumulative_scrobbles_by_track


'# shizzam <- function(y){
#     mutate(cumulative_scrobbles_by_track, {{y}} =  3)
# }'

# cumulative_scrobbles_by_track[is.na(cumulative_scrobbles_by_track)] <- 0
# ll_ninety_days_tracks[is.na(ll_ninety_days_tracks)] <- 0
# ll_one_day_tracks[is.na(ll_one_day_tracks)] <- 0
# ll_seven_days_tracks[is.na(ll_seven_days_tracks)] <- 0
# ll_thirty_days_tracks[is.na(ll_thirty_days_tracks)] <- 0


for(i in 4:(ncol(cumulative_scrobbles_by_track))){
  cumulative_scrobbles_by_track[i] <- rowSums(cumulative_scrobbles_by_track[(i-1):i], na.rm=TRUE)
  cumulative_scrobbles_by_track[cumulative_scrobbles_by_track == 0] <- NA
}


track_ll_helper <- full_join(cumulative_scrobbles_by_track, ll_seven_days_tracks, by = "track") %>% 
  full_join(ll_thirty_days_tracks, by = "track") %>% 
  full_join(ll_ninety_days_tracks, by = "track")

weight_seven <- 0.17
weight_thirty <- 0.45
weight_ninety <- 0.35
weight_overall <- 0.03

adjuster_seven <- weight_seven*12
adjuster_thirty <- weight_thirty*3
adjuster_ninety <- weight_ninety
#adjuster_overall <- 

cumulative_scrobbles_by_track <- cumulative_scrobbles_by_track %>% 
  arrange(track)
track_ll <- track_ll %>% 
  arrange(track)


i <- 9
for(i in 3:8){
  track_ll[i] <- cumulative_scrobbles_by_track[i]*adjuster_seven + cumulative_scrobbles_by_track[i]*adjuster_thirty + cumulative_scrobbles_by_track[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_track[i])*weight_overall
}

for(i in 9:31){
  track_ll[i] <- ll_seven_days_tracks[i-6]*adjuster_seven + cumulative_scrobbles_by_track[i]*adjuster_thirty + cumulative_scrobbles_by_track[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_track[i])*weight_overall
}

for(i in 32:91){
  track_ll[i] <- ll_seven_days_tracks[i-6]*adjuster_seven + ll_thirty_days_tracks[i-29]*adjuster_thirty + cumulative_scrobbles_by_track[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_track[i])*weight_overall
}

for(i in 92:(ncol(track_ll))){
  track_ll[i] <- ll_seven_days_tracks[i-6]*adjuster_seven + ll_thirty_days_tracks[i-29]*adjuster_thirty + ll_ninety_days_tracks[i-89]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_track[i])*weight_overall
}

track_ll_stats <- track_ll %>% 
  pivot_longer(!c("track", "artist"), names_to = "date", values_to = "listening_level") %>% 
  group_by(track, artist) %>% 
  summarise(Max = max(listening_level, na.rm = TRUE),
            Q_3 = quantile(listening_level, na.rm = TRUE, 0.75), 
            Mean = mean(listening_level, na.rm = TRUE),
            Median = median(listening_level, na.rm = TRUE),
            Q_1 = quantile(listening_level, na.rm = TRUE, 0.25))

track_ll[3:(ncol(track_ll))] <- round(track_ll[3:(ncol(track_ll))], 2)

last_7_track_ll <- track_ll %>% 
  select(track, artist, (ncol(track_ll)-6):ncol(track_ll)) 

last_7_track_ll <- arrange(last_7_track_ll, desc(last_7_track_ll[8]))

