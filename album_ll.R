library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")


ll_one_day_albums <- one_day_albums %>% 
  select(album, artist, date, scrobbles) %>% 
  arrange(date) %>% 
  pivot_wider(names_from = date, values_from = scrobbles)
ll_one_day_albums <- ll_one_day_albums[complete.cases(ll_one_day_albums[,c("album")]),]
ll_one_day_albums[is.na(ll_one_day_albums)] <- 0


ll_seven_days_albums <- seven_days_albums %>% 
  select(album, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_seven_days_albums <- ll_seven_days_albums[complete.cases(ll_seven_days_albums[,c("album")]),]
ll_seven_days_albums[is.na(ll_seven_days_albums)] <- 0
ll_seven_days_albums <- ll_seven_days_albums %>% 
  arrange(album)%>% 
  filter(album != "placeholder")


ll_thirty_days_albums <- thirty_days_albums %>% 
  select(album, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_thirty_days_albums <- ll_thirty_days_albums[complete.cases(ll_thirty_days_albums[,c("album")]),]
ll_thirty_days_albums[is.na(ll_thirty_days_albums)] <- 0
ll_thirty_days_albums <- ll_thirty_days_albums %>% 
  arrange(album)%>% 
  filter(album != "placeholder")


ll_ninety_days_albums <- ninety_days_albums %>% 
  select(album, artist, end_date, scrobbles) %>% 
  arrange(end_date) %>% 
  pivot_wider(names_from = end_date, values_from = scrobbles)
ll_ninety_days_albums <- ll_ninety_days_albums[complete.cases(ll_ninety_days_albums[,c("album")]),]
ll_ninety_days_albums[is.na(ll_ninety_days_albums)] <- 0
ll_ninety_days_albums <- ll_ninety_days_albums %>% 
  arrange(album)%>% 
  filter(album != "placeholder")


cumulative_scrobbles_by_album <- ll_one_day_albums
cumulative_scrobbles_by_album <- cumulative_scrobbles_by_album %>% 
  filter(album != "placeholder") %>% 
  arrange(album)
album_ll <- cumulative_scrobbles_by_album


'# shizzam <- function(y){
#     mutate(cumulative_scrobbles_by_album, {{y}} =  3)
# }'

# cumulative_scrobbles_by_album[is.na(cumulative_scrobbles_by_album)] <- 0
# ll_ninety_days_albums[is.na(ll_ninety_days_albums)] <- 0
# ll_one_day_albums[is.na(ll_one_day_albums)] <- 0
# ll_seven_days_albums[is.na(ll_seven_days_albums)] <- 0
# ll_thirty_days_albums[is.na(ll_thirty_days_albums)] <- 0


for(i in 4:(ncol(cumulative_scrobbles_by_album))){
  cumulative_scrobbles_by_album[i] <- rowSums(cumulative_scrobbles_by_album[(i-1):i], na.rm=TRUE)
  cumulative_scrobbles_by_album[cumulative_scrobbles_by_album == 0] <- NA
}


album_ll_helper <- full_join(cumulative_scrobbles_by_album, ll_seven_days_albums, by = "album") %>% 
  full_join(ll_thirty_days_albums, by = "album") %>% 
  full_join(ll_ninety_days_albums, by = "album")

weight_seven <- 0.17
weight_thirty <- 0.45
weight_ninety <- 0.35
weight_overall <- 0.03

adjuster_seven <- weight_seven*12
adjuster_thirty <- weight_thirty*3
adjuster_ninety <- weight_ninety
#adjuster_overall <- 

cumulative_scrobbles_by_album <- cumulative_scrobbles_by_album %>% 
  arrange(album)
album_ll <- album_ll %>% 
  arrange(album)


i <- 9
for(i in 3:8){
  album_ll[i] <- cumulative_scrobbles_by_album[i]*adjuster_seven + cumulative_scrobbles_by_album[i]*adjuster_thirty + cumulative_scrobbles_by_album[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_album[i])*weight_overall
}

for(i in 9:31){
  album_ll[i] <- ll_seven_days_albums[i-6]*adjuster_seven + cumulative_scrobbles_by_album[i]*adjuster_thirty + cumulative_scrobbles_by_album[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_album[i])*weight_overall
}

for(i in 32:91){
  album_ll[i] <- ll_seven_days_albums[i-6]*adjuster_seven + ll_thirty_days_albums[i-29]*adjuster_thirty + cumulative_scrobbles_by_album[i]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_album[i])*weight_overall
}

for(i in 92:(ncol(album_ll))){
  album_ll[i] <- ll_seven_days_albums[i-6]*adjuster_seven + ll_thirty_days_albums[i-29]*adjuster_thirty + ll_ninety_days_albums[i-89]*adjuster_ninety + (90/(i - 2))*(cumulative_scrobbles_by_album[i])*weight_overall
}

album_ll_stats <- album_ll %>% 
  pivot_longer(!c("album", "artist"), names_to = "date", values_to = "listening_level") %>% 
  group_by(album, artist) %>% 
  summarise(Max = max(listening_level, na.rm = TRUE),
            Q_3 = quantile(listening_level, na.rm = TRUE, 0.75), 
            Mean = mean(listening_level, na.rm = TRUE),
            Median = median(listening_level, na.rm = TRUE),
            Q_1 = quantile(listening_level, na.rm = TRUE, 0.25))

album_ll[3:(ncol(album_ll))] <- round(album_ll[3:(ncol(album_ll))], 2)

last_7_album_ll <- album_ll %>% 
  select(album, artist, (ncol(album_ll)-6):ncol(album_ll)) 

last_7_album_ll <- arrange(last_7_album_ll, desc(last_7_album_ll[8]))

