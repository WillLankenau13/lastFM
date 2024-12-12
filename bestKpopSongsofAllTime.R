d_nominations <- read_csv("~/R Stuff/lastFM/Nominations.csv") 
last_ranking <- read_csv("~/R Stuff/lastFM/lastranking.csv") 

# d_nominations <- overall_tracks %>% 
#   filter(scrobbles > 10) %>% 
#   select(track, artist) %>% 
#   filter(artist != "Lexie Liu")

d_nominations[[2]] <- str_to_title(d_nominations[[2]])
last_ranking[[2]] <- str_to_title(last_ranking[[2]])
d_nominations$track[d_nominations$track == "Girls’ Capitalism"] <- "Girls' Capitalism"

first_oneeighty_tracks <- oneeighty_days_tracks %>% 
  filter(start_date == "2022-01-01") %>% 
  rename("scrobbles_first" = "scrobbles")

max_oneeighty_days_tracks_per <- oneeighty_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_180" = "rate")

max_threesixfive_days_tracks_per <- threesixfive_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_365" = "rate")

nominations <- d_nominations %>% 
  left_join(overall_tracks, by = c("artist", "track")) %>% 
  left_join(track_ll_stats, by = c("artist", "track")) %>% 
  left_join(max_oneeighty_days_tracks_per, by = c("artist", "track")) %>% 
  left_join(max_threesixfive_days_tracks_per, by = c("artist", "track")) %>% 
  left_join(first_oneeighty_tracks, by = c("artist", "track")) %>% 
  left_join(last_ranking, by = c("artist", "track"))

nominations <- nominations %>% 
  mutate(lr_score_mid = ifelse(is.na(number), 51, exp((51-number)/10) + 50))

nominations$scrobbles_first[is.na(nominations$scrobbles_first)] <- 0

max_scrobbles = max(nominations$scrobbles)
max_first_scrobbles = max(nominations$scrobbles_first)
max_ll = max(nominations$Max)
max_180 = max(nominations$rate_180)
max_365 = max(nominations$rate_365)
max_lr = max(nominations$lr_score_mid)

nominations <- nominations %>% 
  mutate(scrobble_score = sqrt(scrobbles/max_scrobbles),
         first_score = sqrt(scrobbles_first/max_first_scrobbles),
         ll_score = sqrt(Max/max_ll),
         per180_score = sqrt(rate_180/max_180),
         per365_score = sqrt(rate_365/max_365),
         lr_score = lr_score_mid/max_lr) %>% 
  mutate(overall_score = 0.5*scrobble_score + 0.1*first_score + 0.1*ll_score + 0.1*per180_score + 0.1*per365_score + 0.1*lr_score) %>% 
  select(track, artist, scrobble_score, first_score, ll_score, per180_score, per365_score, lr_score, overall_score)





