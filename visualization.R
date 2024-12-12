library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")

c_artist_ll <- artist_ll %>% 
  full_join(artist_ll_stats, by = "artist")

top_artist_ll <- c_artist_ll %>% 
  filter(Max > 1000 | Mean > 250)

stat_friendly_top_artist_ll <- top_artist_ll %>% 
  pivot_longer(!artist, names_to = "date", values_to = "listening_level") %>% 
  filter(!is.na(listening_level)) %>% 
  filter(date > ymd("2020-1-1")) %>% 
  mutate(date_number = as.Date(date) - ymd("2022-1-1"))

ggplot(data = stat_friendly_top_artist_ll, mapping = aes(x = date_number, y = listening_level)) +
  geom_point(mapping = aes(color = artist), position = "jitter") +
  geom_line(mapping = aes(color = artist))



# thirty_days_artists_stats <- ll_thirty_days_artists %>% 
#   pivot_longer(!artist, names_to = "end_date", values_to = "scrobbles") %>% 
#   group_by(artist) %>% 
#   summarise(Max = max(scrobbles, na.rm = TRUE),
#             Q_3 = quantile(scrobbles, na.rm = TRUE, 0.75), 
#             Mean = mean(scrobbles, na.rm = TRUE),
#             Median = median(scrobbles, na.rm = TRUE),
#             Q_1 = quantile(scrobbles, na.rm = TRUE, 0.25),
#             Min = min(scrobbles, na.rm = TRUE))
# 
# thirty_days_artists_top <- ll_thirty_days_artists %>% 
#   full_join(thirty_days_artists_stats, by = "artist") %>% 
#   filter(Max > 200) %>% 
#   pivot_longer(!artist, names_to = "date", values_to = "scrobbles") %>% 
#   filter(scrobbles != 0) %>% 
#   filter(date > ymd("2020-1-1")) %>% 
#   mutate(date_number = as.Date(date) - ymd("2022-1-1"))
# 
# ggplot(data = thirty_days_artists_top, mapping = aes(x = date_number, y = scrobbles)) +
#   geom_point(mapping = aes(color = artist), position = "jitter") +
#   geom_line(mapping = aes(color = artist))


c_track_ll <- track_ll %>% 
  full_join(track_ll_stats, by = c("track", "artist"))


top_track_ll <- c_track_ll %>% 
  filter(Max > 170)

stat_friendly_top_track_ll <- top_track_ll %>% 
  pivot_longer(!c("track", "artist"), names_to = "date", values_to = "listening_level") %>% 
  filter(!is.na(listening_level)) %>% 
  filter(date > ymd("2020-1-1")) %>% 
  mutate(date_number = as.Date(date) - ymd("2022-1-1"))

ggplot(data = stat_friendly_top_track_ll, mapping = aes(x = date_number, y = listening_level)) +
  geom_point(mapping = aes(color = track), position = "jitter") +
  geom_line(mapping = aes(color = track))


album_ll <- album_ll %>% 
  full_join(album_ll_stats, by = c("album", "artist"))

top_album_ll <- album_ll %>% 
  filter(Mean > 200)

stat_friendly_top_album_ll <- top_album_ll %>% 
  pivot_longer(!c("album", "artist"), names_to = "date", values_to = "listening_level") %>% 
  filter(!is.na(listening_level)) %>% 
  filter(date > ymd("2020-1-1")) %>% 
  mutate(date_number = as.Date(date) - ymd("2022-1-1"))

ggplot(data = stat_friendly_top_album_ll, mapping = aes(x = date_number, y = listening_level)) +
  geom_point(mapping = aes(color = album), position = "jitter") +
  geom_line(mapping = aes(color = album))



