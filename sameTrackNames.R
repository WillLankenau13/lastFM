

#groups scrobbles by track name; i.e. combines different tracks with same names
distinct_tracks <- overall_tracks %>% 
  select(track, artist, scrobbles) %>% 
  mutate(track = tolower(track))

track_name_count <- distinct_tracks %>% 
  group_by(track) %>% 
  summarize(count = n(),
            sum_scrobbles = sum(scrobbles))

tracks_by_count <- distinct_tracks %>% 
  left_join(track_name_count, by = c("track")) %>% 
  arrange(track)
  

