




distinct_tracks <- overall_tracks[!(duplicated(overall_tracks$scrobbles) | duplicated(overall_tracks$scrobbles, fromLast = TRUE)), ]


track_play_count <- overall_tracks %>% 
  group_by(scrobbles) %>% 
  summarize(count = n())



stuff <- stat_friendly_top_artist_ll[order(desc(stat_friendly_top_artist_ll$listening_level)),]


stuff <- stuff[!duplicated(stuff[,c('date')]),]

stuff <- stuff %>% 
  filter(artist != "Lexie Liu")



track_stuff <- stat_friendly_top_track_ll[order(desc(stat_friendly_top_track_ll$listening_level)),]

track_stuff <- track_stuff[!duplicated(track_stuff[,c('date')]),]

track_top_ll <- track_stuff %>% 
  group_by(track) %>% 
  summarize(count = n(),
            max = max(listening_level))


album_stuff <- stat_friendly_top_album_ll[order(desc(stat_friendly_top_album_ll$listening_level)),]

album_stuff <- album_stuff[!duplicated(album_stuff[,c('date')]),]

album_top_ll <- album_stuff %>% 
  group_by(album) %>% 
  summarize(count = n(),
            max = max(listening_level))




