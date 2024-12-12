
hearted <- read_csv("~/R Stuff/lastFM/hearted.csv") %>% 
  select(artist, track)

hearted$artist <- str_to_title(hearted$artist)

f <- function(df, count){
  for(i in 1:count){
    temp <- df %>% 
      distinct(end_date, .keep_all = TRUE)
    df <- anti_join(df, temp, by = c("start_date", "end_date", "track", "artist", "scrobbles"))
  }
  return(temp)
}

return_hearted <- function(df, n){
  cutoffs <- f(df, n) %>% 
    select(end_date, scrobbles) %>% 
    rename("min_scrobbles" = "scrobbles")
  
  joined <- full_join(df, cutoffs, by = c("end_date")) %>% 
    filter(scrobbles >= min_scrobbles)
  
  return(joined)
}

test2 <- return_hearted(threesixfive_days_tracks, 15) %>% 
  mutate(days = 90) %>% 
  group_by(artist, track) %>% 
  summarize(count = n())

thirty_days_top1 <- return_hearted(thirty_days_tracks, 1) %>% 
  group_by(track, artist) %>% 
  summarize(days_30 = n()) %>% 
  mutate(t30 = 1)
ninety_days_top3 <- return_hearted(ninety_days_tracks, 3) %>% 
  group_by(track, artist) %>% 
  summarize(days_90 = n()) %>% 
  mutate(t90 = 1)
oneeighty_days_top5 <- return_hearted(oneeighty_days_tracks, 8) %>% 
  group_by(track, artist) %>% 
  summarize(days_180 = n()) %>% 
  mutate(t180 = 1)
threesixfive_days_top10 <- return_hearted(threesixfive_days_tracks, 15) %>% 
  group_by(track, artist) %>% 
  summarize(days_365 = n()) %>% 
  mutate(t365 = 1)

thirty_days_top3 <- return_hearted(thirty_days_tracks, 3) %>% 
  mutate(days = 90) %>% 
  group_by(track, artist) %>% 
  summarize(days1 = n()) %>% 
  mutate(c1 = 1) 
ninety_days_top5 <- return_hearted(ninety_days_tracks, 5) %>% 
  mutate(days = 90) %>% 
  group_by(track, artist) %>% 
  summarize(days2 = n()) %>% 
  mutate(c2 = 1) 
oneeighty_days_top12 <- return_hearted(oneeighty_days_tracks, 12) %>% 
  mutate(days = 180) %>% 
  group_by(track, artist) %>% 
  summarize(days3 = n()) %>% 
  mutate(c3 = 1) 
threesixfive_days_top20 <- return_hearted(threesixfive_days_tracks, 20) %>% 
  mutate(days = 365) %>% 
  group_by(track, artist) %>% 
  summarize(days4 = n()) %>% 
  mutate(c4 = 1) 
seven_days_top1 <- return_hearted(seven_days_tracks, 1) %>% 
  mutate(days = 7) %>% 
  group_by(track, artist) %>% 
  summarize(days5 = n()) %>% 
  mutate(c5 = 1) %>% 
  filter(days5 > 9) 

secondtier <- thirty_days_top3 %>% 
  full_join(ninety_days_top5, by = c("track", "artist")) %>% 
  full_join(oneeighty_days_top12, by = c("track", "artist")) %>% 
  full_join(threesixfive_days_top20, by = c("track", "artist")) %>% 
  full_join(seven_days_top1, by = c("track", "artist"))
secondtier[is.na(secondtier)] <- 0
secondtier <- secondtier %>% 
  mutate(sumc = c1 + c2 + c3 + c4 + c5) %>% 
  filter(sumc >= 3) %>% 
  select(track, artist) %>% 
  mutate(st = 1)

min_scrobbles <- overall_tracks %>% 
  filter(scrobbles >= 300) %>% 
  select(artist, track) %>% 
  mutate(ot = 1)

max_thirty_days_tracks_per <- thirty_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_30" = "rate")

max_ninety_days_tracks_per <- ninety_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_90" = "rate") 

max_oneeighty_days_tracks_per <- oneeighty_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_180" = "rate") 

max_threesixfive_days_tracks_per <- threesixfive_days_tracks_per %>%
  arrange(desc(rate)) %>% 
  distinct(track, artist, .keep_all = TRUE) %>%
  rename("rate_365" = "rate") 

percents <- overall_tracks %>% 
  left_join(max_thirty_days_tracks_per, by = c("track", "artist")) %>% 
  left_join(max_ninety_days_tracks_per, by = c("track", "artist")) %>% 
  left_join(max_oneeighty_days_tracks_per, by = c("track", "artist")) %>% 
  left_join(max_threesixfive_days_tracks_per, by = c("track", "artist")) %>% 
  select(track, artist, rate_30, rate_90, rate_180, rate_365)

percents[is.na(percents)] <- 0
percents <- percents %>% 
  mutate(sumrates = rate_30 + rate_90 + rate_180 + rate_365) %>% 
  filter(sumrates >= 0.035) %>% 
  select(track, artist) %>% 
  mutate(p = 1)

top_track_ll <- track_ll_stats %>% 
  filter(Max > 140) %>% 
  select(artist, track) %>% 
  mutate(ll = 1)

############

to_heart <- thirty_days_top1 %>% 
  full_join(ninety_days_top3, by = c("track", "artist")) %>% 
  full_join(oneeighty_days_top5, by = c("track", "artist")) %>% 
  full_join(threesixfive_days_top10, by = c("track", "artist")) %>% 
  full_join(top_track_ll, by = c("track", "artist")) %>% 
  full_join(secondtier, by = c("track", "artist")) %>% 
  full_join(min_scrobbles, by = c("track", "artist")) %>% 
  full_join(percents, by = c("track", "artist")) 
to_heart[is.na(to_heart)] <- 0
to_heart <- to_heart %>% 
  mutate(total = t30 + t90 + t180 + t365 + ll + st + ot + p)

unheart  <- anti_join(hearted, to_heart, by = c("track", "artist")) 

# %>% 
#   filter(artist != "Lexie Liu") %>% 
#   filter(artist != "Dua Lipa")

to_heart <- anti_join(to_heart, hearted, by = c("track", "artist")) %>% 
  filter(artist != "Lexie Liu") %>% 
  filter(artist != "Cherry Bullet")

# %>%
#   filter(artist != "Cherry Bullet") %>%
#   filter(artist != "Lexie Liu") %>%
#   filter(artist != "Mirei") %>%
#   filter(artist != "Chymes") %>%
#   filter(track != "Love Lost") %>%
#   filter(track != "To the moon")

write_csv(to_heart, "~/R Stuff/lastFM/to_heart.csv")



