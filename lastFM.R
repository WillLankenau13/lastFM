library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")

#fix album NA with Dingga and other
#make another 7 days scrobbles that's unique (not overlapping days), take the original 7 days and loop through it, putting each start and end date in a list and check to see if either start or end date are between any of the start and end dates of said list
#make it faster
#make sure no possible entire blank 7/30/90 days span that is left out of listening levels

#shizzam shizzam

scrobbles <- read_csv("~/R Stuff/lastFM/raw_data.csv") %>% 
  mutate(date_time = parse_datetime(utc_time, "%d %b %Y, %H:%M"))

scrobbles$track[scrobbles$track == "Girls’ Capitalism"] <- "Girls' Capitalism"

scrobbles$date_time <- with_tz(scrobbles$date_time, tzone = "America/New_York")
scrobbles <- mutate(scrobbles, date = as_date(date_time))

scrobbles <- select(scrobbles, date_time, date, artist, album, track)
scrobbles[[3]] <- str_to_title(scrobbles[[3]])
first_day <- min(scrobbles$date)

myfunc <- function(v1) {
  deparse(substitute(v1))
}

#### one day simple ####

one_day_f <- function(df, a, b, c){
  df %>% 
    group_by({{a}}, {{b}}, {{c}}) %>% 
    summarize(scrobbles = n()) %>% 
    arrange(date)
}

one_day_tracks <- one_day_f(scrobbles, track, artist, date)
one_day_artists <- one_day_f(scrobbles, artist, date)
one_day_albums <- one_day_f(scrobbles, album, artist, date)
one_day_scrobbles <- one_day_f(scrobbles, date)

x <- as.numeric(max(scrobbles$date) - first_day + 1)
i <- 1
while (i <= x) {
  ifelse(as.numeric(one_day_scrobbles[i, 1]) == one_day_scrobbles[1, 1] + i - 1, print(i), rbind(one_day_scrobbles[nrow(one_day_scrobbles) + 1,] <- list(as.Date(ymd("2022-1-1") + days(i) - 1), 0)))
  ifelse(as.numeric(one_day_scrobbles[i, 1]) == one_day_scrobbles[1, 1] + i - 1, print(""), rbind(one_day_artists[nrow(one_day_artists) + 1,] <- list("placeholder", as.Date(ymd("2022-1-1") + days(i) - 1), 0)))
  ifelse(as.numeric(one_day_scrobbles[i, 1]) == one_day_scrobbles[1, 1] + i - 1, print(""), rbind(one_day_tracks[nrow(one_day_tracks) + 1,] <- list("placeholder", "placeholder", as.Date(ymd("2022-1-1") + days(i) - 1), 0)))
  ifelse(as.numeric(one_day_scrobbles[i, 1]) == one_day_scrobbles[1, 1] + i - 1, print(""), rbind(one_day_albums[nrow(one_day_albums) + 1,] <- list("placeholder", "placeholder", as.Date(ymd("2022-1-1") + days(i) - 1), 0)))
  ifelse(as.numeric(one_day_scrobbles[i, 1]) == one_day_scrobbles[1, 1] + i - 1, print(""), i <- i - 1)
  i <- i + 1
   one_day_scrobbles <- one_day_scrobbles %>% 
     arrange(date)
  }

#### end ####


complete_f <- function(n, a, b, c, x){
  
  
  if({{x}} == 2){
    df <- select(one_day_artists, date, {{a}}, {{b}}) %>% 
      mutate(
        artist2 = artist
      )
  }else if({{x}} == 3){
    df <- select(one_day_tracks, date, {{a}}, {{b}}, {{c}})
  }else if({{x}} == 4){
    df <- select(one_day_albums, date, {{a}}, {{b}}, {{c}})
  }else{
    df <- select(one_day_scrobbles, date, {{a}}) %>% 
      mutate(
        scrobbles2 = scrobbles,
        scrobbles3 = scrobbles
      )
  }
  
  scrobbles_grouping <- function(df, n){
    for(i in 1:{{n}}){
      df[i+4] <- sapply(0, "+", as.numeric(df$date - first_day - i + 1)%/%{{n}} * {{n}} + i - 1)
      names(df)[i+4] <- paste("group_", {{i}}, sep = "")
    }
    return(df)
  }

  df <- scrobbles_grouping(df, {{n}})

  group <- list()
  groups_list <- colnames(df)

  group_and_summarize <- function(df, s, b, c){
    df <-  select(df, 1:4, {{s}} + 4) 
    colnames(df)[5] <- "group"
    df <- df %>% group_by(group, {{b}}, {{c}}) %>%
      summarize(c = sum(scrobbles),
                date = min(group)) %>% 
      mutate(
        date = date + first_day
      )
  }

  for(k in 1:{{n}}){
    group[[k]] <- group_and_summarize(df, k, {{b}}, {{c}})
  }

  df <- data.frame()

  for(a in 1:{{n}}){
    df <- bind_rows(df, group[[a]])
  }


  names(df)[1] <- 'Number'
  df  <-  df %>%
    filter(Number >=0) %>%
    mutate(start_date = date,
           end_date = date + {{n}} - 1,
           scrobbles = c) %>%
    select(start_date, end_date, {{b}}, {{c}}, scrobbles) %>%
    arrange(desc(scrobbles))

  if({{x}} == 1){
    df <- df[!duplicated(df$start_date),] %>%
      filter(max(start_date) >= end_date)
  }else{
    df <- df%>%
      filter(max(start_date) >= end_date)
  }

  df <- df[!duplicated(df), ]
  
  return(df)
  
}

seven_days_scrobbles <- complete_f(7, scrobbles, NULL, NULL, 1)
seven_days_artists <- complete_f(7, scrobbles, artist, NULL, 2)
seven_days_tracks <- complete_f(7, scrobbles, track, artist, 3)
seven_days_albums <- complete_f(7, scrobbles, album, artist, 4)

thirty_days_scrobbles <- complete_f(30, scrobbles, NULL, NULL, 1)
thirty_days_artists <- complete_f(30, scrobbles, artist, NULL, 2)
thirty_days_tracks <- complete_f(30, scrobbles, track, artist, 3)
thirty_days_albums <- complete_f(30, scrobbles, album, artist, 4)

ninety_days_scrobbles <- complete_f(90, scrobbles, NULL, NULL, 1)
ninety_days_artists <- complete_f(90, scrobbles, artist, NULL, 2)
ninety_days_tracks <- complete_f(90, scrobbles, track, artist, 3)
ninety_days_albums <- complete_f(90, scrobbles, album, artist, 4)

oneeighty_days_scrobbles <- complete_f(180, scrobbles, NULL, NULL, 1)
oneeighty_days_artists <- complete_f(180, scrobbles, artist, NULL, 2)
oneeighty_days_tracks <- complete_f(180, scrobbles, track, artist, 3)
oneeighty_days_albums <- complete_f(180, scrobbles, album, artist, 4)

threesixfive_days_scrobbles <- complete_f(365, scrobbles, NULL, NULL, 1)
threesixfive_days_artists <- complete_f(365, scrobbles, artist, NULL, 2)
threesixfive_days_tracks <- complete_f(365, scrobbles, track, artist, 3)
threesixfive_days_albums <- complete_f(365, scrobbles, album, artist, 4)


one_per_f <- function(df_specific, df_scrobbles, filter_1 = 0, filter_2 = 0){
  df <- full_join({{df_specific}}, {{df_scrobbles}}, by = "date") %>%
    mutate(rate = scrobbles.x/scrobbles.y) %>%
    na.omit()%>%
    arrange(desc(rate)) %>% 
    filter(scrobbles.x > {{filter_1}}) %>% 
    filter(scrobbles.y > {{filter_2}})
  
  names(df)[names(df) == 'scrobbles.x'] <- 'specific_scrobbles'
  names(df)[names(df) == 'scrobbles.y'] <- 'daily_scrobbles'
  
  return(df)
}

per_f <- function(df_specific, df_scrobbles, filter_1 = 0, filter_2 = 0){
  df <- full_join({{df_specific}}, {{df_scrobbles}}, by = c("start_date", "end_date")) %>%
    mutate(rate = scrobbles.x/scrobbles.y) %>%
    na.omit()%>%
    arrange(desc(rate)) %>% 
    filter(scrobbles.x > {{filter_1}}) %>% 
    filter(scrobbles.y > {{filter_2}})
  
  names(df)[names(df) == 'scrobbles.x'] <- 'specific_scrobbles'
  names(df)[names(df) == 'scrobbles.y'] <- 'total_scrobbles'
  
  return(df)
}

one_day_tracks_per <- one_per_f(one_day_tracks, one_day_scrobbles, 1, 50)
one_day_artists_per <- one_per_f(one_day_artists, one_day_scrobbles, 1, 50)
one_day_albums_per <- one_per_f(one_day_albums, one_day_scrobbles, 1, 50)

seven_days_tracks_per <- per_f(seven_days_tracks, seven_days_scrobbles, 1, 50)
seven_days_artists_per <- per_f(seven_days_artists, seven_days_scrobbles, 1, 50)
seven_days_albums_per <- per_f(seven_days_albums, seven_days_scrobbles, 1, 50)

thirty_days_tracks_per <- per_f(thirty_days_tracks, thirty_days_scrobbles, 1, 50)
thirty_days_artists_per <- per_f(thirty_days_artists, thirty_days_scrobbles, 1, 50)
thirty_days_albums_per <- per_f(thirty_days_albums, thirty_days_scrobbles, 1, 50)

ninety_days_tracks_per <- per_f(ninety_days_tracks, ninety_days_scrobbles, 1, 50)
ninety_days_artists_per <- per_f(ninety_days_artists, ninety_days_scrobbles, 1, 50)
ninety_days_albums_per <- per_f(ninety_days_albums, ninety_days_scrobbles, 1, 50)

oneeighty_days_tracks_per <- per_f(oneeighty_days_tracks, oneeighty_days_scrobbles, 1, 50)

threesixfive_days_tracks_per <- per_f(threesixfive_days_tracks, threesixfive_days_scrobbles, 1, 50)

# overall -----------------------------------------------------------------

overall_f <- function(df, s){
  df %>%
    group_by({{s}}, artist) %>%
    summarise(scrobbles = n()) %>%
    arrange(desc(scrobbles))
}

overall_artists <- scrobbles %>%
  group_by(artist) %>%
  summarise(scrobbles = n()) %>%
  arrange(desc(scrobbles))

overall_tracks <- overall_f(scrobbles, track)
overall_albums <- overall_f(scrobbles, album)


#### Listening Level ####

listening_level <- function(df){
  temp <- df %>% 
    select(artist, end_date, scrobbles) %>% 
    arrange(end_date) %>% 
    pivot_wider(names_from = end_date, values_from = scrobbles)
  temp[is.na(temp)] <- 0
  temp <- temp %>% 
    arrange(artist)%>% 
    filter(artist != "placeholder")
  return(temp)
}

one_day_artists <- one_day_artists %>% 
  mutate(end_date = date)

ll_one_day_artists <- listening_level(one_day_artists)
ll_seven_days_artists <- listening_level(seven_days_artists)
ll_thirty_days_artists <- listening_level(thirty_days_artists)
ll_ninety_days_artists <- listening_level(ninety_days_artists)


cumulative_scrobbles_by_artist <- ll_one_day_artists
cumulative_scrobbles_by_artist <- cumulative_scrobbles_by_artist %>% 
  filter(artist != "placeholder")
artist_ll <- cumulative_scrobbles_by_artist


'# shizzam <- function(y){
#     mutate(cumulative_scrobbles_by_artist, {{y}} =  3)
# }'

# cumulative_scrobbles_by_artist[is.na(cumulative_scrobbles_by_artist)] <- 0
# ll_ninety_days_artists[is.na(ll_ninety_days_artists)] <- 0
# ll_one_day_artists[is.na(ll_one_day_artists)] <- 0
# ll_seven_days_artists[is.na(ll_seven_days_artists)] <- 0
# ll_thirty_days_artists[is.na(ll_thirty_days_artists)] <- 0


for(i in 3:(ncol(cumulative_scrobbles_by_artist))){
  cumulative_scrobbles_by_artist[i] <- rowSums(cumulative_scrobbles_by_artist[(i-1):i], na.rm=TRUE)
  cumulative_scrobbles_by_artist[cumulative_scrobbles_by_artist == 0] <- NA
  }


artist_ll_helper <- full_join(cumulative_scrobbles_by_artist, ll_seven_days_artists, by = "artist") %>% 
  full_join(ll_thirty_days_artists, by = "artist") %>% 
  full_join(ll_ninety_days_artists, by = "artist")

weight_seven <- 0.17
weight_thirty <- 0.45
weight_ninety <- 0.35
weight_overall <- 0.03

adjuster_seven <- weight_seven*12
adjuster_thirty <- weight_thirty*3
adjuster_ninety <- weight_ninety
#adjuster_overall <- 

cumulative_scrobbles_by_artist <- cumulative_scrobbles_by_artist %>% 
  arrange(artist)
artist_ll <- artist_ll %>% 
  arrange(artist)


  
for(i in 2:7){
  artist_ll[i] <- cumulative_scrobbles_by_artist[i]*adjuster_seven + cumulative_scrobbles_by_artist[i]*adjuster_thirty + cumulative_scrobbles_by_artist[i]*adjuster_ninety + (90/(i - 1))*(cumulative_scrobbles_by_artist[i])*weight_overall
}

for(i in 8:30){
  artist_ll[i] <- ll_seven_days_artists[i-6]*adjuster_seven + cumulative_scrobbles_by_artist[i]*adjuster_thirty + cumulative_scrobbles_by_artist[i]*adjuster_ninety + (90/(i - 1))*(cumulative_scrobbles_by_artist[i])*weight_overall
}

for(i in 31:90){
  artist_ll[i] <- ll_seven_days_artists[i-6]*adjuster_seven + ll_thirty_days_artists[i-29]*adjuster_thirty + cumulative_scrobbles_by_artist[i]*adjuster_ninety + (90/(i - 1))*(cumulative_scrobbles_by_artist[i])*weight_overall
}

for(i in 91:(ncol(artist_ll))){
  artist_ll[i] <- ll_seven_days_artists[i-6]*adjuster_seven + ll_thirty_days_artists[i-29]*adjuster_thirty + ll_ninety_days_artists[i-89]*adjuster_ninety + (90/(i - 1))*(cumulative_scrobbles_by_artist[i])*weight_overall
}

artist_ll_stats <- artist_ll %>% 
  pivot_longer(!artist, names_to = "date", values_to = "listening_level") %>% 
  group_by(artist) %>% 
  summarise(Max = max(listening_level, na.rm = TRUE),
            Q_3 = quantile(listening_level, na.rm = TRUE, 0.75), 
            Mean = mean(listening_level, na.rm = TRUE),
            Median = median(listening_level, na.rm = TRUE),
            Q_1 = quantile(listening_level, na.rm = TRUE, 0.25))

artist_ll[2:(ncol(artist_ll))] <- round(artist_ll[2:(ncol(artist_ll))], 2)

last_7_artist_ll <- artist_ll %>% 
  select(artist, (ncol(artist_ll)-6):ncol(artist_ll)) 
  
last_7_artist_ll <- arrange(last_7_artist_ll, desc(last_7_artist_ll[8]))




