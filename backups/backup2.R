library("tidyverse")
library("lubridate")
library("incidence")

#fix album NA with Dingga and other
#make program for my google sheets
#make another 7 days scrobbles that's unique (not overlapping days)
#make with bigger numbers
#make it faster by filtering tiny numbers earlier

complete_seven_days_scrobbles <- read_csv("~/R Stuff/lastFM/seven_days_scrobbles.csv")

scrobbles <- read_csv("~/R Stuff/lastFM.csv") %>% 
  mutate(date_time = parse_datetime(utc_time, "%d %b %Y, %H:%M"))

scrobbles$date_time <- with_tz(scrobbles$date_time, tzone = "America/New_York")
scrobbles <- mutate(scrobbles, date = as_date(date_time))

scrobbles <- select(scrobbles, date_time, date, artist, album, track)

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

#### one day percent ####

# 
# one_day_tracks_per <- full_join(one_day_tracks, one_day_scrobbles, key = "date", na.rm = true) %>%
#   mutate(rate = count/s_count) %>%
#   na.omit()%>%
#   arrange(desc(rate))
# 
# one_day_artists_per <- full_join(one_day_artists, one_day_scrobbles, key = "date", na.rm = true) %>%
#   mutate(rate = count/s_count) %>%
#   na.omit()%>%
#   arrange(desc(rate))
# 
# one_day_albums_per <- full_join(one_day_albums, one_day_scrobbles, key = "date", na.rm = true) %>%
#   mutate(rate = count/s_count) %>%
#   na.omit()%>%
#   arrange(desc(rate))

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
      df[i+4] <- sapply(0, "+", as.numeric(df$date - ymd("2022-1-1") - i + 1)%/%{{n}} * {{n}} + i - 1)
      names(df)[i+4] <- paste("group_", {{i}}, sep = "")
    }
    return(df)
  }
  
  df <- scrobbles_grouping(df, {{n}})
  
  group <- list()
  groups_list <- colnames(df)
  
  group_and_summarize <- function(df, s, b){
    df <-  select(df, 1:4, {{s}} + 4) 
    colnames(df)[5] <- "group"
    df <- df %>% group_by(group, {{b}}) %>%
      summarize(c = sum(scrobbles),
                date = min(group)) %>% 
      mutate(
        date = date + ymd("2022-1-1")
      )
  }
  
  for(k in 1:{{n}}){
    group[[k]] <- group_and_summarize(df, k, {{b}})
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
    select(start_date, end_date, {{b}}, scrobbles) %>%
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


# overall -----------------------------------------------------------------

overall_f <- function(df, s){
  df %>%
    group_by({{s}}) %>%
    summarise(scrobbles = n()) %>%
    arrange(desc(scrobbles))
}

overall_artists <- overall_f(scrobbles, artist)
overall_tracks <- overall_f(scrobbles, track)
overall_albums <- overall_f(scrobbles, album)

