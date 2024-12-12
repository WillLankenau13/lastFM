library("tidyverse")
library("lubridate")
library("incidence")

#fix album NA with Dingga and other
#make program for my google sheets
#make another 7 days scrobbles that's unique
#make 7 days scrobbles reproducible with bigger numbers (30 days, 90 days, etc)
#delete 7 days scrobbles intervals that haven't ended yet

scrobbles <- read_csv("~/R Stuff/lastFM.csv") %>% 
  mutate(date_time = parse_datetime(utc_time, "%d %b %Y, %H:%M"))

scrobbles$date_time <- with_tz(scrobbles$date_time, tzone = "America/New_York")
scrobbles <- mutate(scrobbles, date = as_date(date_time))

scrobbles <- select(scrobbles, date_time, date, artist, album, track)

#### one day simple ####

one_day_f <- function(df, a, b, c){
  df %>% 
    group_by({{a}}, {{b}}, {{c}}) %>% 
    summarize(count = n()) %>% 
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

seven_days_scrobbles <- select(one_day_scrobbles, date, count)

scrobbles_grouping <- function(df, n){
  for(i in 1:{{n}}){
    df[i+2] <- sapply(0, "+", as.numeric((df[[1]] - ymd("2022-1-1") + i))%/%{{n}})
  }
  return(df)
}

renaming_groups <- function(df, n){
  for(i in 1:{{n}}){
    names(df)[i+2] <- paste("group_", {{i}}, sep = "")
  }
  return(df)
}

seven_days_scrobbles <- scrobbles_grouping(seven_days_scrobbles, 7)
seven_days_scrobbles <- renaming_groups(seven_days_scrobbles, 7)

seven_days_group <- list()
seven_days_groups_list <- colnames(seven_days_scrobbles)

group_and_summarize <- function(df, s){
  df %>% group_by(eval(parse(text=paste("group_", {{s}}, sep = "")))) %>% 
    summarize(c = sum(count),
              date = min(date))
}

for(k in 1:7){
  seven_days_group[[k]] <- group_and_summarize(seven_days_scrobbles, k)
}
  
seven_days_scrobbles <- data.frame()
for(a in 1:7){
  seven_days_scrobbles <- bind_rows(seven_days_scrobbles, seven_days_group[[a]])
}

names(seven_days_scrobbles)[1] <- 'Number'
seven_days_scrobbles  <-  seven_days_scrobbles %>% 
  filter(Number >=0) %>% 
  mutate(start_date = date,
         end_date = date + 6,
         count = c) %>% 
  select(start_date, end_date, count) %>% 
  arrange(desc(count)) 
  
seven_days_scrobbles <- seven_days_scrobbles[!duplicated(seven_days_scrobbles$start_date),]
  

# j <- 1
# for(i in 1:nrow(one_day_scrobbles)){
#   j <- i - 6
#   while(j <= i){
#     if(j > nrow(one_day_scrobbles)){
#     }else if(j > 0){
#       if((ymd(seven_days_scrobbles$start_date[[j]]) <= ymd(one_day_scrobbles$date[[i]]) && ymd(one_day_scrobbles$date[[i]])<= ymd(seven_days_scrobbles$end_date[[j]]))){
#         seven_days_scrobbles$count[[j]] <- seven_days_scrobbles$count[[j]] + one_day_scrobbles$count[[i]]
#       }
#     }
#     j <- j + 1
#   }
# }
  
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