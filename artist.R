library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")


artist_list <- list("Lexie Liu")

artist_scrobbles <- scrobbles %>% 
  filter(artist %in% artist_list)

artist_track_ll <- track_ll_stats %>% 
  filter(artist %in% artist_list)


