library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")



#non-kpop artists
non_kpop_artists <- c("Lexie Liu", "Chymes", "Dua Lipa")


kpop_overall_tracks <- overall_tracks %>% 
  filter(!artist %in% non_kpop_artists)

