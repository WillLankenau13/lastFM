# Introduction

This repository gives information regarding a user's LastFM data. The main feature is it gives LastFM scrobbling "records," for example, what was the most a user listened to an artist in 30 days. 


# LastFM

[LastFM](https://www.last.fm/home) records each time a user listened to a musical track. It can be connected with Spotify, Apple Music, and other streaming platforms. Each time the user listens to a track is a "scrobble." User data can be exported [here](https://lastfm.ghan.nl/export/).


# LastFM.R

This is the main piece of code. It tidies the downloaded data and gives the "records" as mentioned above in the form of a dataframe. Dataframes are titled with the time frame and time of record; seven_days_tracks gives the most a user listened to a track in a seven day period. It lists the track name, the number of scrobbles, and the start and end dates of the seven day period. 


# Improvements and To Do List
- Fix album NA with Dingga and other
- Make another 7 days scrobbles that's unique (not overlapping days), take the original 7 days and loop through it, putting each start and end date in a list and check to see if either start or end date are between any of the start and end dates of said list
- Make it faster
- Make sure no possible entire blank 7/30/90 days span that is left out of listening levels


# Info

Date Created: July 13 2022
