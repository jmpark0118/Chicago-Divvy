## LOAD LIBRARY
# The Usual Suspects
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(magrittr)
library(data.table)
library(stringr)
# Fetching
library(rvest)
# Cleaning column names
library(janitor)
# Date/Time formatting
library(lubridate)
# Maps
library(ggmap)
# Used for animated density plots
library(gganimate)
# Only needed for interactive maps
library(leaflet)
library(leaflet.extras)

## LOAD DATA

## 2014
# Read and merge
data14 <- lapply("C:/Users/jeong/Desktop/chicago/2014" %>% list.files, function(name) {
  return(read_csv(paste0("C:/Users/jeong/Desktop/chicago/2014/",name)))
})

rides14 <- rbindlist(data14[-1],fill = T)
rides14 %<>% clean_names(case = "snake")

rides14$age <- 2019-rides14$birthyear
rides14$age_bin <- rides14$age %>% .bincode(seq(0,120,20))
rides14$age_bin <- sapply(rides14$age_bin,function(bin) {
  return(paste0((bin-1)*20,"-",(bin*20)," Years Old"))
})

colnames(rides14)[2] <- 'start_time'


# Trip times
rides14$minutes <- rides14$tripduration/60
rides14$hours <- rides14$tripduration/60/60
# Start times
rides14$start_hour <- lubridate::hour(strptime(rides14$start_time, '%m/%d/%Y %H:%M'))
rides14$mm <- hour(strptime(rides14$start_time, '%m/%d/%Y %H:%M'))*60 + 
  minute(strptime(rides14$start_time, '%m/%d/%Y %H:%M'))
rides14$start_day <- wday(as.Date(rides14$start_time, '%m/%d/%Y'),
                          label =  T, abbr = F, week_start = 1)
# Weekend/Weekday
rides14$start_day_type <- ifelse(wday(as.Date(rides14$start_time, '%m/%d/%Y'), 
                                      week_start = 1)>5, "Weekend", "Weekday")
# Week of year
rides14$week <- week(as.Date(rides14$start_time, '%m/%d/%Y'))
# Month (1-12)
rides14$month <- month(as.Date(rides14$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Month (January-December)
rides14$month_text <- month(as.Date(rides14$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Remove unused levels from factor
rides14$month_text <- droplevels(rides14$month_text)


station14 <- read.csv("C:/Users/jeong/Desktop/chicago/2014/Divvy_Stations_2014.csv")
rides14 %<>% left_join(station14 %>% select(latitude, longitude, id), 
                       by = c('from_station_id' = 'id'))
rides14 %<>% left_join(station14 %>% select(latitude, longitude, id), 
                       by = c('to_station_id' = 'id'),
                       suffix = c('_start', '_end'))



## 2015
# Read and merge
data15 <- lapply("C:/Users/jeong/Desktop/chicago/2015" %>% list.files, function(name) {
  return(read_csv(paste0("C:/Users/jeong/Desktop/chicago/2015/",name)))
})

rides15 <- rbindlist(data15[-1],fill = T)
rides15 %<>% clean_names(case = "snake")

rides15$age <- 2019-rides15$birthyear
rides15$age_bin <- rides15$age %>% .bincode(seq(0,120,20))
rides15$age_bin <- sapply(rides15$age_bin,function(bin) {
  return(paste0((bin-1)*20,"-",(bin*20)," Years Old"))
})

colnames(rides15)[2] <- 'start_time'

# Trip times
rides15$minutes <- rides15$tripduration/60
rides15$hours <- rides15$tripduration/60/60
# Start times
rides15$start_hour <- lubridate::hour(strptime(rides15$start_time, '%m/%d/%Y %H:%M'))
rides15$mm <- hour(strptime(rides15$start_time, '%m/%d/%Y %H:%M'))*60 + 
  minute(strptime(rides15$start_time, '%m/%d/%Y %H:%M'))
rides15$start_day <- wday(as.Date(rides15$start_time, '%m/%d/%Y'),
                          label =  T, abbr = F, week_start = 1)
# Weekend/Weekday
rides15$start_day_type <- ifelse(wday(as.Date(rides15$start_time, '%m/%d/%Y'), 
                                      week_start = 1)>5, "Weekend", "Weekday")
# Week of year
rides15$week <- week(as.Date(rides15$start_time, '%m/%d/%Y'))
# Month (1-12)
rides15$month <- month(as.Date(rides15$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Month (January-December)
rides15$month_text <- month(as.Date(rides15$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Remove unused levels from factor
rides15$month_text <- droplevels(rides15$month_text)
head(rides15)


station15 <- read.csv("C:/Users/jeong/Desktop/chicago/2015/Divvy_Stations_2015.csv")
rides15 %<>% left_join(station15 %>% select(latitude, longitude, id), 
                       by = c('from_station_id' = 'id'))
rides15 %<>% left_join(station15 %>% select(latitude, longitude, id), 
                       by = c('to_station_id' = 'id'),
                       suffix = c('_start', '_end'))



## 2016
# Read and merge
data16 <- lapply("C:/Users/jeong/Desktop/chicago/2016" %>% list.files, function(name) {
  return(read_csv(paste0("C:/Users/jeong/Desktop/chicago/2016/",name)))
})

rides16 <- rbindlist(data16[-1],fill = T)
rides16 %<>% clean_names(case = "snake")

rides16$age <- 2019-rides16$birthyear
rides16$age_bin <- rides16$age %>% .bincode(seq(0,120,20))
rides16$age_bin <- sapply(rides16$age_bin,function(bin) {
  return(paste0((bin-1)*20,"-",(bin*20)," Years Old"))
})

colnames(rides16)[2] <- 'start_time'

# Trip times
rides16$minutes <- rides16$tripduration/60
rides16$hours <- rides16$tripduration/60/60
# Start times
rides16$start_hour <- lubridate::hour(strptime(rides16$start_time, '%m/%d/%Y %H:%M'))
rides16$mm <- hour(strptime(rides16$start_time, '%m/%d/%Y %H:%M'))*60 + 
  minute(strptime(rides16$start_time, '%m/%d/%Y %H:%M'))
rides16$start_day <- wday(as.Date(rides16$start_time, '%m/%d/%Y'),
                          label =  T, abbr = F, week_start = 1)
# Weekend/Weekday
rides16$start_day_type <- ifelse(wday(as.Date(rides16$start_time, '%m/%d/%Y'), 
                                      week_start = 1)>5, "Weekend", "Weekday")
# Week of year
rides16$week <- week(as.Date(rides16$start_time, '%m/%d/%Y'))
# Month (1-12)
rides16$month <- month(as.Date(rides16$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Month (January-December)
rides16$month_text <- month(as.Date(rides16$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Remove unused levels from factor
rides16$month_text <- droplevels(rides16$month_text)


station16 <- read.csv("C:/Users/jeong/Desktop/chicago/2016/Divvy_Stations_2016.csv")
rides16 %<>% left_join(station15 %>% select(latitude, longitude, id), 
                       by = c('from_station_id' = 'id'))
rides16 %<>% left_join(station15 %>% select(latitude, longitude, id), 
                       by = c('to_station_id' = 'id'),
                       suffix = c('_start', '_end'))


## 2017

# Read and merge
data17 <- lapply("C:/Users/jeong/Desktop/chicago/2017" %>% list.files, function(name) {
  return(read_csv(paste0("C:/Users/jeong/Desktop/chicago/2017/",name)))
})

rides17 <- rbindlist(data17[-1],fill = T)
rides17 %<>% clean_names(case = "snake")

rides17$age <- 2019-rides17$birthyear
rides17$age_bin <- rides17$age %>% .bincode(seq(0,120,20))
rides17$age_bin <- sapply(rides17$age_bin,function(bin) {
  return(paste0((bin-1)*20,"-",(bin*20)," Years Old"))
})


# Trip times
rides17$minutes <- rides17$tripduration/60
rides17$hours <- rides17$tripduration/60/60
# Start times
rides17$start_hour <- lubridate::hour(strptime(rides17$start_time, '%m/%d/%Y %H:%M:%S'))
rides17$mm <- hour(strptime(rides17$start_time, '%m/%d/%Y %H:%M:%S'))*60 + 
  minute(strptime(rides17$start_time, '%m/%d/%Y %H:%M:%S'))
rides17$start_day <- wday(as.Date(rides17$start_time, '%m/%d/%Y'),
                          label =  T, abbr = F, week_start = 1)
# Weekend/Weekday
rides17$start_day_type <- ifelse(wday(as.Date(rides17$start_time, '%m/%d/%Y'), 
                                      week_start = 1)>5, "Weekend", "Weekday")
# Week of year
rides17$week <- week(as.Date(rides17$start_time, '%m/%d/%Y'))
# Month (1-12)
rides17$month <- month(as.Date(rides17$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Month (January-December)
rides17$month_text <- month(as.Date(rides17$start_time, '%m/%d/%Y'), label = T,abbr = F)
# Remove unused levels from factor
rides17$month_text <- droplevels(rides17$month_text)


station17 <- read.csv("C:/Users/jeong/Desktop/chicago/2017/Divvy_Stations_2017.csv")
rides17 %<>% left_join(station17 %>% select(latitude, longitude, id), 
                       by = c('from_station_id' = 'id'))
rides17 %<>% left_join(station17 %>% select(latitude, longitude, id), 
                       by = c('to_station_id' = 'id'),
                       suffix = c('_start', '_end'))









