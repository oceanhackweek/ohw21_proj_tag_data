library(ggplot2)
library(tidyverse)
library(weathermetrics)
library(PerformanceAnalytics)
library(lubridate)

#prep data for use
tracks <- read.csv(file.choose())

#see data columns
head(tracks)

#adt = absolute dynamic topography (SSH above the geoid) 
#ugos = absolute geostrophic velocity (east west) (m/s)
#ugosa = " anomaly
#vgos = absolute geostrophic velocity (north south)  
#vgosa = " anomaly 
#err = 
#sla = sea level anomaly (m)
#mag = ocean velocity magnitude

tracks$timestamp <- as.POSIXct(tracks$datetime, tz = "GMT")
tracks$analysed_sst <- kelvin.to.celsius(tracks$analysed_sst)

#create new columns with month and years
tracks <- tracks %>% mutate(month = month(timestamp), year=year(timestamp)) 

#create seasons
tracks <- tracks %>% mutate(season=ifelse(month %in% c(6,7,8,9,10), "wet","dry")) 

#generate frequencies of all numerical variables
tracks %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

#look at relationships between all of them
numshark <- dplyr::select_if(tracks, is.numeric)
chart.Correlation(numshark, histogram=TRUE, pch=19)

#Each significance level is associated to a symbol : 
#p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

### Plots ####
tracks %>% ggplot(., aes(x=as.factor(month),y=sla, color = season))+geom_boxplot()+ 
  theme_bw()

tracks %>% ggplot(., aes(x=lat,y=analysed_sst, color = season))+geom_boxplot()+ 
  theme_bw()

hist(tracks$month)

ggplot(tracks, aes(x=month, y=sla, color=season, group=season))+
  geom_point()+geom_smooth()