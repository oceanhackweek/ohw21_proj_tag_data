library(ggplot)
library(tidyverse)
library(rayshader)
library(weathermetrics)
library(PerformanceAnalytics)
library(lubridate)

#### Load and prep data ####
#prep data for use
tracks <- read.csv(file.choose())
tracks2 <- read.csv(file.choose())

#assign sharkIDs if needed
tracks$sharkid = 144020
tracks2$sharkid = 137736

#bind them and reassigne
newtracks <- rbind(tracks, tracks2)
tracks <- newtracks

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

#change timestamp to posix
tracks$timestamp <- as.POSIXct(tracks$datetime, tz = "GMT")

#convert temperature
tracks$analysed_sst <- kelvin.to.celsius(tracks$analysed_sst)

#create new columns with month and years
tracks <- tracks %>% mutate(month = month(timestamp), year=year(timestamp)) 

#assign season to data
tracks <- tracks %>% mutate(season=ifelse(month %in% c(6,7,8,9,10), "wet","dry")) 


#### Visualizing ####

#generate frequencies of all numerical variables
tracks %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 40)

#Each significance level is associated to a symbol : 
#p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)
#could be used to determine significant collinearity between variables 
#or to pinpoint patterns to explore further
numshark <- dplyr::select_if(tracks, is.numeric)
chart.Correlation(numshark, histogram=TRUE, pch=19)



#### Plots ####

#boxplots over months/seasons by shark ID
tracks %>% ggplot(., aes(x=as.factor(month), y=lat, color = season))+geom_boxplot()+ 
  theme_bw() + facet_grid(~sharkid)

tracks %>% ggplot(., aes(x=as.factor(month),y=analysed_sst, color = season))+geom_boxplot()+ 
  theme_bw() + facet_grid(~sharkid) + theme_bw()

tracks %>% ggplot(., aes(x=as.factor(month),y=mag, color = season))+geom_boxplot()+ 
  theme_bw() + facet_wrap(~sharkid) + theme_bw()

tracks %>% ggplot(., aes(x=as.factor(month),y=sla, color = season))+geom_boxplot()+ 
  theme_bw() + facet_wrap(~sharkid) + theme_bw()

tracks %>% ggplot(., aes(x=as.factor(month),y=EKE, color = season))+geom_boxplot()+ 
  theme_bw() + facet_wrap(~sharkid) + theme_bw()

tracks %>% ggplot(., aes(x=as.factor(month),y=sla, color = season))+geom_boxplot()+ 
  theme_bw() +facet_wrap(~sharkid) + theme_bw()

hist(tracks$month)

#individual point plots to compare

g1 <- ggplot(tracks, aes(x=timestamp, y=EKE, color=season)) +
  geom_point()+ geom_smooth() + facet_grid(~sharkid, scales = "free") + theme_bw()

g2 <- ggplot(tracks, aes(x=timestamp, y=analysed_sst, color=season, group=sharkid))+
  geom_point()+geom_smooth() + facet_grid(~sharkid, scales = "free") + theme_bw()

g3 <- ggplot(tracks, aes(x=timestamp, y=lat, color=season, group=sharkid))+
  geom_point()+geom_smooth() + facet_grid(~sharkid, scales = "free") + theme_bw()


#Arrange a paneled figure for comparisons among sharks and variables
figure <- ggarrange(g3, g2, g1,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)
figure

