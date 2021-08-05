library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(raster)
library(move)
library(rasterVis)

shark_track_df <- read_csv("track_shark144020.csv")
shark_move <- move(x=shark_track_df$lon, y=shark_track_df$lat, 
                   time=as.POSIXct(shark_track_df$datetime, format="%Y-%m-%d", tz="UTC"), 
                   proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                   data=shark_track_df)
shark_path <- plot(shark_move, xlab="Longitude", ylab="Latitude", type="l", pch=16, lwd=0.5)
shark_points <- points(shark_move, pch=20, cex=0.5)

r <- raster("ssh_data.nc")
r_df <- as.data.frame(r, xy = TRUE)

ggplot(data = r_df) +
  geom_raster(aes(x = x, y = y, fill = Absolute.dynamic.topography)) +
  geom_path(data = shark_move_df, aes(x = lon, y = lat), color = "white") +
  geom_point(data = shark_move_df, aes(x = lon, y = lat), color = "white", size = 0.5) +
  theme_bw() +
  coord_cartesian()

