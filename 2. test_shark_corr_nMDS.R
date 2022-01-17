install.packages(c("tidyverse", "rayshader", "weathermetrics", "PerformanceAnalytics", "lubridate", "corrplot", "ggpubr", "raster", "move", "rasterVis", "googlesheets4", "here"))

library(ggplot2)
library(tidyverse)
# library(rayshader)
library(weathermetrics)
library(PerformanceAnalytics)
library(lubridate)
library(corrplot)
library(ggpubr)
# library(move)
# library(raster)
# library(rasterVis)
library(googlesheets4)
library(here)

path <- here("OHW21_proj_tag_data", "OHW_development/data outputs")
here(path)
df <- read.csv(here(path,"ssh_matched_3years_mag.csv"), header = TRUE, sep = ",")


as.data.frame(df)
str(df)
View(df)

#correlation
cor(df[,c(2,3,5:12)], method = 'spearman') # OU method = 'spearman'
plot(df[,c(2,3,5:12)], col = 1, pch = 19)

library(corrplot)
corrplot(cor(df[,c(2,3,5:12),], method ='pearson'), type = "lower",
         method = c("number"), tl.col = "black", number.digits = 1)

# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

df.z <- scale(df[, c(2,3,5:12)])    #matrix standardization

d <- dist(df.z) # euclidean distances between the rows
mds <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
mds # view results

mds <- df.z %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()

mds

# Plot MDS

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(df), cex=.7)

#group
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()

mds <- mds %>%
  mutate(groups = clust)

mds

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="nMDS", type="n")
text(x, y, labels = row.names(df), cex=.7)

devtools::install_github("tidyverse/ggplot2")

library(ggpubr)
library(tidyverse)

ggscatter(mds, x = "Dim.1", y = "Dim.2",    # group plot
          label = rownames(df[,c(2,3,5:12)]),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = FALSE)

