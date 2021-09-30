setwd("/Users/sophieroberts/Downloads/elsa_lab/R")
getwd()

library(raster)
library(rgdal)
library(tidyverse)

ndvi1_RE <- raster("2013_08_27_RE_ndvi.tif")
shape_dat <- readOGR(dsn="merged_rapideye_ROI_shapefiles-selected", layer="merged_rapideye_ROI")
head(shape_dat)
summary(shape_dat)
dim(shape_dat)
brick(ndvi1_RE)
?brick
plot(ndvi1_RE)
plot(shape_dat, add=T)
raster::extract(ndvi1_RE, shape_dat)
?extract

# try loading SR data from Planet or RapidEye to try it
plotRGB(ndvi1_RE, 3, 2, 1) 
# this will proibably outpput a list
# convert list to data frame
# use dplyr::summarize to calculate the mean ndvi for each crown
#plot mean NDVI (y) ~ time (x) for each crown 
