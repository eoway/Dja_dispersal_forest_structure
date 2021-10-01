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
?brick
plot(ndvi1_RE)
plot(shape_dat, add=T)

# this will probably output a list
ROI_ndvi <- raster::extract(ndvi1_RE, shape_dat)
?extract
ROI_ndvi
head(ROI_ndvi)
dim(ROI_ndvi)
length(ROI_ndvi)
summary(ROI_ndvi)
l1 <- ROI_ndvi[[1]]
dim(l1)
length(l1) 
str(l1)
str(ROI_ndvi)
# double check length of list objects and compare to size of ROIs and # pixels (ex: 595 values)

# convert list to data frame
clean_list <- ROI_ndvi[!sapply(ROI_ndvi, is.null)]
ROI_df <- do.call(rbind, clean_list)
dim(ROI_df)
head(ROI_df)
str(ROI_df)


# use dplyr::summarize to calculate the mean ndvi for each crown
#plot mean NDVI (y) ~ time (x) for each crown 


