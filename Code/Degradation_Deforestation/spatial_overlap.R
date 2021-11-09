# install.packages("raster")
# install.packages("rgdal")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("devtools")
# install.packages("benford")


library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(devtools)
library(ggplot2)
library(benford)


memory.limit(size=40000000)


#setwd("C:/Users/ForestsGlobalChange1/Desktop/karen files")
setwd("G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Degradation_Deforestation_Products") #EO 

#------------------------------------------------------------------------------------------------#
# Load data
#------------------------------------------------------------------------------------------------#
# bound <- readOGR(dsn="G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/GIS/Study area", layer="Dja_300km_buffer_rectangle_land")
# plot(bound)
GFC <- raster("clipped hansen lossyear.tif")

JRC_deforestation <- raster("clipped deforestation year.tif")
str(JRC_deforestation)

JRC_degradation <- raster("clipped degredation year.tif")
str(JRC_degradation)

RADD <- raster("RADD_NASA_Biodiv_clipped_b2.tif")
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# Confirm each has the same projection and extent
#------------------------------------------------------------------------------------------------#
extent(GFC)
extent(JRC_deforestation)
extent(JRC_degradation)
extent(RADD)

projection(GFC)
projection(JRC_deforestation)
projection(JRC_degradation)
projection(RADD)

# RADD_e <- crop(RADD, bound)
# GFC_e <- crop(GFC, bound)
# JRC_deforestation_e <- crop(JRC_deforestation, RADD)
# JRC_degradation_e <- crop(JRC_degradation, RADD)
# 
# extent(GFC_e)
# extent(JRC_deforestation_e)
# extent(JRC_degradation_e)
# extent(RADD_e)

# create point shapefile from GFC data
grid <- rasterToPoints(GFC, spatial = T) # if this too slow or gives a memory limit error - use ArcMap (see tool from Sumalika)
# save grid shapefile and use readOGR next time rather than re-running rasterToPoints()
writeOGR(grid, getwd(), "GFC_points", driver="ESRI Shapefile")

grid <- readOGR(dsn=getwd(), layer="GFC_points")

# extract data from raster data for each grid point location (will output as shapefile) 
GFC_e               <- extract(GFC, grid)
JRC_deforestation_e <- extract(JRC_deforestation, grid)
JRC_degradation_e   <- extract(JRC_degradation, grid)
RADD_e              <- extract(RADD, grid)

# convert shapefiles to dataframes with coordinates (maybe byID = T)

# alternative is to resample JRC & RADD datasets to GFC using resample() function 
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# Convert values to 0s and 1s
#------------------------------------------------------------------------------------------------#
GFC[GFC > 0] <- 1
str(GFC)

JRC_deforestation[JRC_deforestation > 1999] <- 1
JRC_deforestation[JRC_deforestation < 2000] <- 0
str(JRC_deforestation)
dim(JRC_deforestation)

JRC_degradation[JRC_degradation > 1999] <- 1
JRC_degradation[JRC_degradation < 2000] <- 0
str(JRC_degradation)
dim(JRC_degradation)

diff_rast <- GFC - JRC_deforestation
warnings()
