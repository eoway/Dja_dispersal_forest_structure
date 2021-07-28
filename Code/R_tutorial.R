## R Tutorial 

#------------------------------------------------------------------------------------------#
# General tutorials and resources
#------------------------------------------------------------------------------------------#
# https://education.rstudio.com/learn/beginner/
# https://data-flair.training/blogs/r-tutorials-home/
# https://www.guru99.com/r-tutorial.html
# https://www.statmethods.net/r-tutorial/index.html
#------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------#
# Load and Mosaic NEON DTM & DSM Data for Soaproot & Teakettle
# 7-28-21
#------------------------------------------------------------------------------------------#
getwd()
setwd("G:/My Drive/Forests_&_Global_Change/Tutorials_Cheat_Sheets/R_tutorial_data")
getwd()
#------------------------------------------------------------------------------------------#
library(raster); library(rgdal)
#------------------------------------------------------------------------------------------#
# library(dplyr); library(ggplot2); library(viridis); library(stringr)
# library(ggfortify); library(cowplot); require(data.table); library(tidyr)
# library(raster); library(rgdal); library(sp); library(GISTools); library(sf)
# library(SDMTools); library(fasterize); library(purrr); library(forcats)
#------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------#
#                               Load Site/Plot Boundaries                                  # 
#------------------------------------------------------------------------------------------#
# load the ForestPlots plot boundaries
# ForPlots = readOGR(dsn="Harvard/CAO_data/GIS", layer="Sepilok_plot_shapefile"); plot(ForPlots)
# ForPlots_spka <- subset(ForPlots, str_detect(Location, "Sepilok-Alluvial")); plot(ForPlots_spka)
# ForPlots_spks <- subset(ForPlots, str_detect(Location, "Sepilok-Sandstone")); plot(ForPlots_spks)
# ForPlots_spkh <- subset(ForPlots, str_detect(Location, "Sepilok-Heath")); plot(ForPlots_spkh)
#------------------------------------------------------------------------------------------#


#https://www.neonscience.org/resources/learning-hub/tutorials/create-chm-rasters-r

# load raster data downloaded from the NEON AOP Data Portal
# https://data.neonscience.org/data-products/explore
soap_dtm1 <- raster("NEON_D17_SOAP_DP3_293000_4100000_DTM.tif")
#soap_dtm2 <- raster("NEON_D17_SOAP_DP3_306000_4098000_DTM.tif")
soap_dtm2 <- raster("NEON_D17_SOAP_DP3_293000_4101000_DTM.tif")
soap_dtm3 <- raster("NEON_D17_SOAP_DP3_293000_4102000_DTM.tif")
soap_dtm4 <- raster("NEON_D17_SOAP_DP3_293000_4103000_DTM.tif")

soap_dsm1 <- raster("NEON_D17_SOAP_DP3_293000_4100000_DSM.tif")
soap_dsm2 <- raster("NEON_D17_SOAP_DP3_293000_4101000_DSM.tif")
soap_dsm3 <- raster("NEON_D17_SOAP_DP3_293000_4102000_DSM.tif")
soap_dsm4 <- raster("NEON_D17_SOAP_DP3_293000_4103000_DSM.tif")

# plot the one of the Soaproot digital terrain model (DTM) tiles and digital surface model (DSM) tiles
plot(soap_dtm1)
plot(soap_dsm1)

# change the plotting parameters to plot 2 figures at the same time
par(mfrow=c(1,2))
plot(soap_dtm1)
plot(soap_dsm1)

plot(soap_dtm1, col=heat.colors(255))
plot(soap_dsm1, col=rainbow(255))

# use raster math to create a canopy height model (CHM)
soap_chm1 <- soap_dsm1 - soap_dtm1
par(mfrow=c(1,1))
plot(soap_chm1, col=rainbow(255))

dim(soap_chm1)
summary(soap_chm1)
res(soap_chm1)

#head(soap_chm1)
str(soap_chm1)

dat_stack <- stack(soap_dtm1, soap_dsm1, soap_chm1)
dim(dat_stack)
dim(soap_chm1)

plot(dat_stack, col=rainbow(255))

res(soap_dtm1); res(soap_dsm1)
projection(soap_dtm1); projection(soap_dsm1)
extent(soap_dtm1); extent(soap_dsm1)


# load SOAP & TEAK flight bounding box (tiled)
SOAP_flight_box = readOGR(dsn="G:/My Drive/Forests_&_Global_Change/Tutorials_Cheat_Sheets/R_tutorial_data", layer="2019_SOAP_4_merged_tiles")
SOAP_flight_box = readOGR(dsn=getwd(), layer="2019_SOAP_4_merged_tiles") # same as above, with getwd() instead
plot(SOAP_flight_box)

TEAK_flight_box = readOGR(dsn=getwd(), layer="2019_TEAK_4_merged_tiles")
plot(TEAK_flight_box)

# load SOAP & TEAK plot locations
#SOAP_plots = readOGR(dsn=getwd(), layer="2019_SOAP_plot_locations")

plot(SOAP_flight_box)
plot(soap_dtm1, add=T, col=rainbow(255))
plot(soap_dtm2, add=T, col=rainbow(255))
plot(soap_dtm3, add=T, col=rainbow(255))
plot(soap_dtm4, add=T, col=rainbow(255))

plot(SOAP_flight_box)
plot(soap_dtm1, add=T, col=rainbow(255), zlim=c(500,2000))
plot(soap_dtm2, add=T, col=rainbow(255), zlim=c(500,2000))
plot(soap_dtm3, add=T, col=rainbow(255), zlim=c(500,2000))
plot(soap_dtm4, add=T, col=rainbow(255), zlim=c(500,2000))

# documentation for the mosaic() function: https://www.rdocumentation.org/packages/raster/versions/3.4-10/topics/mosaic
soap_dtm_mosaic <- mosaic(soap_dtm1,soap_dtm2,soap_dtm3,soap_dtm4)
?mosaic
soap_dtm_mosaic <- mosaic(soap_dtm1,soap_dtm2,soap_dtm3,soap_dtm4, fun=mean)
plot(soap_dtm_mosaic, col=rainbow(255))
plot(SOAP_flight_box, add=T)
plot(SOAP_flight_box, add=T, border="red")

# write (save) raster mosaic 
writeRaster(soap_dtm_mosaic, "G:/My Drive/Forests_&_Global_Change/Tutorials_Cheat_Sheets/R_tutorial_data/test_SOAP_DTM_mosaic.tif")


# create data frame from raster data 
soap_dtm_mosaic_df <- as.data.frame(soap_dtm_mosaic, xy=T)

head(soap_dtm_mosaic_df)
colnames(soap_dtm_mosaic_df)
colnames(soap_dtm_mosaic_df) <- c("lon","lat","elev")
colnames(soap_dtm_mosaic_df)

dim(soap_dtm_mosaic_df)
summary(soap_dtm_mosaic_df)

# plot histogram 
hist(soap_dtm_mosaic_df$elev)

# create canopy height models for 3 other tiles and mosaic 
soap_chm2 <- soap_dsm2 - soap_dtm2
soap_chm3 <- soap_dsm3 - soap_dtm3
soap_chm4 <- soap_dsm4 - soap_dtm4
soap_chm_mosaic <- mosaic(soap_chm1,soap_chm2,soap_chm3,soap_chm4, fun=mean)
plot(soap_chm_mosaic)

soap_chm_mosaic_df <- as.data.frame(soap_chm_mosaic, xy=T)

# plot histograms of DTM and CHM distributions in same figure
par(mfrow=c(1,2))
hist(soap_dtm_mosaic_df$elev)
hist(soap_chm_mosaic_df$layer)

par(mfrow=c(2,1))
hist(soap_dtm_mosaic_df$elev)
hist(soap_chm_mosaic_df$layer)

dim(soap_dtm_mosaic_df)
dim(soap_chm_mosaic_df)

colnames(soap_dtm_mosaic_df)
colnames(soap_dtm_mosaic_df) <- c("x","y","elev")

colnames(soap_chm_mosaic_df)
colnames(soap_chm_mosaic_df) <- c("x","y","height")

library(tidyverse)
dtm_chm_df <- inner_join(soap_dtm_mosaic_df, soap_chm_mosaic_df, by="x")

rm(soap_chm1); rm(soap_chm2); rm(soap_chm3); rm(soap_chm4)
rm(soap_dtm1); rm(soap_dtm2); rm(soap_dtm3); rm(soap_dtm4)
rm(soap_dsm1); rm(soap_dsm2); rm(soap_dsm3); rm(soap_dsm4)
rm(dat_stack)
rm(soap_chm_mosaic); rm(soap_dtm_mosaic)

dtm_chm_df <- inner_join(soap_dtm_mosaic_df, soap_chm_mosaic_df, by="x")

head(soap_dtm_mosaic_df); head(soap_chm_mosaic_df)
tail(soap_dtm_mosaic_df); tail(soap_chm_mosaic_df)

# add height as column to elevation data 
soap_dtm_mosaic_df$height <- soap_chm_mosaic_df$height

head(soap_dtm_mosaic_df)

plot(soap_dtm_mosaic_df$elev, soap_dtm_mosaic_df$height)

# sample rows from data frame to plot
dat_samp <- soap_dtm_mosaic_df[sample(nrow(soap_dtm_mosaic_df), 100), ]
dim(dat_samp)
plot(dat_samp$elev, dat_samp$height)
plot(dat_samp$elev, dat_samp$height, xlab ="elevation (m)" , ylab = "height (m)", pch=19)
reg<-lm(height ~ elev, data = dat_samp)
abline(reg, add=T, col="red")
# pch = 19
# col = "red"