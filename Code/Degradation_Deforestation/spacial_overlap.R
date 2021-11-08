install.packages("raster")
install.packages("rgdal")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("devtools")
install.packages("benford")


library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(devtools)
library(ggplot2)
library(benford)


memory.limit(size=40000000)




setwd("C:/Users/ForestsGlobalChange1/Desktop/karen files")

GFC <- raster("clipped hansen lossyear.tif")

JRC_deforestation <- raster("clipped deforestation year.tif")
str(JRC_deforestation)

JRC_degradation <- raster("clipped degredation year.tif")
str(JRC_degradation)

RADD <- raster("RADD_NASA_Biodiv_clipped_b2.tif")

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
