#------------------------------------------------------------------------------------------------#
# Load and plot GEDI data
# 4-29-21
#------------------------------------------------------------------------------------------------#
# see package info here: https://github.com/carlos-alberto-silva/rGEDI
#------------------------------------------------------------------------------------------------#
library(rGEDI); library(ggplot2); library(rgdal); library(sf); library(raster); library(dplyr)
library(tidyverse) 
#------------------------------------------------------------------------------------------------#

download.dir="C:/Users/missa/Downloads/monodominant_shots"

files.GEDI02_A<-list.files(download.dir,pattern="*GEDI02_A*",recursive = F,full.names=T)
files.GEDI02_A<-files.GEDI02_A[grepl('.h5$',files.GEDI02_A)]

files.GEDI01_B <- list.files(download.dir,pattern="*GEDI01_B*",recursive = F,full.names=T)
files.GEDI01_B<-files.GEDI01_B[grepl('.h5$',files.GEDI01_B)]

#WREF_bb <- c(45.8977, 45.7792, -122.0983, -121.7786)

listof1B <- list()
for (b in 1:length(files.GEDI01_B)) {
  gedilevel1b <-readLevel1B(level1Bpath = files.GEDI01_B[b])
  level1BGeo   <- getLevel1BGeo(gedilevel1b)
  #level1bGeo_sub <- subset(level1BGeo, latitude_bin0 < WREF_bb[1] & latitude_bin0 > WREF_bb[2] & longitude_bin0 > WREF_bb[3] & longitude_bin0 < WREF_bb[4])
  #WREF_shot_numbers <- level1bGeo_sub$shot_number
  listof1B[[b]] <-level1BGeo
  clean_list1B <- listof1B[!sapply(listof1B, is.null)]
  final_dat <- do.call(rbind, clean_list1B)
}

listofdfs <- list()
for (ii in 1:length(files.GEDI02_A)){
  gedilevel2a <-readLevel2A(level2Apath = files.GEDI02_A[ii])
  level2AM   <- getLevel2AM(gedilevel2a)
  level2AM_sub <- subset(level2AM, shot_number %in% WREF_shot_numbers)
  level2AM_sub <- subset(level2AM, beam %in% c("BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"))
  level2AM_qf <- subset(level2AM_sub, quality_flag == 1)
  level2AM_sen <- subset(level2AM_qf, sensitivity >= 0.95)
  listofdfs[[ii]] <- level2AM_sen
  clean_list <- listofdfs[!sapply(listofdfs, is.null)]
  final_dat <- do.call(rbind, clean_list)
  
}
