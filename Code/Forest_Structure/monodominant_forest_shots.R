#------------------------------------------------------------------------------------------------#
# Load and plot GEDI data
#------------------------------------------------------------------------------------------------#
# see package info here: https://github.com/carlos-alberto-silva/rGEDI
#------------------------------------------------------------------------------------------------#
library(rGEDI); library(ggplot2); library(rgdal); library(sf); library(raster); library(dplyr)
library(tidyverse) 
#------------------------------------------------------------------------------------------------#

# set this to your working directory where the folder with the files is located
#download.dir="~/Desktop/NASA/Dja_monodominant_shots-2" 
download.dir="C:/Users/elsa-admin/Desktop/Dja_monodominant_shots" 



files.GEDI02_A<-list.files(download.dir,pattern="*GEDI02_A*",recursive = F,full.names=T)
files.GEDI02_A<-files.GEDI02_A[grepl('.h5$',files.GEDI02_A)]

files.GEDI01_B <- list.files(download.dir,pattern="*GEDI01_B*",recursive = F,full.names=T)
files.GEDI01_B<-files.GEDI01_B[grepl('.h5$',files.GEDI01_B)]

# This for loop needs to be udpated to include the use of getLevel1BWF(), which returns rxwaveform and elevation
listof1B <- list()
for (b in 1:length(files.GEDI01_B)) {
  gedilevel1b <-readLevel1B(level1Bpath = files.GEDI01_B[1])
  level1BGeo   <- getLevel1BGeo(gedilevel1b)
  # for loop within the for loop to iterate the following over each shot
#  for (i in 1:length(level1BGeo$shot_number))
#  level1BWF   <- getLevel1BWF(gedilevel1b, shot_number = level1BGeo$shot_number[i])
  listof1B[[b]] <-level1BGeo
  clean_list1B <- listof1B[!sapply(listof1B, is.null)]
  final_dat1A <- do.call(rbind, clean_list1B)
}

head(final_dat1A)
dim(final_dat1A)

listofdfs <- list()
for (ii in 1:length(files.GEDI02_A)){
  gedilevel2a <-readLevel2A(level2Apath = files.GEDI02_A[ii])
  level2AM   <- getLevel2AM(gedilevel2a)
  #level2AM_sub <- subset(level2AM, shot_number %in% mbers) #don't necessarily need this line
  #level2AM_sub <- subset(level2AM, beam %in% c("BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011")) # come back to this (look at full power/coverage beams for now)
  level2AM_qf <- subset(level2AM, quality_flag == 1)
  level2AM_sen <- subset(level2AM_qf, sensitivity >= 0.95)
  listofdfs[[ii]] <- level2AM_sen
  clean_list <- listofdfs[!sapply(listofdfs, is.null)]
  final_dat2A <- do.call(rbind, clean_list) 
}

head(final_dat2A)


### For code below, you're selecting a single shot
# This is an example
# But what we want is to calculate an average across all shots and plot that

# look at the number of shots in the final_dat2A data frame
length(final_dat2A$shot_number) 

#-------------------------------------------------------------------------------------#
# Using 1 hf file to test plotWFMetrics plotting by hand 
#-------------------------------------------------------------------------------------#

# using the readLevel2A and readLevel1B functions to load a single hf file 
gedilevel2a <-readLevel2A(level2Apath = files.GEDI02_A[3])
gedilevel1b <-readLevel1B(level1Bpath = files.GEDI01_B[3])

level2AM   <- getLevel2AM(gedilevel2a)

head(level2AM)

#level1bGeo_sub <- subset(level1B, quality_flag == 1)
level2AM_sub <- subset(level2AM, quality_flag == 1)

head(level2AM_sub)
dim(level2AM_sub)

# you can pick any number between 1 and the number of shots (8 in this example) to put in the brackets
shot_num   <- as.character(level2AM$shot_number[6]) 
plotWFMetrics(gedilevel1b, gedilevel2a, shot_num, rh=c(25, 50, 75, 90))

# Use level 1B data to create that part of the plot manually 
level1B <- getLevel1BGeo(gedilevel1b)
level1BWF <- getLevel1BWF(gedilevel1b, shot_num) #This has everything you need for now (Rxwaveform and elevation)
level1BWF$binned_elev <- cut(level1BWF$elevation, breaks = c(seq(700, 1000, by=5))) # edit the sequence as needed 


# then take the average rxwaveform across shots for each bin 

new_dat <- level1BWF_all_shots %>% group_by(binned_elev) %>% summarize(avg_rxwf = mean(rxwaveform, na.rm=T),
                                                                       sd_rxwf = sd(rxwaveform, na.rm=T),
                                                                       n_shots = n(),
                                                                       CI_lower = avg_rxwf - 1.960 * (sd_rxwf / sqrt(n_shots)), 
                                                                      CI_upper = avg_rxwf + 1.960 * (sd_rxwf / sqrt(n_shots))) 


wf_df <- as.data.frame(level1BWF@dt)

# subset l1B & l2A to shot number selected above
l1b_single_shot <- subset(level1B, shot_number == shot_num)
l2a_single_shot <- subset(level2AM_sub, shot_number == shot_num)

#------------------------------------------------------------------------------------#
# rh       = relative height data from l2a
# top      = highest return - in elevation (m)
# ground   = ground return - in elevation (m) = elev_lowestmode + rh0
# bin0     = elevation closest to GEDI sensor (higher in elevation/altitude)
# last_bin = lowest elevation (often - elevation, e.g. -4m)
#------------------------------------------------------------------------------------#

rh          <- l2a_single_shot[,12:112]
#rh          <- l2a_single_shot[,13:113] # for Valentina's subset data
bin0        <- l1b_single_shot$elevation_bin0 
last_bin    <- l1b_single_shot$elevation_lastbin
top         <- l2a_single_shot$elev_highestreturn
elev_lowest <- l2a_single_shot$elev_lowestmode 
ground      <- rh[[1]] + elev_lowest # same as l2a_single_shot$rh0 + elev_lowest
coords      <- c(l1b_single_shot$latitude_lastbin, l1b_single_shot$longitude_lastbin)

ggplot() + 
  geom_line(data=wf_df, aes(elevation, rxwaveform), lwd=1) + # come back to this (1B & 2A)
  coord_flip() +
  # xlim(ground-20, top+20) + 
  # geom_vline(xintercept = top, lty=2, lwd=1) + 
  # geom_vline(xintercept = ground, lty=2, lwd=1) + 
  labs(y="Waveform Amplitude", x="Elevation (m)")  +
  theme_classic()
