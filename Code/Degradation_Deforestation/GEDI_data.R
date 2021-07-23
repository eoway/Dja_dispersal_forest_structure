#------------------------------------------------------------------------------------------------#
# Load and plot GEDI data
# 4-29-21
#------------------------------------------------------------------------------------------------#
# see package info here: https://github.com/carlos-alberto-silva/rGEDI
#------------------------------------------------------------------------------------------------#
library(rGEDI); library(ggplot2); library(rgdal); library(sf); library(raster); library(dplyr)
library(tidyverse)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# location of your GEDI .h5 files
outdir="C:/Users/elsao/Desktop/temp_files/GEDI"

# #------------------------------------------------------------------------------------------------#
# # load .h5 files
# #------------------------------------------------------------------------------------------------#
gedilevel1b <-readLevel1B(level1Bpath = paste0(outdir,"\\GEDI01_B_2019130130016_O02309_T01444_02_003_01.h5"))
gedilevel2a <-readLevel2A(level2Apath = paste0(outdir,"\\GEDI02_A_2019130130016_O02309_T01444_02_001_01.h5"))
gedilevel2b <-readLevel2B(level2Bpath = paste0(outdir,"\\GEDI02_B_2019130130016_O02309_T01444_02_001_01.h5"))
# #------------------------------------------------------------------------------------------------#
# level1bGeo         <- getLevel1BGeo(gedilevel1b)
# level2AM           <- getLevel2AM(gedilevel2a)
# wf_dat             <- getLevel1BWF(gedilevel1b, shot_number=shot_num)
# wf_df              <- as.data.frame(wf_dat@dt)
# level2B_totalPAI   <- getLevel2BVPM(gedilevel2b) # "pai" variable = total PAI
# level2BPAIProfile  <- getLevel2BPAIProfile(gedilevel2b)
# level2BPAVDProfile <- getLevel2BPAVDProfile(gedilevel2b)
# 
# write.csv(level1bGeo_sub, paste0(outdir,"/level1bGeo_sub.csv"))
# write.csv(level2AM_sub, paste0(outdir,"/level2AM_sub.csv"))
# write.csv(wf_df, paste0(outdir,"/level1bWF.csv"))
# write.csv(level2B_totalPAI, paste0(outdir,"/level2B_totalPAI.csv"))
# write.csv(level2BPAIProfile, paste0(outdir,"/level2BPAIProfile.csv"))
# write.csv(level2BPAVDProfile, paste0(outdir,"/level2BPAVDProfile.csv"))
# write.csv(pgap_df, paste0(outdir,"/pgap_df.csv"))

#------------------------------------------------------------------------------------#
# load temp datasets (CSV files created from h5 files above)
#------------------------------------------------------------------------------------#
# level1bGeo_sub     <- read_csv(paste0(outdir,"/level1bGeo_sub.csv"))
# level2AM_sub       <- read_csv(paste0(outdir,"/level2AM_sub.csv"))
# wf_df              <- read_csv(paste0(outdir,"/level1bWF.csv"))
# 
# # finish using files above then run line 45, then load files below
# #rm(level1bGeo_sub); rm(level2AM_sub); rm(wf_df)
# 
# level2B_totalPAI   <- read_csv(paste0(outdir,"/level2B_totalPAI.csv"))
# level2BPAIProfile  <- read_csv(paste0(outdir,"/level2BPAIProfile.csv"))
# level2BPAVDProfile <- read_csv(paste0(outdir,"/level2BPAVDProfile.csv"))
#pgap_df            <- read_csv(paste0(outdir,"/pgap_df.csv"))
#------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------#
# Waveforms - Level 1B & 2A 
#------------------------------------------------------------------------------------#
# extract info of interest from .h5 files
level1bGeo <- getLevel1BGeo(gedilevel1b)
level2AM   <- getLevel2AM(gedilevel2a)

# look at data pulled from L1B
head(level1bGeo)
# look at data pulled from L2A
head(level2AM)

# Wind River Experimental Forest bounding box coordinates (based on area that overlaps with NEON data)
WREF_bb <- c(45.8977, 45.7792, -122.0983, -121.7786)

# subset L1B data to WREF bounding box
level1bGeo_sub <- subset(level1bGeo, latitude_bin0 < WREF_bb[1] & latitude_bin0 > WREF_bb[2] & longitude_bin0 > WREF_bb[3] & longitude_bin0 < WREF_bb[4])
summary(level1bGeo_sub)
# create vector of shot numbers within WREF
WREF_shot_numbers <- level1bGeo_sub$shot_number

# subset L2A data to WREF shots
level2AM_sub <- subset(level2AM, shot_number %in% WREF_shot_numbers)
summary(level2AM_sub)
# subset L2A data to full-power beams only
#level2AM_sub <- subset(level2AM_sub, beam %in% c("BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011"))
# subset L2A data to quality == 1 only
level2AM_sub <- subset(level2AM_sub, quality_flag == 1)

# select shot number
length(level2AM_sub$shot_number) 
# there are 220 shots within the WREF area from this file that are usable based on the quality flag

# you can pick any number between 1 and the number of shots (220 in this example) to put in the brackets
shot_num   <- as.character(level2AM_sub$shot_number[160]) 
plotWFMetrics(gedilevel1b, gedilevel2a, shot_num, rh=c(25, 50, 75, 90))
#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
# subset l1B & l2A to shot number selected above
l1b_single_shot <- subset(level1bGeo_sub, shot_number == shot_num)
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
#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
# convert GEDI waveform data into dataframe
#------------------------------------------------------------------------------------#
wf_dat <- getLevel1BWF(gedilevel1b, shot_number=shot_num)
wf_df <- as.data.frame(wf_dat@dt)
#------------------------------------------------------------------------------------#

ggplot() + 
  geom_line(data=wf_df, aes(elevation, rxwaveform), lwd=1) +
  coord_flip() +
  xlim(ground-20, top+20) + # include data 20m above and below GEDI defined ground & top
  geom_vline(xintercept = top, lty=2, lwd=1) + 
  geom_vline(xintercept = ground, lty=2, lwd=1) + 
  labs(y="Waveform Amplitude", x="Elevation (m)") + 
  theme_classic()
#------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------#
# PAI and PAVD profiles  - Level 2B
#------------------------------------------------------------------------------------#
level2B_totalPAI      <- getLevel2BVPM(gedilevel2b) # "pai" variable = total PAI
level2BPAIProfile     <- getLevel2BPAIProfile(gedilevel2b)
level2BPAVDProfile    <- getLevel2BPAVDProfile(gedilevel2b)

# load source code to import pgap theta z data
# Valentina, ignore next two lines of code if lodaing pgap_df directly as csv file
source("G:/My Drive/Projects/GEDI/Code/getLevel2BPgapProfile.R")
level2BPgapProfile    <- getLevel2BPgapProfile2(gedilevel2b, shot_number=shot_num)
pgap_df <- as.data.frame(level2BPgapProfile@dt)
head(pgap_df)

ggplot() + 
  geom_line(data=pgap_df, aes(elevation, pgap_theta_z), lwd=1) +
  coord_flip() +
  labs(y=expression(pgap~theta~(z)), x="Elevation (m)") + 
  theme_classic()


# subset l2B total PAI to shot number selected above
l2b_total_PAI <- subset(level2B_totalPAI, shot_number == shot_num)
l2b_total_PAI$pai
l2b_total_PAI$pgap_theta # total pgap(theta)

# subset l2B PAI & PAVD profiles to shot number selected above
l2b_PAI_profile  <- subset(level2BPAIProfile, shot_number == shot_num)

l2b_PAI_profile_df <- as.data.frame(cbind(t(l2b_PAI_profile[,12:41]),seq(2.5,147.5,by=5)))
#l2b_PAI_profile_df <- as.data.frame(cbind(t(l2b_PAI_profile[,13:42]),seq(2.5,147.5,by=5))) ] # for Valentina's subset data
colnames(l2b_PAI_profile_df) <- c("PAI","height")
l2b_PAI_profile_df$PAI <- ifelse(l2b_PAI_profile_df$PAI == 0, NA, l2b_PAI_profile_df$PAI)

ggplot() + 
  geom_line(data=l2b_PAI_profile_df, aes(PAI, height), lwd=1) +
  ylim(0,50) + 
  labs(y="Height from ground (m)", x="Cumulative PAI") + 
  theme_classic()
#------------------------------------------------------------------------------------#
