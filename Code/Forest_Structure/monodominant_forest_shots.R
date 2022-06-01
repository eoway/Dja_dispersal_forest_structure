#------------------------------------------------------------------------------------------------#
# Load and plot GEDI data
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

# WREF_bb <- c(45.8977, 45.7792, -122.0983, -121.7786)

# This for loop needs to be udpated to include the use of getLevel1BWF(), which returns rxwaveform and elevation
listof1B <- list()
listWF_all_shots <- list()
final_dat1BWF_all <- list()

for (b in 1:length(files.GEDI01_B)) {
  gedilevel1b <-readLevel1B(level1Bpath = files.GEDI01_B[b])
  level1BGeo   <- getLevel1BGeo(gedilevel1b)
  # for loop within the for loop to iterate the following over each shot
  len <- length(level1BGeo$shot_number)
  for (i in 1:len){
    level1BWF   <- getLevel1BWF(gedilevel1b, shot_number = level1BGeo$shot_number[i])
    level1BWF_df <- as.data.frame(level1BWF@dt) #convert output to a dataframe
    level1BWF_df$shot_num <- rep(level1BGeo$shot_number[i], nrow(level1BWF_df))# add column for the shotnumber ID - repeat for the length of rows 
    listWF_all_shots[[i]] <- level1BWF_df 
    clean_list1BWF <- listWF_all_shots[!sapply(listWF_all_shots, is.null)]
    final_dat1BWF <- do.call(rbind, clean_list1BWF)
    # store the dataframe created (columns: rxwaveform, elevation, shot_num) for each iteration through each shotnumber AND each file
    # this may need to be done within this for loop, OR outside of it
  }
  final_dat1BWF_all[[b]] <- final_dat1BWF
  clean_list1BWF_all <- final_dat1BWF_all[!sapply(final_dat1BWF_all, is.null)]
  all_shots_WF <- do.call(rbind, clean_list1BWF_all)
  
  listof1B[[b]] <-level1BGeo
  clean_list1B <- listof1B[!sapply(listof1B, is.null)]
  final_dat1B <- do.call(rbind, clean_list1B)
} 

head(all_shots_WF)

dim(all_shots_WF)
table(all_shots_WF$shot_num)
length(all_shots_WF$shot_num)
length(table(all_shots_WF$shot_num)) #424
# make sure 424 = the total number of shots across all files


length(all_shots_WF)


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

# ground   = ground return - in elevation (m) = elev_lowestmode + rh0
final_dat2A$ground <- final_dat2A$elev_lowestmode + final_dat2A$rh0
head(final_dat2A$ground)


#all_shots_WF$ht <- all_shots_WF$elev - final_dat2A$ground
all_shots_WF$height <- all_shots_WF$elev - final_dat2A$ground

comb_all_data <- cbind(final_dat2A,all_shots_WF )
head(comb_all_data)
length(comb_all_data)

comb_all_data <- comb_all_data  %>% 
  mutate(ht = if_else(all_shots_WF$shot_num == final_dat2A$shot_number, all_shots_WF$shot_num + final_dat2A$shot_number, all_shots_WF$shot_num - final_dat2A$shot_number))


#iff (all_shots_WF$shot_num == final_dat2A$shot_number) conditonal statements, apply, combine two columns conditional on another column


head(all_shots_WF)
table(all_shots_WF)
dim(all_shots_WF)


head(final_dat2A)
dim(final_dat2A)
str(final_dat2A)

#select shot number
length(final_dat2A$shot_number) 

# Using 1 hf file to test plotWFMetrics plotting by hand 
#gedilevel2a <-readLevel2A(level2Apath = files.GEDI02_A[3])
#gedilevel1b <-readLevel1B(level1Bpath = files.GEDI01_B[3])

#level2AM   <- getLevel2AM(gedilevel2a)

#head(level2AM)

#level1bGeo_sub <- subset(level1B, quality_flag == 1)
#level2AM_sub <- subset(level2AM, quality_flag == 1)

#head(level2AM_sub)
#dim(level2AM_sub)

# you can pick any number between 1 and the number of shots (8 in this example) to put in the brackets
#shot_num   <- as.character(final_dat2A$shot_number[ii]) 
#plotWFMetrics(listWF_all_shots, final_dat2A, shot_num, rh=c(25, 50, 75, 90))

# Use level 1B data to create that part of the plot manually 
#level1B <- getLevel1BGeo(gedilevel1b)
#level1BWF <- getLevel1BWF(gedilevel1b, shot_num) #This has everything you need for now (Rxwaveform and elevation)


#---------------------------------------------------------------------------------------------------------#
# use the output dataframe from your for loop that includes all shots from all files 
all_shots_WF$binned_elev <- cut(all_shots_WF$elevation, seq(0, max(all_shots_WF$elevation), by=5))  # this may need updating too
  # get info from Elsa on binning the elevation column
head(all_shots_WF)


# then take the average rxwaveform across shots for each bin 

new_dat <- all_shots_WF %>% group_by(binned_elev) %>% summarize(elev = mean(elevation, na.rm=T),
                                                                avg_rxwf = mean(rxwaveform, na.rm=T),
                                                                       sd_rxwf = sd(rxwaveform, na.rm=T),
                                                                       n_shots = n(),
                                                                       CI_lower = avg_rxwf - 1.960 * (sd_rxwf / sqrt(n_shots)), 
                                                                       CI_upper = avg_rxwf + 1.960 * (sd_rxwf / sqrt(n_shots))) 



head(new_dat)

# use the ggplot example below
# plot binned_elev and rxwaveform 
# add confidence intervals (CI_lower, CI_upper) using geom_ribbon
#-----------------------------------------------------------------------------------------------------------------#

final_dat1BWF$binned_elev <- cut(final_dat1BWF$elevation, seq(0, max(final_dat1BWF$elevation), by=5))  # this may need updating too
# get info from Elsa on binning the elevation column
head(final_dat1BWF)
str(final_dat1BWF)
dim(final_dat1BWF)

# then take the average rxwaveform across shots for each bin 

new_dat <- final_dat1BWF %>% group_by(binned_elev) %>% summarize(avg_rxwf = mean(rxwaveform, na.rm=T),
                                                                    sd_rxwf = sd(rxwaveform, na.rm=T),
                                                                    n_shots = n(),
                                                                    CI_lower = avg_rxwf - 1.960 * (sd_rxwf / sqrt(n_shots)), 
                                                                    CI_upper = avg_rxwf + 1.960 * (sd_rxwf / sqrt(n_shots))) 

dim(new_dat)

ggplot() + 
  geom_line(data=new_dat, aes(x=binned_elev, y= avg_rxwf), lwd=1) + # come back to this (1B & 2A)
  coord_flip() +
  # xlim(ground-20, top+20) + 
  # geom_vline(xintercept = top, lty=2, lwd=1) + 
  # geom_vline(xintercept = ground, lty=2, lwd=1) + 
  labs(y="Waveform Amplitude", x="Elevation (m)")  +
  theme_classic()


#---------------------------------------------------------------------------------------------------------#
wf_df <- as.data.frame(level1BWF@dt)

level2AM <- getLevel2AM(gedilevel2a)
#level2AMWF <- getLevel2AMWF(gedilevel2a, shot_num)
wf_df_2a <- as.data.frame(level2AMWF@dt)

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
  xlim(ground-20, top+20) + 
  geom_vline(xintercept = top, lty=2, lwd=1) + 
  geom_vline(xintercept = ground, lty=2, lwd=1) + 
  labs(y="Waveform Amplitude", x="Elevation (m)")  +
  theme_classic()

#for (i in final_dat1A) {
#  ggplot([,i]) 
#    geom_line(data=wf_df, aes(elevation, rxwaveform), lwd=1) + # come back to this (1B & 2A)
#    coord_flip() +
#    xlim(ground-20, top+20) + 
#    geom_vline(xintercept = top, lty=2, lwd=1) + 
#    geom_vline(xintercept = ground, lty=2, lwd=1) + 
#    labs(y="Waveform Amplitude", x="Elevation (m)") +
#    theme_classic()
#}

################# variability ################# 

################# 2A ################# 

elevation_difference2A <- (final_dat2A$elev_highestreturn - (final_dat2A$rh1 + final_dat2A$elev_lowestmode))
elev_diff2A <- as.data.frame(elevation_difference2A)
#elev_diff1A

head(elevation_difference2A)

avg2A <- mean(elev_diff2A$elevation_difference2A)
avg2A

avg2A_rh100 <- mean(final_dat2A$rh100, na.rm = TRUE)
avg2A_rh100

med2A <- median(elev_diff2A$elevation_difference2A)
med2A

a <- quantile(elev_diff2A$elevation_difference2A, 0.05)
a

b <- quantile(elev_diff2A$elevation_difference2A, 0.95)
b



################# 2A ################# 

elevation_difference2A <- (final_dat2A$elev_highestreturn - final_dat2A$elev_lowestmode)
elev_diff2A <- as.data.frame(elevation_difference2A)
#elev_diff2A
avg2A <- mean(elev_diff2A$elevation_difference2A)
avg2A

med2A <- median(elev_diff2A$elevation_difference2A)
med2A

c <- quantile(elev_diff2A$elevation_difference2A, 0.05)
c

d <- quantile(elev_diff2A$elevation_difference2A, 0.95)
d

ggplot() + 
  geom_line(data=wf_df, aes(elevation, rxwaveform), lwd=1) + # come back to this (1B & 2A)
  coord_flip() +
  xlim(ground-20, top+20) + 
  geom_vline(xintercept = top, lty=2, lwd=1) + 
  geom_vline(xintercept = ground, lty=2, lwd=1) + 
  labs(y="Waveform Amplitude", x="Elevation (m)")  +
  theme_classic()

for (i in final_dat1A) {
  ggplot() +
    geom_line(data=wf_df, aes(elevation, rxwaveform), lwd=1) + # come back to this (1B & 2A)
    coord_flip() +
    xlim(ground-20, top+20) + 
    geom_vline(xintercept = top, lty=2, lwd=1) + 
    geom_vline(xintercept = ground, lty=2, lwd=1) + 
    labs(y="Waveform Amplitude", x="Elevation (m)") +
    theme_classic()
}
