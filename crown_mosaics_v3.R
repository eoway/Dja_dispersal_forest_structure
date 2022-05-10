setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation")
getwd()
#------------------------------------------------------------------------------------------------#
# Elsa's working directory
#------------------------------------------------------------------------------------------------#
setwd("G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Planet")
#------------------------------------------------------------------------------------------------#

library(raster)
library(rgdal)
library(tidyverse)


# load crown data
crowns <- readOGR(dsn="delineated_crowns.shp", layer="delineated_crowns")
crowns <- readOGR(dsn=getwd(), layer="delineated_crowns") # EO load
phen_crowns <- subset(crowns, Point_FID != "NA")
#crown_test <- subset(crowns, Point_FID == 63)
final_trees <- subset(crowns, Point_FID %in% c(63, 219, 111))
head(final_trees)

# load RGB mosaics
mosaic_2020_5_27 <- brick("mosaics/2020-05-27_1003_mosaic.tif")
mosaic_2020_5_27 <- brick("2020-05-27_1003_mosaic.tif") # EO load

# plot mosaic data
plot(mosaic_2020_5_27) # plot each layer (n=4) separately
plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin") # plot a composite of multiple bands


# load EVI mosaics (each image is one date from the year 2021)
# you'll stack all dates to get multiple dates from 2021, roughly 1-2 per month
# load every single file in the path directory that's a tif file (all your EVI mosaic images) - call it rastlist
rastlist <- list.files(path = "/path/to/wd", pattern='.TIF$',all.files=TRUE, full.names=FALSE)
# stack every file from rastlist 
evi_stack <- stack(rastlist)


# look at projection and crs (coordinate reference system)
projection(final_trees)
crs(final_trees) #longlat / longitude latitude 

projection(mosaic_2020_5_27)
crs(mosaic_2020_5_27) # UTM Zone 33

# set final_trees crs to be same as raster data (mosaic)
new_crs <- crs(mosaic_2020_5_27) # could replace mosaic_2020_5_27 with EVI data
final_treesUTM <- spTransform(final_trees, CRS=new_crs)
projection(final_treesUTM) #make sure the new final_trees projection is now UTM

#set IDs for final_trees
final_treesUTM$CID <- seq(1:length(final_trees$Point_FID))

# plot 
plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin", ext=final_treesUTM)
plot(final_treesUTM, border="white", lwd=3, add=T)

# # subset to just five final_trees (final_trees 10-15) and look at those
# crownsUTM_sub <- crownsUTM[10:15,] 
# plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin", ext=crownsUTM_sub)
# plot(crownsUTM, border="black", add=T)

# examine data
res(mosaic_2020_5_27)
head(final_treesUTM)
dim(final_treesUTM)


# extract crown info for each band separately (this takes a minute)
mosaic_crowns_b1_1 <- raster::extract(mosaic_2020_5_27[[1]], final_treesUTM) #B
mosaic_crowns_b2_1 <- raster::extract(mosaic_2020_5_27[[2]], final_treesUTM) #G
mosaic_crowns_b3_1 <- raster::extract(mosaic_2020_5_27[[3]], final_treesUTM) #R
mosaic_crowns_b4_1 <- raster::extract(mosaic_2020_5_27[[4]], final_treesUTM) #NIR
class(mosaic_crowns_b1_1) #object of class list - need to convert to data frame

# first, examine list
str(mosaic_crowns_b1_1) # list of 285 objects (matrices)
head(mosaic_crowns_b1_1) # not particularly useful for lists, more useful for data frames
dim(mosaic_crowns_b1_1) # doesn't work on lists to get dimensions, so use length() instead in next line
length(mosaic_crowns_b1_1) # good, same number of list objects as crown polygons (n=285)
summary(mosaic_crowns_b1_1) # summarizes each of the 285 objects (length, class, and mode)
# in the summary output, length tells you how many elements are in each list
# notice each object/vector within the list is of a different length 
# each object/vector consists of pixel values extracted for each crown (n=285)

### XXX ###
# this #/4 is equivalent to the number of pixels per polygon


#------------------------------------------------------------------------------------------------#
# convert list to data frame
#------------------------------------------------------------------------------------------------#
# the code below converts each list object (n=285) to a column and sets the number of rows to the maximum list object/vector length (595)
mosaic_crowns_b1_1_df <- data.frame(lapply(mosaic_crowns_b1_1, "length<-", max(lengths(mosaic_crowns_b1_1))))
# rename the columns
dim(mosaic_crowns_b1_1_df)
colnames(mosaic_crowns_b1_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b1_1_df))
colnames(mosaic_crowns_b1_1_df) <- paste0("Crown_",c(63, 219, 111))
head(mosaic_crowns_b1_1_df)
dim(mosaic_crowns_b1_1_df)
summary(mosaic_crowns_b1_1_df) # notice the different number of NAs for each ROI; each column is 595 rows, but not all polygons had 595 pixels within them; NAs used to fill in the No Data rows 

# do the same for the extracted lists from bands 2-4 
mosaic_crowns_b2_1_df <- data.frame(lapply(mosaic_crowns_b2_1, "length<-", max(lengths(mosaic_crowns_b2_1))))
colnames(mosaic_crowns_b2_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b2_1_df))

mosaic_crowns_b3_1_df <- data.frame(lapply(mosaic_crowns_b3_1, "length<-", max(lengths(mosaic_crowns_b3_1))))
colnames(mosaic_crowns_b3_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b3_1_df))

mosaic_crowns_b4_1_df <- data.frame(lapply(mosaic_crowns_b4_1, "length<-", max(lengths(mosaic_crowns_b4_1))))
colnames(mosaic_crowns_b4_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b4_1_df))

#------------------------------------------------------------------------------------------------#
# use dplyr::summarize to calculate the mean band value for each crown
#------------------------------------------------------------------------------------------------#
# use gather() to reformat data to use summarize function
mosaic_crowns_b1_1_df_long <- gather(mosaic_crowns_b1_1_df, "Crown", "B1")
head(mosaic_crowns_b1_1_df_long)
table(mosaic_crowns_b1_1_df_long$Crown)
# the above code converts the data from 'wide' format to 'long' format
mosaic_crowns_b2_1_df_long <- gather(mosaic_crowns_b2_1_df, "Crown", "B2")
mosaic_crowns_b3_1_df_long <- gather(mosaic_crowns_b3_1_df, "Crown", "B3")
mosaic_crowns_b4_1_df_long <- gather(mosaic_crowns_b4_1_df, "Crown", "B4")

head(mosaic_crowns_b2_1_df_long)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Combine all data into a single data frame
#------------------------------------------------------------------------------------------------#
mosaic_crowns_all_1 <- cbind(mosaic_crowns_b1_1_df_long, mosaic_crowns_b2_1_df_long$B2, mosaic_crowns_b3_1_df_long$B3, mosaic_crowns_b4_1_df_long$B4)
head(mosaic_crowns_all_1)
colnames(mosaic_crowns_all_1) <- c("Crown","B1","B2","B3","B4")
head(mosaic_crowns_all_1)

# use summarize() to summarize all pixels within each ROI
Crown_dat_1 <- mosaic_crowns_all_1 %>% group_by(Crown) %>% summarize(n=n(),
                                                                     n_NAs     = sum(is.na(B1)),
                                                                     n_pixels  = n-n_NAs,
                                                                     mean_B1 = mean(B1, na.rm=T),
                                                                     mean_B2 = mean(B2, na.rm=T),
                                                                     mean_B3 = mean(B3, na.rm=T),
                                                                     mean_B4 = mean(B4, na.rm=T))
Crown_dat_1
Crown_dat_1$date = "2020-05-27"

# check number of pixels in summarized data against Crown_dat_1
Crown_dat_1$n_pixels
summary(Crown_dat_1) # same number of pixels, but in different order because Crown_dat_1 lists 1, then 10...
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# summarize again to get single EVI value per crown per month
#------------------------------------------------------------------------------------------------#
Crown_dat_reformat <- gather(Crown_dat_1, "band", "EVI", -c(Crown, n, n_NAs, n_pixels)) # where columns 5:8 are your band values - in this case bands, will be dates
head(Crown_dat_reformat)
#Crown_dat_reformat$month <- # pull month value from date value

monthly_dat <- Crown_dat_reformat %>% group_by(month) %>% summarize(n=n(), 
                                                             mean_EVI = mean(EVI, na.rm=T))

# from here plot (ggplot - geom_point and geom_line): 
# y-axis = EVI
# x-axis = month
# group / color / fill = Crown 

# can add precip data to this
#------------------------------------------------------------------------------------------------#















#------------------------------------------------------------------------------------------------#
# repeat with new mosaic
#------------------------------------------------------------------------------------------------#
# load mosaic
mosaic_2020_9_4 <- brick("mosaics/2020-09-04_1004_mosaic.tif")


# plot mosaic data
plot(mosaic_2020_9_4) # plot each layer (n=4) separately
plotRGB(mosaic_2020_9_4, r=4,g=2,b=1, stretch = "lin") # plot a composite of multiple bands

# plot 
plotRGB(mosaic_2020_9_4, r=4,g=2,b=1, stretch = "lin", ext=crownsUTM)
plot(crownsUTM, border="black", add=T)

# subset to just five crowns (crowns 10-15) and look at those
crownsUTM_sub <- crownsUTM[10:15,] 
plotRGB(mosaic_2020_9_4, r=4,g=2,b=1, stretch = "lin", ext=crownsUTM_sub)
plot(crownsUTM, border="black", add=T)

# examine data
res(mosaic_2020_9_4)
head(crownsUTM)
dim(crownsUTM)


# extract crown info for each band separately (this takes a minute)
mosaic_crowns_b1_2 <- raster::extract(mosaic_2020_9_4[[1]], crownsUTM)
mosaic_crowns_b2_2 <- raster::extract(mosaic_2020_9_4[[2]], crownsUTM)
mosaic_crowns_b3_2 <- raster::extract(mosaic_2020_9_4[[3]], crownsUTM)
mosaic_crowns_b4_2 <- raster::extract(mosaic_2020_9_4[[4]], crownsUTM)


#------------------------------------------------------------------------------------------------#
# convert list to data frame
#------------------------------------------------------------------------------------------------#
# the code below converts each list object (n=285) to a column and sets the number of rows to the maximum list object/vector length (595)
mosaic_crowns_b1_2_df <- data.frame(lapply(mosaic_crowns_b1_2, "length<-", max(lengths(mosaic_crowns_b1_2))))
# rename the columns
dim(mosaic_crowns_b1_2_df)
colnames(mosaic_crowns_b1_2_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b1_2_df))
head(mosaic_crowns_b1_2_df)
dim(mosaic_crowns_b1_2_df)
summary(mosaic_crowns_b1_2_df) 

# do the same for the extracted lists from bands 2-4 
mosaic_crowns_b2_2_df <- data.frame(lapply(mosaic_crowns_b2_2, "length<-", max(lengths(mosaic_crowns_b2_2))))
colnames(mosaic_crowns_b2_2_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b2_2_df))

mosaic_crowns_b3_2_df <- data.frame(lapply(mosaic_crowns_b3_2, "length<-", max(lengths(mosaic_crowns_b3_2))))
colnames(mosaic_crowns_b3_2_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b3_2_df))

mosaic_crowns_b4_2_df <- data.frame(lapply(mosaic_crowns_b4_2, "length<-", max(lengths(mosaic_crowns_b4_2))))
colnames(mosaic_crowns_b4_2_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b4_2_df))

#------------------------------------------------------------------------------------------------#
# use dplyr::summarize to calculate the mean band value for each crown
#------------------------------------------------------------------------------------------------#
# use gather() to reformat data to use summarize function
mosaic_crowns_b1_2_df_long <- gather(mosaic_crowns_b1_2_df, "Crown", "B1")
head(mosaic_crowns_b1_2_df_long)
table(mosaic_crowns_b1_2_df_long$Crown)
# the above code converts the data from 'wide' format to 'long' format
mosaic_crowns_b2_2_df_long <- gather(mosaic_crowns_b2_2_df, "Crown", "B2")
mosaic_crowns_b3_2_df_long <- gather(mosaic_crowns_b3_2_df, "Crown", "B3")
mosaic_crowns_b4_2_df_long <- gather(mosaic_crowns_b4_2_df, "Crown", "B4")

head(mosaic_crowns_b2_2_df_long)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Combine all data into a single data frame
#------------------------------------------------------------------------------------------------#
mosaic_crowns_all_2 <- cbind(mosaic_crowns_b1_2_df_long, mosaic_crowns_b2_2_df_long$B2, mosaic_crowns_b3_2_df_long$B3, mosaic_crowns_b4_2_df_long$B4)
head(mosaic_crowns_all_2)
colnames(mosaic_crowns_all_2) <- c("Crown","B1","B2","B3","B4")
head(mosaic_crowns_all_2)

# use summarize() to summarize all pixels within each ROI
Crown_dat_2 <- mosaic_crowns_all_2 %>% group_by(Crown) %>% summarize(n=n(),
                                                                     n_NAs     = sum(is.na(B1)),
                                                                     n_pixels  = n-n_NAs,
                                                                     mean_B1 = mean(B1, na.rm=T),
                                                                     mean_B2 = mean(B2, na.rm=T),
                                                                     mean_B3 = mean(B3, na.rm=T),
                                                                     mean_B4 = mean(B4, na.rm=T))
Crown_dat_2
Crown_dat_2$date = "2020-09-04"

Crown_dat_2

# check number of pixels in summarized data against Crown_dat_2
Crown_dat_2$n_pixels
summary(Crown_dat_2) # same number of pixels, but in different order because Crown_dat_2 lists 1, then 10...















#------------------------------------------------------------------------------------------------#
# Plot
#------------------------------------------------------------------------------------------------#
ggplot(mosaic_crowns, aes(x,y))+
  geom_line()

