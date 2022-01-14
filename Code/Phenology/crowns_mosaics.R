#------------------------------------------------------------------------------------------------#
# Sophie's working directory
#------------------------------------------------------------------------------------------------#
setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation")
#------------------------------------------------------------------------------------------------#
# Elsa's working directory
#------------------------------------------------------------------------------------------------#
setwd("G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Planet")
#------------------------------------------------------------------------------------------------#
getwd()

library(raster)
library(rgdal)
library(tidyverse)


# load crown data
crowns <- readOGR(dsn="delineated_crowns", layer="delineated_crowns")
crowns <- readOGR(dsn=getwd(), layer="delineated_crowns") # EO load


# load mosaic
mosaic_2020_5_27 <- brick("mosaics/2020-05-27_1003_mosaic.tif")
mosaic_2020_5_27 <- brick("2020-05-27_1003_mosaic.tif") # EO load


# plot mosaic data
plot(mosaic_2020_5_27) # plot each layer (n=4) separately
plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin") # plot a composite of multiple bands

# look at projection and crs (coordinate reference system)
projection(crowns)
crs(crowns) #longlat / longitude latitude 

projection(mosaic_2020_5_27)
crs(mosaic_2020_5_27) # UTM Zone 33

# set crowns scr to be same as mosaic
new_crs <- crs(mosaic_2020_5_27)
crownsUTM <- spTransform(crowns, CRS=new_crs)
projection(crownsUTM) #make sure the new crowns projection is now UTM

#set IDs for crowns
crownsUTM$CID <- seq(1:length(crowns$Point_FID))

# plot 
plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin", ext=crownsUTM)
plot(crownsUTM, border="black", add=T)

# subset to just five crowns (crowns 10-15) and look at those
crownsUTM_sub <- crownsUTM[10:15,] 
plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin", ext=crownsUTM_sub)
plot(crownsUTM, border="black", add=T)

# examine data
res(mosaic_2020_5_27)
head(crownsUTM)
dim(crownsUTM)


# extract crown info for each band separately (this takes a minute)
mosaic_crowns_b1 <- raster::extract(mosaic_2020_5_27[[1]], crownsUTM)
mosaic_crowns_b2 <- raster::extract(mosaic_2020_5_27[[2]], crownsUTM)
mosaic_crowns_b3 <- raster::extract(mosaic_2020_5_27[[3]], crownsUTM)
mosaic_crowns_b4 <- raster::extract(mosaic_2020_5_27[[4]], crownsUTM)
class(mosaic_crowns_b1) #object of class list - need to convert to data frame

# first, examine list
str(mosaic_crowns_b1) # list of 285 objects (matrices)
head(mosaic_crowns_b1) # not particularly useful for lists, more useful for data frames
dim(mosaic_crowns_b1) # doesn't work on lists to get dimensions, so use length() instead in next line
length(mosaic_crowns_b1) # good, same number of list objects as crown polygons (n=285)
summary(mosaic_crowns_b1) # summarizes each of the 285 objects (length, class, and mode)
# in the summary output, length tells you how many elements are in each list
# notice each object/vector within the list is of a different length 
# each object/vector consists of pixel values extracted for each crown (n=285)

### XXX ###
# this #/4 is equivalent to the number of pixels per polygon


#------------------------------------------------------------------------------------------------#
# convert list to data frame
#------------------------------------------------------------------------------------------------#
# the code below converts each list object (n=285) to a column and sets the number of rows to the maximum list object/vector length (595)
mosaic_crowns_b1_df <- data.frame(lapply(mosaic_crowns_b1, "length<-", max(lengths(mosaic_crowns_b1))))
# rename the columns
dim(mosaic_crowns_b1_df)
colnames(mosaic_crowns_b1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b1_df))
head(mosaic_crowns_b1_df)
dim(mosaic_crowns_b1_df)
summary(mosaic_crowns_b1_df) # notice the different number of NAs for each ROI; each column is 595 rows, but not all polygons had 595 pixels within them; NAs used to fill in the No Data rows 

# do the same for the extracted lists from bands 2-4 
mosaic_crowns_b2_df <- data.frame(lapply(mosaic_crowns_b2, "length<-", max(lengths(mosaic_crowns_b2))))
colnames(mosaic_crowns_b2_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b2_df))

mosaic_crowns_b3_df <- data.frame(lapply(mosaic_crowns_b3, "length<-", max(lengths(mosaic_crowns_b3))))
colnames(mosaic_crowns_b3_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b3_df))

mosaic_crowns_b4_df <- data.frame(lapply(mosaic_crowns_b4, "length<-", max(lengths(mosaic_crowns_b4))))
colnames(mosaic_crowns_b4_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b4_df))

#------------------------------------------------------------------------------------------------#
# use dplyr::summarize to calculate the mean band value for each crown
#------------------------------------------------------------------------------------------------#
# use gather() to reformat data to use summarize function
mosaic_crowns_b1_df_long <- gather(mosaic_crowns_b1_df, "Crown", "B1")
head(mosaic_crowns_b1_df_long)
table(mosaic_crowns_b1_df_long$Crown)
# the above code converts the data from 'wide' format to 'long' format
mosaic_crowns_b2_df_long <- gather(mosaic_crowns_b2_df, "Crown", "B2")
mosaic_crowns_b3_df_long <- gather(mosaic_crowns_b3_df, "Crown", "B3")
mosaic_crowns_b4_df_long <- gather(mosaic_crowns_b4_df, "Crown", "B4")

head(mosaic_crowns_b2_df_long)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Combine all data into a single data frame
#------------------------------------------------------------------------------------------------#
mosaic_crowns_all <- cbind(mosaic_crowns_b1_df_long, mosaic_crowns_b2_df_long$B2, mosaic_crowns_b3_df_long$B3, mosaic_crowns_b4_df_long$B4)
head(mosaic_crowns_all)
colnames(mosaic_crowns_all) <- c("Crown","B1","B2","B3","B4")
head(mosaic_crowns_all)

# use summarize() to summarize all pixels within each ROI
Crown_dat <- mosaic_crowns_all %>% group_by(Crown) %>% summarize(n=n(),
                                                       n_NAs     = sum(is.na(B1)),
                                                       n_pixels  = n-n_NAs,
                                                       mean_B1 = mean(B1, na.rm=T),
                                                       mean_B2 = mean(B2, na.rm=T),
                                                       mean_B3 = mean(B3, na.rm=T),
                                                       mean_B4 = mean(B4, na.rm=T))
Crown_dat

# check number of pixels in summarized data against Crown_dat
Crown_dat$n_pixels
summary(Crown_dat) # same number of pixels, but in different order because Crown_dat lists 1, then 10...
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Plot
#------------------------------------------------------------------------------------------------#
ggplot(mosaic_crowns, aes(x,y))+
  geom_line()

