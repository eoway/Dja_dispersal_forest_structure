setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/")
getwd()

library(raster)
library(rgdal)
library(tidyverse)


# load crown data
crowns <- readOGR(dsn="delineated_crowns.shp", layer="delineated_crowns")
phen_crowns <- subset(crowns, Point_FID != "NA") # this gets rid of crowns not associated with tree list
final_trees <- subset(crowns, Point_FID %in% c(63, 219, 203)) # these are the top 3 trees by dbh with sufficient related ground data
head(final_trees)

# load ndvi mosaics (each image is one date from the year 2021)
# stack all dates to get multiple dates from 2021
# load every single file in the path directory that's a tif file (all your ndvi mosaic images) - call it rastlist
rastlist <- list.files(path = "NDVI", pattern='.tif$',all.files=TRUE, full.names=FALSE)
head(rastlist)

# stack every file from rastlist 
setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/NDVI/") #set path for images to be stacked
ndvi_stack <- stack(rastlist) # this doesn't work because not all images are same extent


# testing it out with 1 image from 2021-03-27
setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/") #reset working directory
ndvi_2021_03_27 = stack("NDVI/4311133_3340504_2021-03-27_2414_ndvi.tif")


# set final_trees crs to be same as raster data
new_crs <- crs(ndvi_2021_03_27)
final_treesUTM <- spTransform(final_trees, CRS=new_crs)
projection(final_treesUTM) #make sure the new final_trees projection is now UTM

#set IDs for final_trees
final_treesUTM$CID <- seq(1:length(final_trees$Point_FID))

# plot raster with crowns overlayed
plot(ndvi_2021_03_27[[1]], ext=final_treesUTM)
plot(final_treesUTM, border="black", add = T)


# extract crown info for each date separately
ndvi_03_27 <- raster::extract(ndvi_2021_03_27[[1]], final_treesUTM)
#ndvi_04_17 <- raster::extract(ndvi_2021_04_17[[1]], final_treesUTM) #this is an example of what I'd do if all the images stacked
class(ndvi_03_27) #object of class list - need to convert to data frame


# first, examine list
str(ndvi_03_27) # list of 3 objects (matrices)
summary(ndvi_03_27) # summarizes each of the 3 objects (length, class, and mode)
# in the summary output, length tells you how many elements are in each list
# notice each object/vector within the list is of a different length 
# each object/vector consists of pixel values extracted for each crown (n=3)

#------------------------------------------------------------------------------------------------#
# convert list to data frame
#------------------------------------------------------------------------------------------------#
# the code below converts each list object (n=3) to a column and sets the number of rows to the maximum list object/vector length (50)
ndvi_03_27_df <- data.frame(lapply(ndvi_03_27, "length<-", max(lengths(ndvi_03_27))))
# rename the columns
dim(ndvi_03_27_df)
colnames(ndvi_03_27_df) <- paste0("Crown_",1:ncol(ndvi_03_27_df))
colnames(ndvi_03_27_df) <- paste0("Crown_",c(63, 219, 203))
head(ndvi_03_27_df)
dim(ndvi_03_27_df)
summary(ndvi_03_27_df) # notice the different number of NAs for each ROI; each column is 50 rows, but not all polygons had 50 pixels within them; NAs used to fill in the No Data rows 

# do the same for the extracted lists from other dates 
#ndvi_04_17_df <- data.frame(lapply(ndvi_04_17, "length<-", max(lengths(ndvi_04_17)))) not ready to do yet but example of how it would continue with new dates
#colnames(ndvi_04_17_df) <- paste0("Crown_",1:ncol(ndvi_04_17_df))


#------------------------------------------------------------------------------------------------#
# use dplyr::summarize to calculate the mean band value for each crown
#------------------------------------------------------------------------------------------------#
# use gather() to reformat data to use summarize function
ndvi_03_27_df_long <- gather(ndvi_03_27_df, "Crown", "2021-03-27") # i don't think this is right
head(ndvi_03_27_df_long)
table(ndvi_03_27_df_long$Crown)
# the above code converts the data from 'wide' format to 'long' format
#ndvi_04_17_df_long <- gather(ndvi_04_17_df, "Crown", "2021-03-27") will eventually do for all dates


#------------------------------------------------------------------------------------------------#
# Combine all data into a single data frame
#------------------------------------------------------------------------------------------------#
#ndvi_crowns_all <- cbind(ndvi_03_27_df_long, ndvi_04_17_df_long...) # not ready to do yet bc don't have all dates
head(ndvi_crowns_all)
# colnames(ndvi_crowns_all) <- c("Crown","2021-03-27","2021-04-17") will do with all dates
head(ndvi_crowns_all)

# use summarize() to summarize all pixels within each ROI
Crown_dat_1 <- ndvi_03_27_df %>% group_by(Crown) %>% summarize(n=n(),
                                                               n_NAs     = sum(is.na(B1)),
                                                               n_pixels  = n-n_NAs,
                                                               mean_NDVI = mean(B1, na.rm=T))

colMeans(ndvi_03_27_df, na.rm=TRUE)


Crown_dat_1
Crown_dat_1$date = "2020-03-27"

# check number of pixels in summarized data against Crown_dat_1
Crown_dat_1$n_pixels
summary(Crown_dat_1) # same number of pixels, but in different order because Crown_dat_1 lists 1, then 10...
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# summarize again to get single ndvi value per crown per month
#------------------------------------------------------------------------------------------------#
Crown_dat_reformat <- gather(Crown_dat_1, "band", "ndvi", -c(Crown, n, n_NAs, n_pixels)) # where columns 5:8 are your band values - in this case bands, will be dates
head(Crown_dat_reformat)
#Crown_dat_reformat$month <- # pull month value from date value

monthly_dat <- Crown_dat_reformat %>% group_by(month) %>% summarize(n=n(), 
                                                                    mean_ndvi = mean(ndvi, na.rm=T))

# from here plot (ggplot - geom_point and geom_line): 
# y-axis = ndvi
# x-axis = month
# group / color / fill = Crown 

# can add precip data to this
#------------------------------------------------------------------------------------------------#
