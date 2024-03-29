setwd("/Users/sophieroberts/Downloads/elsa_lab/R")
getwd()

library(raster)
library(rgdal)
library(tidyverse)

?brick

# load data

ndvi1_RE <- raster("2013_08_27_RE_ndvi.tif")
head(ndvi1_RE)
dim(ndvi1_RE)
shape_dat <- readOGR(dsn="merged_rapideye_ROI_shapefiles-selected", layer="merged_rapideye_ROI")
head(shape_dat)
summary(shape_dat)
dim(shape_dat)

# plot data
# par(mfrow=c(1,1))
plot(ndvi1_RE)
plot(shape_dat, add=T)

# extract ndvi data within ROI polygons

?extract
ROI_ndvi <- raster::extract(ndvi1_RE, shape_dat)
# this outputs a list

# explore ROI_ndvi object
str(ROI_ndvi) # list of 21 objects (matrices)
head(ROI_ndvi) # not particularly useful for lists, more useful for data frames
length(ROI_ndvi) # list of 21 objects
summary(ROI_ndvi) # summarizes each of the 21 objects (length, class, and mode)

# pull out first object in the list and store it in object called "l1"
l1 <- ROI_ndvi[[1]]
dim(l1)
length(l1) 
str(l1)
str(ROI_ndvi)
# double check length of list objects and compare to size of ROIs and # pixels (ex: 595 values)

# convert list to data frame
# the code below converts each list object (n=21) to a column and sets the number of rows to the maximum list object/vector length (595)
ROI_df <- data.frame(lapply(ROI_ndvi, "length<-", max(lengths(ROI_ndvi))))
# rename the columns
colnames(ROI_df) <- paste0("ROI_",1:21)
head(ROI_df)
dim(ROI_df)
summary(ROI_df) # notice the different number of NAs for each ROI; each column is 595 rows, but not all polygons had 595 pixels within them; NAs used to fill in the No Data rows 

# use dplyr::summarize to calculate the mean ndvi for each crown

# use gather() to reformat data (from wide to long) to use summarize function
ROI_df_long <- gather(ROI_df, "ROI", "NDVI")
head(ROI_df_long)
table(ROI_df_long$ROI)

# use summarize() to summarize all pixels within each ROI
ROI_dat <- ROI_df_long %>% group_by(ROI) %>% summarize(n=n(),
                                                       n_NAs     = sum(is.na(NDVI)),
                                                       n_pixels  = n-n_NAs,
                                                       mean_NDVI = mean(NDVI, na.rm=T),
                                                       min_NDVI  = min(NDVI, na.rm=T),
                                                       max_NDV   = max(NDVI, na.rm=T))
ROI_dat

# check number of pixels in summarized data against ROI_ndvi
ROI_dat$n_pixels
summary(ROI_ndvi) # same number of pixels, but in different order because ROI_dat lists 1, then 10...

# plot mean NDVI (y) ~ time (x) for each crown 

# revise above code by: 
# loading entire stack of NDVI data for different dates
# extracting entire stack
# convert to data frame
# assign dates to NDVI values (as colnames?)
# gather and summarize
# plot
