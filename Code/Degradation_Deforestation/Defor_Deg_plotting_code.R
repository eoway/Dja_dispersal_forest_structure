# Load Defor & Degradation data

# check out these links to explore functions that will be useful
# dplyr tutorial: https://genomicsclass.github.io/book/pages/dplyr_tutorial.html
# tidyr cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#1. Load libraries that are needed for code below
# if you have not installed the libraries below, use install.packages("[library_name_here")
# notice install.packages("") uses quotes. the libraries use exactly the same name, but without quotes
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(ggplot2)

#2. Set working directory
getwd() #check the default working directory
# then set your working directory to the path location where your data are saved
setwd("G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Degradation_Deforestation_Products")
getwd()

#3. Load all Deforestation and Degradation products
hansen_tl <- raster("Hansen et al 2013/clipped hansen lossyear.tif")

str(hansen_tl) #use structure function to examine the data type
dim(hansen_tl) #rows x columns (in terms pixels) x bands
plot(hansen_tl)

# if you want to explore what a function does, use the "?" in front of the function to get more info
?as.data.frame
?raster
?str


# ...
# v_defor <- ...
# v_degradation <- ...


radd <- brick("RADD_Alert/RADD_NASA_Biodiv_clipped.tif")
#radd <- raster("RADD.tif")
radd_date <- radd$Date
radd_date <- radd[[2]]
str(radd_date) #use structure function to examine the data type


#4. Convert each into a dataframe
hansen_df <- as.data.frame(hansen_tl)
# this takes a minute
str(hansen_df)
dim(hansen_df)

head(hansen_df) # shows you the top several rows of the data frame
#summary(hansen_df) # reports the summary statistics for each column (in this case)

# Remove 0's in hansen_df using the subset() function
hansen_df_sub <- subset(hansen_df, clipped_hansen_lossyear != 0) # != means not equal to; == means equal to

summary(hansen_df_sub)
dim(hansen_df_sub)

write.csv(hansen_df_sub, "Hansen et al 2013/hansen_lossyear_df.csv")

new_df <- read_csv("Hansen et al 2013/hansen_lossyear_df.csv")

dim(hansen_df_sub)
dim(new_df)

head(hansen_df_sub)
head(new_df)

# summary(hansen_df_sub)
# summary(new_df)

# Draw a random sample of 1000 rows from new_df to write the rest of your code
# Basically create a smaller version of the orginal dataset 
new_df_samp <- new_df[sample(nrow(new_df), 1000), ]
dim(new_df_samp)
summary(new_df_samp)

par(mfrow=c(1,2))
hist(new_df$clipped_hansen_lossyear)
hist(new_df_samp$clipped_hansen_lossyear)

# Use new_df_samp for writing the rest of the code
# When the code is functioning, you can replace the new_df_samp dataframe with new_df 

# convert all the other datasets to data frames

#5. Reshape the data so that you have the total deforestation summed for each year 
# column1 = Year, column2 = total defor area
# knowing that each observation = 1 30x30m pixel
# go from new_df_samp to a new dataframe that has the columns described above
 


# just make a barchart with one of the datasets
# the dataset will require 3 columns
# column 1: Year
# column 2: Deforestation_total
# column 3: Dataset 


## END GOAL ##
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# create 'grouped barchart' with total area of deforestation on the y-axis and year on the x-axis
# the groups (different color bars) will be the 3 different datasets

