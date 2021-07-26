# Load Defor & Degradation data

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
hansen_df <- as.data.frame(hansen_tl, xy=T)
# this takes a minute
str(hansen_df)
dim(hansen_df)

head(hansen_df) # shows you the top several rows of the data frame
summary(hansen_df) # reports the summary statistics for each column (in this case)

# convert all the other datasets to data frames

#5. Reshape the data so that you have the total deforestation summed for each year 
# column1 = Year, column2 = total defor area
# knowing that each observation = 1 30x30m pixel

# check out these links to explore functions that will be useful
# dplyr tutorial: https://genomicsclass.github.io/book/pages/dplyr_tutorial.html
# tidyr cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf