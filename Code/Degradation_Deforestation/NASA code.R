# Load Defor & Degradation data

# check out these links to explore functions that will be useful
# dplyr tutorial: https://genomicsclass.github.io/book/pages/dplyr_tutorial.html
# tidyr cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#1. Load libraries that are needed for code below
# if you have not installed the libraries below, use install.packages("[library_name_here")
# notice install.packages("") uses quotes. the libraries use exactly the same name, but without quotes
install.packages("raster")
install.packages("rgdal")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("devtools")
install.packages("benford")
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(devtools)
library(ggplot2)
library(benford)


#Set working directory
setwd("//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products")

# set memory limit
memory.limit(size=40000000)


#3. Load all Deforestation and Degradation products
hansen_tl <- raster("clipped hansen lossyear.tif")

str(hansen_tl) #use structure function to examine the data type
dim(hansen_tl) #rows x columns (in terms pixels) x bands
plot(hansen_tl)

# if you want to explore what a function does, use the "?" in front of the function to get more info
?as.data.frame
?raster
?str
?dim

str(new_radd) #use structure function to examine the data type
new_radd <- raster("RADD_resample_30m.tif")
radd_date <- new_radd[[2]]
#4. Convert each into a dataframe
radd_df <- as.data.frame(new_radd)
str(radd_df)
dim(new_radd)

V_deforestation <- raster("clipped deforestation year.tif")
deforestation_df <- as.data.frame(V_deforestation)
str(deforestation_df)
dim(deforestation_df)

V_degradation <- raster("clipped degredation year.tif")
degradation_df <- as.data.frame(V_degradation)
str(degradation_df)
dim(degradation_df)

radd_df <- as.data.frame(radd_date)
# this takes a minute
str(hansen_df)
dim(hansen_df)


head(deforestation_df) # shows you the top several rows of the data frame
#a) # reports the summary statistics for each column (in this case)

# Remove 0's in df using the subset() function
hansen_df_sub <- subset(hansen_df, clipped_hansen_lossyear != 0) # != means not equal to; == means equal to
V_deforestation_sub <- subset(deforestation_df, clipped_deforestation_year != 0) # != means not equal to; == means equal to
V_degradation_sub <- subset(degradation_df, clipped_degredation_year != 0) 
radd_df_sub <- subset(radd_df, RADD_resample_30m != 0) 
str(radd_df_sub)
dim(radd_df_sub)

summary(hansen_df_sub)
dim(hansen_df_sub)
str(hansen_df_sub)

write.csv(hansen_df_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/hansen.csv")
write.csv(V_deforestation_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/v_deforestation.csv")
write.csv(V_degradation_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/v_degradation.csv")
write.csv(radd_df_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/radd_df_sub.csv")

#use these functions to check if needed
dim(hansen_df_sub)
str(hansen_df_sub)
head(hansen_df_sub)


#load subset versions of csv files
hansen_df_sub <- read.csv("hansen.csv")
deforestation_sub <- read.csv("v_deforestation.csv")
degradation_sub <- read.csv("v_degradation.csv")
radd_df_sub <- read.csv("radd_df_sub.csv")


str(deforestation_sub)
str(degradation_sub)

radd_df_sub$year = substr(radd_df_sub$RADD_resample_30m,1,2)

#Use the line of code below to transform the csv file
colnames(hansen_df_sub) <- c("X","year")
colnames(deforestation_sub) <- c("X","year")
colnames(degradation_sub) <- c("X","year")
colnames(radd_df_sub) <- c("X","Date","year")

grouped_Hansen <- hansen_df_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                  defor_km2 = n_pixels*(0.0009))

grouped_Deforestation  <- deforestation_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                              defor_km2 = n_pixels*(0.0009))


grouped_Degradation  <- degradation_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                          defor_km2 = n_pixels*(0.0009))

grouped_RADD  <- radd_df_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                          defor_km2 = n_pixels*(0.0009))

#use these lines of code and replace variables to remove, rename, or add columns as needed
deforestation_sub$clipped_deforestation_year<- NULL
v_deg$Dataset <- NULL
deforestation_sub$Year <-deforestation_sub$clipped_deforestation_year 

grouped_RADD$source      <- "RADD" 
grouped_RADD$type      <- "disturbance"
hansen$source      <- "GFC" 
hansen$type      <- "tree_loss" 
v_def$source      <- "JRC" 
v_def$type      <- "deforestation" 
v_deg$source      <- "JRC" 
v_deg$type      <- "degradation" 



#Use this line of code to make final csv files, which are used for plotting in the next step
write.csv(hansen,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/Hansen_final.csv")
write.csv(v_def,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/Deforestation_final.csv")
write.csv(v_deg,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/Degradation_final.csv")
write.csv(grouped_RADD,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/RADD_final.csv")
write.csv(combined_df,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/combined_df.csv")

#Code for individual barcharts
library(ggplot2)
hansen<- read.csv("Hansen_final.csv")
ggplot(hansen, aes(year, defor_km2)) + geom_bar(stat = "identity")

v_def<- read.csv("Deforestation_final.csv")
ggplot(v_def, aes(year, defor_km2)) + geom_bar(stat = "identity")

v_deg<- read.csv("Degradation_final.csv")
ggplot(v_deg, aes(year, defor_km2)) + geom_bar(stat = "identity")


## END GOAL ##
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# create 'grouped barchart' with total area of deforestation on the y-axis and year on the x-axis
# the groups (different color bars) will be the 3 different datasets


combined_df <- rbind(v_deg,v_def,hansen,grouped_RADD) 

combined <- read.csv("combined_df.csv")

ggplot(combined, aes(fill=type, y=defor_km2, x=year)) + list(
  geom_bar(position="dodge", stat="identity", width = 1.0),
  theme(legend.position = "bottom", legend.title = element_blank()),
  scale_fill_manual(values=c("#FF9999","#CC6666","#9999CC","#66CC99"), labels=c("JRC Deforestation", "JRC Degradation", "RADD Disturbance", "GFC Tree loss")), 
  labs(x="Year", y="Change in Tree Cover (km2)"),
  NULL
)

