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
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(devtools)
library(ggplot2)


? %>% 

library(tidyverse)

examp_df <- as.data.frame(cbind(c(1,2,3,4,5),c(2000,2000,2001,2003,2003)))

colnames(examp_df) <- c("X","year")


sum_df <- examp_df  %>% group_by(year) %>% summarize(n_pixels_per_year=n(),
                                                    defor_km2 = n_pixels_per_year*(900/1000))
ggplot2
?read.csv
#2. Set working directory
#getwd() #check the default working directory
# then set your working directory to the path location where your data are saved
setwd("//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products")
memory.limit(size=40000)

barplot(height=data$value, names=data$name, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="categories", 
        ylab="values", 
        main="My title", 
        ylim=c(0,40)
)
#3. Load all Deforestation and Degradation products
hansen_tl <- raster("clipped hansen lossyear.tif")

str(hansen_tl) #use structure function to examine the data type
dim(hansen_tl) #rows x columns (in terms pixels) x bands
plot(hansen_tl)

# if you want to explore what a function does, use the "?" in front of the function to get more info
?as.data.frame
?raster
?str

radd <- brick("RADD_NASA_Biodiv_clipped.tif")
#radd <- raster("RADD.tif")
radd_date <- radd$Date
radd_date <- radd[[2]]
str(radd_date) #use structure function to examine the data type


#4. Convert each into a dataframe
hansen_df <- as.data.frame(hansen_tl)
str(hansen_df)
dim(hansen_df)

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

summary(hansen_df_sub)
dim(hansen_df_sub)
str(hansen_df_sub)

write.csv(new_df_samp,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/new_df_samp.csv")
write.csv(V_deforestation_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/v_deforestation.csv")
write.csv(V_degradation_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/v_degradation.csv")

hansen_df_sub <- read.csv("hansen.csv")

# dim(hansen_df_sub)
dim(hansen_df_sub)
str(hansen_df_sub)
# head(hansen_df_sub)
head(hansen_df_sub)


deforestation_sub <- read.csv("v_deforestation.csv")
degradation_sub <- read.csv("v_degradation.csv")

str(deforestation_sub)
str(degradation_sub)
# Draw a random sample of 1000 rows from new_df to write the rest of your code
# Basically create a smaller version of the orginal dataset 
new_df_samp <- new_df[sample(nrow(new_df), 1000), ]
dim(deforestation_sub)
summary(new_df_samp)

par(mfrow=c(1,2))

hist(hansen_df_sub$clipped_hansen_lossyear)

deforestation_sub$Deforestation_total <- deforestation_sub$X*0.03
deforestation_sub$clipped_deforestation_year<- NULL
deforestation_sub$Year <-deforestation_sub$clipped_deforestation_year 

new_df_samp$clipped_hansen_lossyear <- NULL
deforestation_sub$X <- NULL

deforestation_sub$Dataset      <- "vancutsem" 

# When the code is functioning, you can replace the new_df_samp dataframe with new_df 

write.csv(deforestation_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/NASA products/deforestation_sub.csv")

library(ggplot2)
hansen<- read.csv("hansen_df_sub.csv")
ggplot(hansen, aes(Year, Deforestation_total)) + geom_bar(stat = "identity")

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
