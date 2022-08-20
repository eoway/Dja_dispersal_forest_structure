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
setwd("//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads")

# set memory limit
memory.limit(size=40000000)


#3. Load all Deforestation and Degradation products
GFC <- raster("GFC_2001_2021_clipped.tif")
GFCdf <- as.data.frame(GFC)
head(GFC)

str(GFC) #use structure function to examine the data type
dim(GFC) #rows x columns (in terms pixels) x bands
plot(GFC)

# if you want to explore what a function does, use the "?" in front of the function to get more info
?as.data.frame
?raster
?str
?dim

str(new_radd) #use structure function to examine the data type
radd <- raster("RADD_clipped_dates_res.tif")
radd_date <- new_radd[[2]]
#4. Convert each into a dataframe
radd_df <- as.data.frame(radd)
str(radd_df)
dim(new_radd)

JRC_def <- raster("JRC_Def_1982_2021_clipped.tif")
JRC_def_df <- as.data.frame(JRC_def)
str(JRC_def_df)
dim(JRC_def_df)

JRC_deg <- raster("JRC_Deg_1982_2021_clipped.tif")
JRC_deg_df <- as.data.frame(JRC_deg)
str(JRC_deg_df)
dim(JRC_deg_df)
    

# Remove 0's in df using the subset() function
GFC_df_sub <- subset(GFCdf, GFC_2001_2021_clipped != 0) # != means not equal to; == means equal to
JRC_def_sub <- subset(JRC_def_df, JRC_Def_1982_2021_clipped != 0) # != means not equal to; == means equal to
JRC_deg_sub <- subset(JRC_deg_df, JRC_Deg_1982_2021_clipped != 0) 
radd_df_sub <- subset(radd_df, RADD_clipped_dates_res != 0) 
str(radd_df_sub)
dim(radd_df_sub)

summary(GFC_df_sub)
dim(GFC_df_sub)
str(GFC_df_sub)
 
write.csv(GFC_df_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/GFC.csv")
write.csv(JRC_def_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/JRCdef21.csv")
write.csv(JRC_deg_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/JRCdeg21.csv")
write.csv(radd_df_sub,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/radd_df_sub.csv")



#load subset versions of csv files
GFC_df_sub <- read.csv("GFC.csv")
JRC_def_sub <- read.csv("JRCdef21.csv")
JRC_deg_sub <- read.csv("JRCdeg21.csv")
radd_df_sub <- read.csv("radd_df_sub.csv")


str(JRC_def_sub)
str(JRC_deg_sub)


#Use the line of code below to transform the csv file
colnames(GFC_df_sub) <- c("X","year")
colnames(JRC_def_sub) <- c("X","year")
colnames(JRC_deg_sub) <- c("X","year")
colnames(radd_df_sub) <- c("X","year")

radd_df_sub$year = substr(radd_df_sub$year,1,2)

grouped_GFC <- GFC_df_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                  defor_km2 = n_pixels*(0.0009))

grouped_Def  <- JRC_def_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                  defor_km2 = n_pixels*(0.0009))


grouped_Deg  <- JRC_deg_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                                   defor_km2 = n_pixels*(0.0009))

grouped_RADD  <- radd_df_sub  %>% group_by(year) %>% summarize(n_pixels=n(),
                                                               defor_km2 = n_pixels*(0.0009))

#use these lines of code and replace variables to remove, rename, or add columns as needed
JRC_def_sub$clipped_deforestation_year<- NULL
v_deg$Dataset <- NULL
JRC_def_sub$Year <-JRC_def_sub$clipped_deforestation_year 

grouped_RADD$source      <- "RADD" 
grouped_RADD$type      <- "disturbance"
grouped_GFC$source      <- "GFC" 
grouped_GFC$type      <- "tree_loss" 
grouped_Def$source      <- "JRC" 
grouped_Def$type      <- "deforestation" 
grouped_Deg$source      <- "JRC" 
grouped_Deg$type      <- "degradation" 



#Use this line of code to make final csv files, which are used for plotting in the next step



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
grouped_RADD$year <- sub("^", "20", grouped_RADD$year )
grouped_RADD$year <- as.numeric(as.character(grouped_RADD$year))

grouped_GFC$year[grouped_GFC$year=="1"]<-"2001"
grouped_GFC$year[grouped_GFC$year=="2"]<-"2002"
grouped_GFC$year[grouped_GFC$year=="3"]<-"2003"
grouped_GFC$year[grouped_GFC$year=="4"]<-"2004"
grouped_GFC$year[grouped_GFC$year=="5"]<-"2005"
grouped_GFC$year[grouped_GFC$year=="6"]<-"2006"
grouped_GFC$year[grouped_GFC$year=="7"]<-"2007"
grouped_GFC$year[grouped_GFC$year=="8"]<-"2008"
grouped_GFC$year[grouped_GFC$year=="9"]<-"2009"
grouped_GFC$year[grouped_GFC$year=="10"]<-"2010"
grouped_GFC$year[grouped_GFC$year=="11"]<-"2011"
grouped_GFC$year[grouped_GFC$year=="12"]<-"2012"
grouped_GFC$year[grouped_GFC$year=="13"]<-"2013"
grouped_GFC$year[grouped_GFC$year=="14"]<-"2014"
grouped_GFC$year[grouped_GFC$year=="15"]<-"2015"
grouped_GFC$year[grouped_GFC$year=="16"]<-"2016"
grouped_GFC$year[grouped_GFC$year=="17"]<-"2017"
grouped_GFC$year[grouped_GFC$year=="18"]<-"2018"
grouped_GFC$year[grouped_GFC$year=="19"]<-"2019"
grouped_GFC$year[grouped_GFC$year=="20"]<-"2020"
grouped_GFC$year[grouped_GFC$year=="21"]<-"2021"

grouped_GFC$year <- as.numeric(as.character(grouped_GFC$year))

exclude <- c(1984:2000)

library(dplyr)
grouped_Defx <- grouped_Def %>%
  filter(!(year %in% exclude))

grouped_Degx <- grouped_Deg %>%
  filter(!(year %in% exclude))

grouped_RADD <- grouped_RADD %>% add_row(year = 2001:2018, n_pixels = 0,   
                         defor_km2 = 0,   
                         source = "RADD",    
                         type = "disturbance")  

combined_df <- rbind(grouped_Defx,grouped_Degx ,grouped_RADD, grouped_GFC) 

write.csv(combined_df,"//slcsvr3.nslc.ucla.edu/Students/kdutko2001/Downloads/combined_dfNEW.csv")

combined <- read.csv("combined_df.csv")

combined_df<-combined_df[!(combined_df$n_pixels== 268236),]

ggplot(combined_df, aes(fill=type, y=defor_km2, x=year)) + list(
  geom_bar(position="dodge", stat="identity", width = 1.0),
  theme(legend.position = "bottom", legend.title = element_blank()),
  scale_fill_manual(values=c("#FF9999","#CC6666","#9999CC","#66CC99"), labels=c("JRC Deforestation", "JRC Degradation", "RADD Disturbance", "GFC Tree loss")), 
  labs(x="Year", y="Change in Tree Cover (km2)"),
  NULL
)

ggplot(combined_df, aes(fill=type, y=defor_km2, x=year)) + list(
  geom_bar(position="dodge", stat="identity", width = 1), theme_classic(),
  theme(legend.position = "bottom", legend.title = element_blank()),
  scale_fill_manual(values=c("#FF9999","#CC6666","#9999FF","#66CC99"), labels=c("JRC Deforestation", "JRC Degradation", "RADD Disturbance", "GFC Tree Loss")), 
  labs(x="Year", y="Change in Tree Cover (sq km)"),
  NULL
)
