setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/")
getwd()


library(tools)
library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

# load crown data
crowns <- readOGR(dsn="delineated_crowns.shp", layer="delineated_crowns")
phen_crowns <- subset(crowns, Point_FID != "NA") # this gets rid of crowns not associated with tree list
final_trees <- subset(crowns, Point_FID %in% c(63, 219, 203)) # these are the top 3 trees by dbh with sufficient related ground data
head(final_trees)

# load ndvi files for the same tile and convert it into raster object.
rastlist <- list.files(path = "04_files", pattern='.tif$',all.files=TRUE, full.names=FALSE)
head(rastlist)



sel_tile_to_raster <-function(rast){
  rast_tile_num <-  strsplit(rast,split="_") 
  if (rast_tile_num[[1]][2] == 3340504){
    rast <- raster(rast)
  }
}

?lapply
setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/04_files/")
tile_img_list <-lapply(rastlist, raster)

#tile_img_list= tile_img_list[-which(sapply(tile_img_list, is.null))]



new_crs <- crs(tile_img_list[[1]])
final_treesUTM <- spTransform(final_trees, CRS=new_crs)
projection(final_treesUTM) 
final_treesUTM$CID <- seq(1:length(final_trees$Point_FID))#set IDs for final_trees

len = length(tile_img_list)

num_list <-c(1:len)

analysis <-function(i){

  img <-tile_img_list[[i]]
    
  #plot(img, ext=final_treesUTM)
  #plot(final_treesUTM, border="black", add = T)

  ndvi_file <- raster::extract(img, final_treesUTM)
  crown_1 <-ndvi_file[[1]]
  crown_2 <-ndvi_file[[2]]
  crown_3 <- ndvi_file[[3]]


  mean_ndvi_crown_1 <- mean(ndvi_file[[1]], na.rm =T)
  mean_ndvi_crown_2 <- mean(ndvi_file[[2]], na.rm =T)
  mean_ndvi_crown_3 <- mean(ndvi_file[[3]], na.rm =T)
  

    
  filename <-strsplit(names(img),split="_")
  date <-filename [[1]][3]

  
  output_list = c(date, mean_ndvi_crown_1, mean_ndvi_crown_2, mean_ndvi_crown_3)
    
}   


output <-lapply(num_list, analysis)

df <- data.frame(matrix(unlist(output), nrow=length(output), byrow=TRUE))
df

#rename columns
df = rename(df, "date"="X1")
df = rename(df, "crown_63"="X2", "crown_219"="X3", "crown_203"="X4")


plot(tile_img_list[[21]], ext=final_treesUTM)
plot(final_treesUTM, border="black", add = T)

#find monthly mean
#add month column to df
df$month <- str_sub(df$date,6,7)

??str_sub

#find mean ndvi for each crown
#convert values from character to numeric
df$crown_63 = as.numeric(df$crown_63)
df$crown_219 = as.numeric(df$crown_219)
df$crown_203 = as.numeric(df$crown_203)

ndvi_mean_dat <- df %>% group_by(month) %>% 
  summarize(mean_63 = mean(crown_63, na.rm=T),
            mean_219 = mean(crown_219, na.rm=T),
            mean_203 = mean(crown_203, na.rm=T),
            n_dat = n())


### import weather data ###
setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/")

weather_total <- read_csv("ruksan_data/weather_data/24Oct2017-17Fev2022_Bouamir_Weather_data.csv")
?read.csv
head(weather_total)

weather_total <- weather_total[,c(1:6)]

head(weather_total)

# create year column
weather_total$year <- str_sub(weather_total$date,1,4)

head(weather_total)

# create month column
weather_total$month <- str_sub(weather_total$date,6,7)

head(weather_total)

# subset to 2021
subset_dat <- subset(weather_total, year %in% c(2021))
table(subset_dat$year)


# find mean monthly rainfall for 2021 and total
mean_dat <- weather_total %>% group_by(month, year) %>% 
  summarize(mean_monthly = mean(rain_mm, na.rm=T),
            sd_monthly = sd(rain_mm, na.rm=T),
            n_dat = n())

head(mean_dat)

subset_dat <- subset(mean_dat, year %in% c(2021))

table(subset_dat$year)

head(mean_dat)

mean_monthly_dat <- mean_dat %>% group_by(month) %>% 
  summarize(mean_annual = mean(mean_monthly, na.rm=T))

mean_monthly_dat_v2 <- weather_total %>% group_by(month) %>% 
  summarize(mean_monthly = mean(rain_mm, na.rm=T),
            sd_monthly = sd(rain_mm, na.rm=T),
            n_dat = n())


head(mean_monthly_dat_v2)
mean_monthly_dat_v2$year <- rep("2017-2022", length(mean_monthly_dat_v2$month))
head(mean_monthly_dat_v2)

mean_monthly_dat_v2

# calculate confidence interval for X? (precip data)
# 95th confidence interval = the mean +- 1.960 * (standard deviation / square root of the sample size) 
mean_monthly_dat_v2$ci_lower <- mean_monthly_dat_v2$mean_monthly - 
  (1.960 * (mean_monthly_dat_v2$sd_monthly) / sqrt(mean_monthly_dat_v2$n_dat))
mean_monthly_dat_v2$ci_upper <- mean_monthly_dat_v2$mean_monthly + 
  (1.960 * (mean_monthly_dat_v2$sd_monthly) / sqrt(mean_monthly_dat_v2$n_dat))
head(mean_monthly_dat_v2)

##----------------------------------------------Plot--Data--Below------------------------------------------------------------------------------

ndvi_mean_dat_long <- gather(ndvi_mean_dat, "species", "ndvi", -c(month, n_dat))



ndvi_plot <- ggplot() +
  geom_point(data=ndvi_mean_dat_long, aes(x=month, y=ndvi * 12, color=species),
             size=4) +
  geom_line(data=ndvi_mean_dat_long, aes(x=month, y=ndvi * 12, color = species, group = species), linetype="dashed") +
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) +
  geom_hline(yintercept = 0) + 
  scale_y_continuous("mean NDVI", breaks=c(0,1.2,2.4,3.6,4.8,6,7.2,8.4, 9.6, 10.8, 12), labels=c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7", "0.8", "0.9","1"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs(title="NDVI Over Time in 3 Different Tree Species") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_color_manual('Tree Species', values = c("#268E81", "#B04C01", "#5636A0"), labels = c("Mabe", "Sene", "Etenge")) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=16), 
        plot.title=element_text(size=18), 
        legend.text=element_text(size=16),
        legend.title=element_text(size=18))


ndvi_plot

# ndvi <- ggplot() +
#   geom_point(data=ndvi_mean_dat, aes(x=month, y=ndvi * 12, color=species),
#              size=4) +
#   #geom_point(data=ndvi_mean_dat, aes(x=month, y=mean_219 * 12, color="#d95f02"),
#              #size=4) +
#   #geom_point(data=ndvi_mean_dat, aes(x=month, y=mean_203 * 12, color="#7570b3"),
#              #size=4) +
#   geom_line(data=ndvi_mean_dat, aes(x=month, y=mean_63 * 12, group=1), linetype="dashed") +
#   geom_line(data=ndvi_mean_dat, aes(x=month, y=mean_219 * 12, group=1), linetype="dashed") +
#   geom_line(data=ndvi_mean_dat, aes(x=month, y=mean_203 * 12, group=1), linetype="dashed") + 
#   geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
#   geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
#               fill="#1f78b4",alpha = 0.2) +
#   geom_hline(yintercept = 0) + 
#   scale_y_continuous("mean NDVI", breaks=c(0,1.2,2.4,3.6,4.8,6,7.2,8.4, 9.6, 10.8, 12), labels=c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7", "0.8", "0.9","1"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
#   scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
#   labs(title="NDVI Over Time in 3 Different Tree Species") +
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
#   scale_fill_manual('Tree Species', labels = c("Mabe", "Sene", "Etenge")) +
#   theme(axis.text=element_text(size=12), 
#         axis.title=element_text(size=15), 
#         plot.title=element_text(size=15), 
#         legend.text=element_text(size=12),
#         legend.title=element_text(size=12))
#   
#   
# 
# 
# ndvi

ggsave("ndvi/top_3.pdf")
  




