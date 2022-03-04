setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/ruksan_data")
getwd()

library(ggplot2)
library(readxl)
library(stringr)
library(dplyr)

merged_dat = read_xlsx("merged_data/Pheno_Bouamir_Tree_merged.xlsx")
table(merged_dat$Baka_name)
Bosiko <- subset(merged_dat, Baka_name=="Bosiko")
dim(merged_dat)
dim(Bosiko)

Bosiko$year <- str_sub(Bosiko$date_census,1,4)

Bosiko$month <- str_sub(Bosiko$date_census,6,7)

# young leaves
yl_Bosiko_sum_dat <- Bosiko %>% group_by(month, year) %>% summarize(leaves_young_mean = mean(leaves_young, na.rm=T))
yl_Bosiko_sum_dat



# import total weather data
weather_total <- read_csv("24Oct2017-17Fev2022_Bouamir_Weather_data.csv")

weather_total <- weather_total[,c(1,2,3,4,5,6)]

# create year column
weather_total$year <- str_sub(weather_total$date,1,4)

head(weather_total)

# create month column
weather_total$month <- str_sub(weather_total$date,6,7)

head(weather_total)

# subset to 2020 and 2021
subset_2020 <- subset(weather_total, year==2020)

subset_2021 <- subset(weather_total, year==2021)


# find mean monthly rainfall for 2020 and 2021
mean_2020 <- subset_2020 %>% group_by(month, year) %>% 
  summarize(mean_monthly_2020 = mean(rain_mm, na.rm=T))

mean_2021 <- subset_2021 %>% group_by(month, year) %>% 
  summarize(mean_monthly_2021 = mean(rain_mm, na.rm=T))


# plot young leaf change with weather data
ggplot(data = yl_Bosiko_sum_dat, aes(fill=as.factor(year), y=leaves_young_mean*3.333, x=month)) +
  geom_bar(position="dodge", stat="identity") + 
  geom_line(data=mean_2020, aes(x=month, y=mean_monthly_2020, group =1))+ 
  geom_line(linetype = "dashed", data=mean_2021, aes(x=month, y=mean_monthly_2021, group =1))+ 
  scale_y_continuous("rainfall_mm", sec.axis = sec_axis(~ . *0.3, name = "leaves_young_mean"))+
  labs(title="Young Leaf Change over Time in Etenge Tree Species")




ggsave("analysis_plots/Bosiko/Bosiko_new_leaf.pdf")


# mature leaves
head(Bosiko)

ml_Bosiko_sum_dat <- Bosiko %>% group_by(month, year) %>% summarize(leaves_mature_mean = mean(leaves_mature, na.rm=T))
ml_Bosiko_sum_dat

ggplot(data = ml_Bosiko_sum_dat, aes(fill=as.factor(year), y=leaves_mature_mean, x=month)) + geom_bar(position="dodge", stat="identity") + labs(title="Mature Leaf Change over Time in Bosiko Tree Species")
ggsave("analysis_plots/Bosiko/Bosiko_mature_leaf.pdf")

# old leaves
ol_Bosiko_sum_dat <- Bosiko %>% group_by(month, year) %>% summarize(leaves_old_mean = mean(leaves_old, na.rm=T))
ol_Bosiko_sum_dat

ggplot(data = ol_Bosiko_sum_dat, aes(fill=as.factor(year), y=leaves_old_mean, x=month)) + geom_bar(position="dodge", stat="identity") + labs(title="Old Leaf Change over Time in Bosiko Tree Species")
ggsave("analysis_plots/Bosiko/Bosiko_old_leaf.pdf")
