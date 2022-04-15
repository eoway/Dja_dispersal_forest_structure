setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/ruksan_data")
getwd()
setwd("G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Sophia")

library(ggplot2)
library(stringr)
library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readr)
library(readxl)

#merged_dat = read_xlsx("merged_data/Pheno_Bouamir_Tree_merged.xlsx")
merged_dat = read_xlsx("Pheno_Bouamir_Tree_merged.xlsx")
table(merged_dat$Baka_name)
Bosiko <- subset(merged_dat, Baka_name=="Bosiko")
dim(merged_dat)
dim(Bosiko)

Bosiko$year <- str_sub(Bosiko$date_census,1,4)

Bosiko$month <- str_sub(Bosiko$date_census,6,7)

# young leaves
yl_Bosiko_sum_dat <- Bosiko %>% group_by(month, year) %>% 
  summarize(leaves_young_mean = mean(leaves_young, na.rm=T))

yl_Bosiko_sum_dat



# import total weather data
weather_total <- read_csv("24Oct2017-17Fev2022_Bouamir_Weather_data.csv")

head(weather_total)

weather_total <- weather_total[,c(1:6)]

head(weather_total)

# create year column
weather_total$year <- str_sub(weather_total$date,1,4)

head(weather_total)

# create month column
weather_total$month <- str_sub(weather_total$date,6,7)

head(weather_total)

# subset to 2020 and 2021

subset_dat <- subset(weather_total, year %in% c(2020,2021))

table(subset_dat$year)


# find mean monthly rainfall for 2020 and 2021 and total
mean_dat <- weather_total %>% group_by(month, year) %>% 
  summarize(mean_monthly = mean(rain_mm, na.rm=T),
            sd_monthly = sd(rain_mm, na.rm=T),
            n_dat = n())

head(mean_dat)

subset_dat <- subset(mean_dat, year %in% c(2020,2021))

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

?geom_smooth

mean_monthly_dat_v2

# plot young leaf change with weather data

?scale_y_continuous

# calculate confidence interval for X? (precip data)
# 95th confidence interval = the mean +- 1.960 * (standard deviation / square root of the sample size) 
mean_monthly_dat_v2$ci_lower <- mean_monthly_dat_v2$mean_monthly - (1.960 * (mean_monthly_dat_v2$sd_monthly) / sqrt(mean_monthly_dat_v2$n_dat))
mean_monthly_dat_v2$ci_upper <- mean_monthly_dat_v2$mean_monthly + (1.960 * (mean_monthly_dat_v2$sd_monthly) / sqrt(mean_monthly_dat_v2$n_dat))



ggplot() +
  geom_bar(data = yl_Bosiko_sum_dat, aes(fill=as.factor(year), y=leaves_young_mean * 2, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "red") +
  # geom_smooth(data = mean_monthly_dat, aes(x=month, y=mean_annual, group = year), linetype = 0, fill = "red")+
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = mean_monthly-ci_lower, ymax = mean_monthly+ci_upper, group=year), fill="red",alpha = 0.2) + 
  scale_y_continuous("leaves_young_mean", breaks=c(0,2,4,6,8), labels=c("0","0-25","25-50","50-75","75-100"), sec.axis = sec_axis(~ . *1, name = "rainfall_mm"))+
  labs(title="Young Leaf Change over Time in Bosiko Tree Species") + 
  theme_classic()




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
