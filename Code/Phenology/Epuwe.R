setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/ruksan_data")
getwd()

library(ggplot2)
library(stringr)
library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readr)
library(readxl)

merged_dat = read_xlsx("merged_data/Pheno_Bouamir_Tree_merged.xlsx")
table(merged_dat$Baka_name)
Epuwe <- subset(merged_dat, Baka_name=="Epuwe")
dim(merged_dat)
dim(Epuwe)

head(Epuwe)


Epuwe$year <- str_sub(Epuwe$date_census,1,4)

Epuwe$month <- str_sub(Epuwe$date_census,6,7)

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

# mean_monthly_dat <- mean_dat %>% group_by(month) %>% 
#  summarize(mean_annual = mean(mean_monthly, na.rm=T))

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

# plot pheno change with weather data

##################################################################################################################
# young leaves (find mean and sd)
yl_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(leaves_young_mean = mean(leaves_young, na.rm=T), leaves_young_sd = sd(leaves_young, na.rm=T))


Epuwe_new_leaf <- ggplot() +
  geom_bar(data = yl_Epuwe_sum_dat, aes(fill=as.factor(year), y=leaves_young_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  # geom_smooth(data = mean_monthly_dat, aes(x=month, y=mean_annual, group = year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % young leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Young Leaf Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_new_leaf

ggsave("analysis_plots/Epuwe/Epuwe_new_leaf.pdf")

##################################################################################################################
# mature leaves
head(Epuwe)

ml_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(leaves_mature_mean = mean(leaves_mature, na.rm=T), leaves_mature_sd = sd(leaves_mature, na.rm=T))
ml_Epuwe_sum_dat

Epuwe_mature_leaf <- ggplot() +
  geom_bar(data = ml_Epuwe_sum_dat, aes(fill=as.factor(year), y=leaves_mature_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % mature leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Mature Leaf Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_mature_leaf

ggsave("analysis_plots/Epuwe/Epuwe_mature_leaf.pdf")


##################################################################################################################
# old leaves
ol_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(leaves_old_mean = mean(leaves_old, na.rm=T), leaves_old_sd = sd(leaves_old, na.rm=T))
ol_Epuwe_sum_dat

Epuwe_old_leaf <- ggplot() +
  geom_bar(data = ol_Epuwe_sum_dat, aes(fill=as.factor(year), y=leaves_old_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % old leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Old Leaf Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_old_leaf
ggsave("analysis_plots/Epuwe/Epuwe_old_leaf.pdf")

##################################################################################################################

# flower bud
fb_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(flower_bud_mean = mean(flower_bud, na.rm=T), flower_bud_sd = sd(flower_bud, na.rm=T))

fb_Epuwe_sum_dat

Epuwe_flower_bud <- ggplot() +
  geom_bar(data = fb_Epuwe_sum_dat, aes(fill=as.factor(year), y=flower_bud_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % flower buds", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Flower Bud Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_flower_bud
ggsave("analysis_plots/Epuwe/Epuwe_flower_bud.pdf")

##################################################################################################################

# open flower
fo_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(flower_open_mean = mean(flower_open, na.rm=T), flower_open_sd = sd(flower_open, na.rm=T))

fo_Epuwe_sum_dat

Epuwe_flower_open <- ggplot() +
  geom_bar(data = fo_Epuwe_sum_dat, aes(fill=as.factor(year), y=flower_open_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % open flower", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Open Flower Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_flower_open
ggsave("analysis_plots/Epuwe/Epuwe_flower_open.pdf")

##################################################################################################################

# unripe fruit
fu_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(fruit_unripe_mean = mean(fruit_unripe, na.rm=T), fruit_unripe_sd = sd(fruit_unripe, na.rm=T))

fu_Epuwe_sum_dat

Epuwe_fruit_unripe <- ggplot() +
  geom_bar(data = fu_Epuwe_sum_dat, aes(fill=as.factor(year), y=fruit_unripe_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % unripe fruit", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Unripe Fruit Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_fruit_unripe
ggsave("analysis_plots/Epuwe/Epuwe_fruit_unripe.pdf")

##################################################################################################################

# ripe fruit
fr_Epuwe_sum_dat <- Epuwe %>% group_by(month, year) %>% 
  summarize(fruit_ripe_mean = mean(fruit_ripe, na.rm=T), fruit_ripe_sd = sd(fruit_ripe, na.rm=T))

fr_Epuwe_sum_dat

Epuwe_fruit_ripe <- ggplot() +
  geom_bar(data = fr_Epuwe_sum_dat, aes(fill=as.factor(year), y=fruit_ripe_mean * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % ripe fruit", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Ripe Fruit Change over Time in Epuwe Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Epuwe_fruit_ripe
ggsave("analysis_plots/Epuwe/Epuwe_fruit_ripe.pdf")




##################################################################################################################
# combined chart

#find avg of pheno metrics between both years (2020 and 2021)
#young leaves
yl_Epuwe_avg <- yl_Epuwe_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(leaves_young_mean, na.rm=T), sd_avg = mean(leaves_young_sd)) 

yl_Epuwe_avg

#open flowers
fo_Epuwe_avg <- fo_Epuwe_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(flower_open_mean, na.rm=T), sd_avg = mean(flower_open_sd)) 

fo_Epuwe_avg

#unripe fruit
fu_Epuwe_avg <- fu_Epuwe_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(fruit_unripe_mean, na.rm=T), sd_avg = mean(fruit_unripe_sd)) 

fu_Epuwe_avg

#merge data sets by appending

yl_Epuwe_avg$pheno_metric = "young_leaf"
fo_Epuwe_avg$pheno_metric = "open_flower"
fu_Epuwe_avg$pheno_metric = "unripe_fruit"

Epuwe_combined = rbind(yl_Epuwe_avg, fo_Epuwe_avg, fu_Epuwe_avg)


#plot (still need error bars)
Epuwe_combined_plot <- ggplot() +
  geom_bar(data=Epuwe_combined, aes(fill=as.factor(pheno_metric), y=total_avg * 3, x=month), 
           position='dodge', stat='identity') +
  #geom_errorbar(data = Epuwe_combined, aes(x = month, ymin = total_avg-sd_avg, ymax = total_avg+sd_avg), width = .2 , position = "dodge")+
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % pheno metric intensity", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs(title="Combined Phenological Change over Time in Epuwe Tree Species", fill="year") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))

Epuwe_combined_plot


