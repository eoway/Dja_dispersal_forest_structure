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
head(merged_dat)
PH003 <- subset(merged_dat, tree_ID=="PH003")
dim(merged_dat)
dim(PH003)

PH003$year <- str_sub(PH003$date_census,1,4)

PH003$month <- str_sub(PH003$date_census,6,7)

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

# young leaves
yl_PH003_sum_dat <- PH003 %>% group_by(month, year) 

yl_PH003_sum_dat

PH003_new_leaf <- ggplot() +
  geom_bar(data = yl_PH003_sum_dat, aes(fill=as.factor(year), y=leaves_young * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  # geom_smooth(data = mean_monthly_dat, aes(x=month, y=mean_annual, group = year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % young leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Young Leaf Change over Time in PH003 Tree Species", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_new_leaf

ggsave("analysis_plots/PH003/PH003_new_leaf.pdf")

# mature leaves
ml_PH003_sum_dat <- PH003 %>% group_by(month, year)
ml_PH003_sum_dat

PH003_mature_leaf <- ggplot() +
  geom_bar(data = ml_PH003_sum_dat, aes(fill=as.factor(year), y=leaves_mature * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  # geom_smooth(data = mean_monthly_dat, aes(x=month, y=mean_annual, group = year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% mature leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Mature Leaf Change over Time in  Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_mature_leaf

ggsave("analysis_plots/PH003/PH003_mature_leaf.pdf")

# old leaves
ol_PH003_sum_dat <- PH003 %>% group_by(month, year)
ol_PH003_sum_dat

PH003_old_leaf <- ggplot() +
  geom_bar(data = ol_PH003_sum_dat, aes(fill=as.factor(year), y=leaves_old * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  # geom_smooth(data = mean_monthly_dat, aes(x=month, y=mean_annual, group = year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% old leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Old Leaf Change over Time in  Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_old_leaf

ggsave("analysis_plots/PH003/PH003_old_leaf.pdf")

# flower bud
fb_PH003_sum_dat <- PH003 %>% group_by(month, year)
fb_PH003_sum_dat

PH003_flower_bud <- ggplot() +
  geom_bar(data = fb_PH003_sum_dat, aes(fill=as.factor(year), y=flower_bud * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% flower buds", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Flower Bud Change over Time in Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_flower_bud
ggsave("analysis_plots/PH003/PH003_flower_bud.pdf")

# open flower
fo_PH003_sum_dat <- PH003 %>% group_by(month, year)
fo_PH003_sum_dat

PH003_flower_open <- ggplot() +
  geom_bar(data = fo_PH003_sum_dat, aes(fill=as.factor(year), y=flower_open * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% open flower ", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Open Flower  Change over Time in Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_flower_open
ggsave("analysis_plots/PH003/PH003_flower_open.pdf")


# unripe fruit
fu_PH003_sum_dat <- PH003 %>% group_by(month, year)
fu_PH003_sum_dat

PH003_fruit_unripe <- ggplot() +
  geom_bar(data = fu_PH003_sum_dat, aes(fill=as.factor(year), y=fruit_unripe * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% unripe fruit ", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Unripe Fruit Change over Time in Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_fruit_unripe
ggsave("analysis_plots/PH003/PH003_fruit_unripe.pdf")

# ripe fruit
fr_PH003_sum_dat <- PH003 %>% group_by(month, year)
fr_PH003_sum_dat

PH003_fruit_ripe <- ggplot() +
  geom_bar(data = fr_PH003_sum_dat, aes(fill=as.factor(year), y=fruit_ripe * 3, x=month), 
           position="dodge", stat="identity") + 
  geom_line(data=subset_dat, aes(x=month, y=mean_monthly, group = year, linetype = year)) + 
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("% ripe fruit ", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Ripe Fruit Change over Time in Tree PH003", fill="year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

PH003_fruit_ripe
ggsave("analysis_plots/PH003/PH003_fruit_ripe.pdf")


##################################################################################################################
# combined chart

#find avg of pheno metrics between both years (2020 and 2021)
#young leaves
yl_PH003_avg <- yl_PH003_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(leaves_young, na.rm=T), 
            sd = mean(leaves_young),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

yl_PH003_avg

#open flowers
fo_PH003_avg <- fo_PH003_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(flower_open, na.rm=T), 
            sd = mean(flower_open),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

fo_PH003_avg

#unripe fruit
fu_PH003_avg <- fu_PH003_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(fruit_unripe, na.rm=T), 
            sd = mean(fruit_unripe),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

fu_PH003_avg

#merge data sets by appending

yl_PH003_avg$pheno_metric = "young_leaf"
fo_PH003_avg$pheno_metric = "open_flower"
fu_PH003_avg$pheno_metric = "unripe_fruit"

PH003_combined = rbind(yl_PH003_avg, fo_PH003_avg, fu_PH003_avg)

PH003_combined_plot <- ggplot() +
  # geom_bar(data=PH003_combined, aes(x=month, y=total_avg * 3, fill=as.factor(pheno_metric)), 
  #          stat='identity', position=position_dodge()) +
  geom_errorbar(data=PH003_combined, aes(x=month, group=as.factor(pheno_metric), 
                                         ymin = total_avg*3-abs(sd*3), ymax = total_avg*3+abs(sd*3)), 
                width = 0.2, position=position_dodge(0.3))+
  geom_point(data=PH003_combined, aes(x=month, y=total_avg * 3, fill=as.factor(pheno_metric)), 
             pch=21, size=4, position=position_dodge(0.3)) +
  geom_line(data=PH003_combined, aes(x=month, y=total_avg * 3, group=as.factor(pheno_metric)), linetype="dashed",
            position=position_dodge(0.3)) +
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous("mean % pheno metric intensity", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs(title="Combined Phenological Change over Time in PH003 Tree", fill="year") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))

PH003_combined_plot

ggsave("analysis_plots/specific_trees/PH003_combined.pdf")
