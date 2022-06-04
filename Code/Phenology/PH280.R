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
PH280 <- subset(merged_dat, tree_ID=="PH280")
dim(merged_dat)
dim(PH280)

PH280$year <- str_sub(PH280$date_census,1,4)

PH280$month <- str_sub(PH280$date_census,6,7)

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
yl_PH280_sum_dat <- PH280 %>% group_by(month, year) 

# mature leaves
ml_PH280_sum_dat <- PH280 %>% group_by(month, year)

# old leaves
ol_PH280_sum_dat <- PH280 %>% group_by(month, year)

# flower bud
fb_PH280_sum_dat <- PH280 %>% group_by(month, year)

# open flower
fo_PH280_sum_dat <- PH280 %>% group_by(month, year)

# unripe fruit
fu_PH280_sum_dat <- PH280 %>% group_by(month, year)

# ripe fruit
fr_PH280_sum_dat <- PH280 %>% group_by(month, year)

##################################################################################################################
# combined chart

#find avg of pheno metrics between both years (2020 and 2021)
#young leaves
yl_PH280_avg <- yl_PH280_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(leaves_young, na.rm=T), 
            sd = mean(leaves_young),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

yl_PH280_avg

#open flowers
fo_PH280_avg <- fo_PH280_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(flower_open, na.rm=T), 
            sd = mean(flower_open),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

fo_PH280_avg

#unripe fruit
fu_PH280_avg <- fu_PH280_sum_dat %>% group_by(month) %>% 
  summarize(total_avg = mean(fruit_unripe, na.rm=T), 
            sd = mean(fruit_unripe),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd/sqrt(n_dat))))

fu_PH280_avg

#merge data sets by appending

yl_PH280_avg$pheno_metric = "young_leaf"
fo_PH280_avg$pheno_metric = "open_flower"
fu_PH280_avg$pheno_metric = "unripe_fruit"

PH280_combined = rbind(yl_PH280_avg, fo_PH280_avg, fu_PH280_avg)

PH280_combined_plot <- ggplot() +
  # geom_bar(data=PH280_combined, aes(x=month, y=total_avg * 3, fill=as.factor(pheno_metric)), 
  #          stat='identity', position=position_dodge()) +
  geom_errorbar(data=PH280_combined, aes(x=month, group=as.factor(pheno_metric), 
                                         ymin = total_avg*3-abs(sd*3), ymax = total_avg*3+abs(sd*3)), 
                width = 0.2, position=position_dodge(0.3))+
  geom_point(data=PH280_combined, aes(x=month, y=total_avg * 3, fill=as.factor(pheno_metric)), 
             pch=21, size=4, position=position_dodge(0.3)) +
  geom_line(data=PH280_combined, aes(x=month, y=total_avg * 3, group=as.factor(pheno_metric)), linetype="dashed",
            position=position_dodge(0.3)) +
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous("mean % pheno metric intensity", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs(title="Combined Phenological Change over Time in PH280 Tree", fill="year") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))

PH280_combined_plot

ggsave("analysis_plots/specific_trees/PH280_combined.pdf")
