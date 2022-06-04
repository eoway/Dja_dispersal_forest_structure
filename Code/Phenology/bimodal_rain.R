setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/")
getwd

library(tools)
library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

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


# plot
rainfall <- ggplot() +
  geom_line(data=mean_monthly_dat_v2, aes(x=month, y=mean_monthly, group = year), color = "#1f78b4") +
  geom_ribbon(data = mean_monthly_dat_v2, aes(x=month, ymin = ci_lower, ymax = ci_upper, group=year), 
              fill="#1f78b4",alpha = 0.2) +
  geom_hline(yintercept = 0) + 
  # scale_y_continuous("mean NDVI", breaks=c(0,1.2,2.4,3.6,4.8,6,7.2,8.4, 9.6, 10.8, 12), labels=c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7", "0.8", "0.9","1"), 
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs(title="Bimodal Weather Pattern in Dja Reserve") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ylab("rainfall (mm)") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=15), 
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))

rainfall

ggsave("bimodal_rain.pdf")

