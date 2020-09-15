##############################
############Setup#############
##############################

#package imports
library("tidyverse")
library("data.table")
library("stargazer")
library("here")
library("vtable")
library(ggplot2)


#set working directory and read in dataset as csv
# setwd("C:/Users/prani/Dropbox (MIT)/2020-2021/14.33/Short Project/1433-short-project")
df <- read.csv("cleaned_OI_state_data.csv")


#################################
############Cleaning#############
#################################

#change date column class from factor to date
date_cols <- c("date", "close_nonessential_bus", "begin_reopen", "mandate_mask",  "gov_inaug_date", "begin_shelter_in_place")
df[, date_cols] <- lapply(df[, date_cols], function(x) as.Date(x, format = "%m/%d/%y"))

#businesses closed dummy
df$bus_close_D <- (df$date > df$close_nonessential_bus) & (df$date < df$begin_reopen)

#mask mandate in effect dummy
df$mm_D <- (df$date > df$mandate_mask)

#shelter in place in effect dummy
df$sip_D <- (df$date > df$mandate_mask)


####################################
############Exploratory#############
####################################

#scatterplot for each sate with spend_aer (dependent, blue), gps_away_from_home (independent, black), spend_all (control, red)
#include vertical lines for dates of non-pharmaceutical interventions
ggplot(data = df, aes(x = date)) +
  facet_wrap(~state_abbrev) + 
  geom_point(aes(y = spend_aer), color = 'blue', size = 0.1) +
  geom_point(aes(y = gps_away_from_home), size = 0.1) +
  geom_point(aes(y = spend_all), color = 'red', size = 0.1) +
  geom_vline(aes(xintercept = close_nonessential_bus)) +
  geom_vline(aes(xintercept = begin_shelter_in_place), color = 'red') +
  geom_vline(aes(xintercept = begin_reopen), color = 'green') +
  geom_vline(aes(xintercept = mandate_mask), color = 'blue') +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  



