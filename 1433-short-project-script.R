##############################
############Setup#############
##############################

#package imports
library("tidyverse")
library("data.table")
library("stargazer")
library("here")
library("vtable")
library("ggplot2")
library("AER")
library("MASS")
library('plm')
library('dplyr')
library('tidyselect')

#set working directory and read in dataset csv as dataframe
setwd("C:/Users/prani/Dropbox (MIT)/2020-2021/14.33/Short Project/1433-short-project")
df <- read.csv("cleaned_OI_state_data.csv")

#read in shelter in place/stay at home order end date csv as dataframe
end_shelter <- read.csv("end_shelter.csv")

#################################
############Cleaning#############
#################################
#merge shelter in place/stay at home order end date with primary dataset on statefips
#keep all observations in primary dataset
df <- merge(df, end_shelter, by.x = 'statefips', by.y = 'ï..statefips', all.x = TRUE, all.y = FALSE)


#change date column class from factor to date
date_cols <- c("date", "close_nonessential_bus", "begin_reopen", "mandate_mask",  "gov_inaug_date", "begin_shelter_in_place", "end_shelter_in_place")
df[, date_cols] <- lapply(df[, date_cols], function(x) as.Date(x, format = "%m/%d/%y"))

#generate businesses closed dummy
#If only start(end) date reported, assume policy in effect through the end(start) of sample
df$bus_close_D <- ((df$date >= df$close_nonessential_bus) & (df$date <= df$begin_reopen)) | (is.na(df$close_nonessential_bus) & (df$date <= df$begin_reopen)) | (is.na(df$begin_reopen) & (df$date >= df$close_nonessential_bus))

#generate shelter in place in effect dummy
#If only start(end) date reported, assume policy in effect through the end(start) of sample
df$sip_D <- ((df$date >= df$begin_shelter_in_place) & (df$date <= df$end_shelter_in_place)) | (is.na(df$end_shelter_in_place) & (df$date >= df$begin_shelter_in_place)) | (is.na(df$begin_shelter_in_place) & (df$date <= df$end_shelter_in_place))

#Set NA values of policy in effect dummies to False
#If no start or end date, we can assume the policy was not in effect for a given state.
df[c("bus_close_D", "sip_D")][is.na(df[c("bus_close_D", "sip_D")])] <- FALSE


#############################################
############Exploratory Analysis#############
#############################################

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
  geom_vline(aes(xintercept = end_shelter_in_place), color = 'blue') +
  xlab('') +
  ylab('Change Relative to January Baseline') 
ggsave("f1.png")

########################################
############OLS Regressions#############
########################################

#Regression 1: Discretionary spending on time away from home
m1 <- lm(formula = spend_aer ~ gps_away_from_home, data = df)

#Regression 2: Add control for total spending
m2 <- lm(formula = spend_aer ~ gps_away_from_home + spend_all, data = df)

#Regression 3: Add controls for all small businesses open, leisure/hospitality small businesses open
m3 <- lm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70, data = df)

#Regression 4: Add control for new COVID death rate
m4 <- lm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate, data = df)


###############################################################
############Panel Data (Fixed Effects) Regressions#############
###############################################################

#convert dataset to panel data format with state as entity, date as time variable
p_df = pdata.frame(df, index = c("state", "date"))

#Regression 5: State fixed effects
m5 <- plm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate, data = p_df, model = 'within')

#Regression 6: State and time fixed effects
m6 <- plm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate, data = p_df, model = 'within', effect = "twoways")

#Open txt file for test results
sink('tests.txt')

#F test for state fixed effects 
pFtest(m5,m4)

#F test for time fixed effects
pFtest(m6,m4)

#Close txt file
sink()

#Table 1: Output tables for regressions 1-6 to tex file
stargazer(m1,m2,m3,m4,m5,m6, 
          title = "Table 1: OLS/Fixed Effects Regressions of Discretionary Spending on Time Away from Home",
          covariate.labels = c("Time Away from Home", "Total Spending", "Small Business Openings", "Leisure/Hospitality Business Openings", "New COVID Death Rate"),
          dep.var.caption = "Dependent Variable: Arts/Entertainment/Recreation Credit/Debit Card Spending", 
          dep.var.labels.include  = FALSE,  
          omit.stat = c('ser', 'f'),
          add.lines = list(c('Fixed Effects', 'No', 'No', 'No', 'No', 'State', 'State'),
                           c('','','','','','','Time'),
                           c('Fixed Effects F Statistic', '', '', '', '', '91.862***', '78.515***')),
          type = 'latex', 
          out = 't1.tex')

##########################################################
############Instrumental Variable Regressions#############
##########################################################

#Regression 7: Instrumental Variable - business closure in effect
m7 <- plm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate | . - gps_away_from_home + bus_close_D , data = p_df, model = 'within', effect = "twoways")

#Regression 8: Instrumental variable - shelter in place/stay at home order in effect
m8 <- plm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate | . - gps_away_from_home + sip_D, data = p_df, model = 'within', effect = "twoways")

#Include only observation with non-NA values for all variables used in regression
#makes it more convenient to use residuals during J test
m9df <- na.omit(p_df[,c("spend_aer", "gps_away_from_home",  "spend_all",  "merchants_all",  "merchants_ss70", "new_death_rate", "bus_close_D", "sip_D")])

#Regression 9: Instrumental variables - both business closure and shelter in place dummies
#note we use m9df, not p_df
m9 <- plm(formula = spend_aer ~ gps_away_from_home + spend_all + merchants_all + merchants_ss70 + new_death_rate | . - gps_away_from_home + bus_close_D + sip_D , data = m9df, model = 'within', effect = "twoways")

#Table 2: Output table for regressions 7-9 to tex file 
stargazer(m7, m8, m9,
          title = "Table 2: Instrumental Variable Regressions of Discretionary Spending on Time Away from Home",
          covariate.labels = c("Time Away from Home", "Total Spending", "Small Business Openings", "Leisure/Hospitality Business Openings", "New COVID Death Rate"),
          dep.var.caption = "Dependent Variable: Arts/Entertainment/Recreation Credit/Debit Card Spending", 
          dep.var.labels.include  = FALSE,  
          column.labels = c('(7)','(8)','(9)'),
          model.numbers = FALSE,
          omit.stat = c('f'),
          add.lines = list(c('Fixed Effects', 'State', 'State', 'State'),
                           c('', 'Time', 'Time', 'Time'),
                           c('Instrumental Variables', 'Business Closure', 'Shelter in Place', 'Business Closure'),
                           c('','','','Shelter in Place'),
                           c('First-stage F Statistic', '70.778***', '336.79***', '176.3***'),
                           c('J-test', '', '', '8.2847'),
                           c('(p-value)', '', '', '(0.016)')),
          type = 'latex',
          out = 't2.tex')

#############################################################
############Instrumental Variable Validity Tests#############
#############################################################

#First stage regression for regression 7
rel7 <- plm(formula = gps_away_from_home ~ spend_all + merchants_all + merchants_ss70 + new_death_rate + bus_close_D , data = p_df, model = 'within', effect = "twoways")

#First stage regression for regression 8
rel8 <- plm(formula = gps_away_from_home ~ spend_all + merchants_all + merchants_ss70 + new_death_rate + sip_D , data = p_df, model = 'within', effect = "twoways")

#First stage regression for regression 9
rel9 <- plm(formula = gps_away_from_home ~ spend_all + merchants_all + merchants_ss70 + new_death_rate + bus_close_D + sip_D , data = p_df, model = 'within', effect = "twoways")

#Residual regression for J test for regression 9
exo9 <- plm(formula = residuals(m9) ~ spend_all + merchants_all + merchants_ss70 + new_death_rate + bus_close_D + sip_D , data = m9df, model = 'within', effect = "twoways")

#Reopen txt file for test results
sink('tests.txt', append = TRUE)

#Check relevance (F test) for instrument in regression 7
linearHypothesis(rel7, "bus_close_DTRUE", test = "F")

#Check relevance (F test) for instrument in regression 8
linearHypothesis(rel8, "sip_DTRUE", test = "F")

#Check relevance (F test) for instruments in regression 9
linearHypothesis(rel9, c("bus_close_DTRUE", "sip_DTRUE"), test = "F")

#Check exogeneity (J test) for instruments in regression 9
linearHypothesis(exo9, c("bus_close_DTRUE", "sip_DTRUE"), test = "Chisq")

#close txt file
sink()


