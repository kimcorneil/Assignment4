# Assignment 4
# Kimberly Corneil
# July 19, 2024

####### MASTER PLAN #######
#' Save file into Assignment4 folder
#' Set working directory to Assignment4 (informing user they will need to ensure the file is in their wd)
#' Read the data into a dataframe 
#' View and summarize the data
#' Clean the data and ensure it is in the correct format
#' Remove potential hoaxs
#' Add report_delay
#' Remove sightings that were reported before they happened
#' Create average report_delay table per country
#' Create a histogram of the duration per seconds column

###### Assignment ######
# Load file as data frame
## For reviewer: ensure ufo_subset.csv is in your working directory 
ufos <- read.csv("ufo_subset.csv")
class(ufos)

# View and summarize the data
# Also check column names
View(ufos)
summary(ufos)
names(ufos)
## some initial issues: 
#' datetime and date_posted are character 
#' duration.hours.min is a mess
#' some countries missing
#' zeros in duration.seconds

# Fix date issue in datetime and date_posted
# Install tidyr - previously installed so add to library 
library("tidyr")
# Make datetime into seperate columns for dates and times observed
ufos2 <- separate(ufos, datetime, into = c("date_observed", "time_observed"), sep = " ")
# View and summarize the data after making changes
View(ufos2)
summary(ufos2)
names(ufos2)

# Make date_observed and date_posted into recognizable dates
# Install lubridate - previously installed so add to library 
library("lubridate")
# duplicate ufos2 and name it ufos3
ufos3 <- ufos2
# convert date_observed to ymd format so it can be recognized as a date and not a character
ufos3$date_observed <- ymd(ufos3$date_observed)
# convert date_posted to dmy format so it can be recognized as a date and not a character
ufos3$date_posted <- dmy(ufos3$date_posted)
# convert date_posted to ymd so it is presented in the same format as date_observed for consistency
ufos3$date_posted <- ymd(ufos3$date_posted)
# View and summarize the data after making changes
View(ufos3)
summary(ufos3)

# duplicate ufos3 and name it ufos4
ufos4 <- ufos3
# Make time_observed into a time period using lubridate and not a character
ufos4$time_observed <- hm(ufos4$time_observed)
# View and summarize the data after making changes
View(ufos4)
summary(ufos4)

