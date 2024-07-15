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


