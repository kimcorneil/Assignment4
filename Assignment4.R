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

# Periods in column names are not the best so replace with underscores
# duplicate ufos4 and name it ufos5
ufos5 <- ufos4
names(ufos5)[names(ufos5) == "duration.seconds"] <- "duration_seconds"
names(ufos5)[names(ufos5) == "duration.hours.min"] <- "duration_hours_min"
# View and summarize the data after making changes
View(ufos5)
summary(ufos5)
names(ufos5)

# Change city, state, country, and shape into factors
# duplicate ufos5 and name it ufos6
ufos6 <- ufos5
ufos6$city <- factor(ufos6$city)
ufos6$state <- factor(ufos6$state)
ufos6$country <- factor(ufos6$country)
ufos6$shape <- factor(ufos6$shape)
# View and summarize the data after making changes
View(ufos6)
summary(ufos6)

# Replace empty values with NA in state, country 
# duplicate ufos6 and name it ufos7
ufos7 <- ufos6
# get the levels to know which to replace
levels(ufos7$country)
levels(ufos7$state)
levels(ufos7$shape)
# replace with NAs
ufos7$country[ufos7$country == ""] <- NA
ufos7$state[ufos7$state == ""] <- NA
ufos7$shape[ufos7$shape == ""] <- NA
ufos7$shape[ufos7$shape == "unknown"] <- NA
# drop empty levels
ufos7$country <- droplevels(ufos7$country)
ufos7$state <- droplevels(ufos7$state)
ufos7$shape <- droplevels(ufos7$shape)
# View and summarize the data after making changes
View(ufos7)
summary(ufos7)

# Deal with the craziness that is the city column
# See why it is crazy by checking the levels
levels(ufos7$city)

# Fix stuff for duration_seconds
# Deal with zeros
# add dplyr to library
library("dplyr")
# see zeros
ufos7 %>% filter(duration_seconds == 0)
# there are no zeros, lowest value is 0.02

# Remove columns that may be a hoax
# duplicate ufos7 and name it ufos8
ufos8 <- ufos7
# replace any columns in comments that have ((HOAX??)) with NA
## use mutate_at to specify only the comments column
## create an annonymous function using ifelse with the test being to search for ((HOAX??)) with grepl (repturns a logical)
## if grepl returns true replace it with NA and if grepl returns false leave it
## use \\ in HOAX to deal with metacharacters
ufos8 <- ufos8 %>% mutate_at(c("comments"), ~ ifelse(grepl("\\(\\(HOAX\\?\\?\\)\\)", ufos8$comments), NA, ufos8$comments))
# remove any rows that have na in the comments using tidyr function drop_na()
ufos8 <- ufos8 %>% drop_na(comments)
# View and summarize the data after making changes
View(ufos8)
summary(ufos8) 
# compare dimensions with ufos8 and ufos7 to ensure rows were removed
dim(ufos8)                                      
dim(ufos7)    

# Create report_delay column which is the delay in days between date_posted and date_observed
# duplicate ufos8 and name it ufos9
ufos9 <- ufos8
# Use mutate to create a new colummn called report_delaye which is date_posted - date_observed
ufos9 <- ufos9 %>% mutate(ufos9, report_delay = date_posted - date_observed)
# remove all rows where the number of days is negative using the same method as LINE WHAT KIM FOCUS HERE ##########
ufos9 <- ufos9 %>% mutate_at(c("report_delay"), ~ifelse(grepl("\\-", report_delay), NA, report_delay))
# remove any rows that have na in the comments using tidyr function drop_na()
ufos9 <- ufos9 %>% drop_na(report_delay)
# View and summarize the data after making changes
View(ufos9)
summary(ufos9)    
# compare dimensions with ufos8 and ufos7 to ensure rows were removed
dim(ufos8)                                      
dim(ufos9)  

# Create a table representing report_delay per country
table(ufos9$report_delay, ufos9$country)

### DELETE THIS CAUSE I DO NOT KNOW HOW TO EXTRACT JUST THE COUNTRIES
# fix some NAs in country by using what is in the brackets in city
# duplicate ufos9 and name it ufos10
ufos10 <- ufos9
# make a new column for if there were brackets in city
ufos10 <- separate(ufos10, city, into = c("city", "brackets"), sep = "\\(")
# get rid of anything that is not a letter or ) or " "
ufos10 <- ufos10 %>% mutate_at(c("brackets"), ~ifelse(grepl("[^a-z) ]", brackets), NA, brackets))
# make brackets a fatcor to view the levels
ufos10$brackets <- factor(ufos10$brackets)
levels(ufos10$brackets)
levels(ufos10$country)
## IF I CONTINUED I WOULD REPLACE ANY LEVEL IN BRACKETS WITH ie canada) with ca IN COUNTRIES ##

# Create a histogram of duration_seconds
hist(ufos9$duration_seconds)




