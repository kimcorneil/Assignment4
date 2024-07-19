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
# Ensure ufos is a data frame
class(ufos)

# View and summarize the data
# Also check column names
View(ufos)
summary(ufos)
names(ufos)

# Fix date issue in datetime and date_posted
# Install tidyr - previously installed so add to library 
library("tidyr")
# Make datetime into separate columns for dates and times observed, separating by a space
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
# Make time_observed into a time period (hours and minutes) using lubridate and not a character
ufos4$time_observed <- hm(ufos4$time_observed)
# View and summarize the data after making changes
View(ufos4)
summary(ufos4)

# Replace periods in column names with underscores
# duplicate ufos4 and name it ufos5
ufos5 <- ufos4
names(ufos5)[names(ufos5) == "duration.seconds"] <- "duration_seconds"
names(ufos5)[names(ufos5) == "duration.hours.min"] <- "duration_hours_min"
# Confirm the changes
names(ufos5)

# Change city, state, country, and shape into factors to see the summary of data better (summarized by levels)
# duplicate ufos5 and name it ufos6
ufos6 <- ufos5
ufos6$city <- factor(ufos6$city)
ufos6$state <- factor(ufos6$state)
ufos6$country <- factor(ufos6$country)
ufos6$shape <- factor(ufos6$shape)
# View and summarize the data after making changes
View(ufos6)
summary(ufos6)

# Replace empty values with NA in state, country, and shape to avoid them skewing data in downstream analysis
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

# Remove columns that may be a hoax
# duplicate ufos7 and name it ufos8
ufos8 <- ufos7
# replace any columns in comments that have ((HOAX??)) with NA
## use mutate_at to specify only the comments column
## create an annonymous function using ifelse with the test being to search for ((HOAX??)) with grepl (returns a logical)
## if grepl returns true replace it with NA and if grepl returns false leave it
## use \\ in HOAX to deal with metacharacters
ufos8 <- ufos8 %>% mutate_at(c("comments"), ~ ifelse(grepl("\\(\\(HOAX\\?\\?\\)\\)", comments), NA, comments))
# remove any rows that have NA in the comments using tidyr function drop_na()
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
# Use mutate to create a new column called report_delae which is date_posted - date_observed
ufos9 <- ufos9 %>% mutate(ufos9, report_delay = date_posted - date_observed)
# remove all rows where the number of days is negative using the same method as 110
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
mytable <- ufos9 %>% 
  filter(!is.na(country)) %>%  # remove NAs from country
  group_by(country) %>% # group by country
  summarize(AvgDelay = mean(report_delay, na.rm = T)) # get the mean number of days delayed per country 

# Create a histogram of duration_seconds
# remove the zeros (log10(0) is negative infinity)
ufos9F <- ufos9 %>% filter(duration_seconds > 0)
# create a histogram using base log10 because the data in duration_seconds spans multiple orders of magnitude and without log10 is grouped into 1 large bin
hist(log10(ufos9F$duration_seconds), main = "Log Transformed Frequencies of the Duration of UFO Sitings", xlab = "Log Duration (Seconds)", ylab = "Frequency")




