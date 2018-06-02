
 
library(dplyr)
library(ggplot2)
library(lubridate)
library(stats)

##set directory and read in file
setwd("~/Data Science/Reproducible data/Week 2/data")
activity <- read.csv("activity.csv",header=TRUE, sep=",")

##str(activity)
##Convert date to date format
activity$date <- mdy(activity$date)

##Calculate mean number of steps taken per day and create a histogram

  
  tsteps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

##histogram
ggplot(tsteps, aes(x = steps)) +
  geom_histogram(fill = "royalblue2", binwidth = 1000) +
  labs(title = "Steps per day", x = "Steps per day", y = "Frequency")

## calculate mean and median steps per day
  
MeanSteps <- mean(tsteps$steps, na.rm = TRUE)
MedianSteps <- median(tsteps$steps, na.rm = TRUE)

MeanSteps
MedianSteps

##average daily activity pattern-Make a time series plot of the 5 minute interval and 
##find the interval with the max number of steps

interval <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
 
##time series plot of 5 minute interval

ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "royalblue2") + xlab("5-minute interval") +
  ylab("Average number of steps") 

##Which interval has on average the max number of steps

interval[which.max(interval$steps),]

##Imputing missing values


##calculate number of missing values
numMissingValues <- length(which(is.na(activity$steps)))
numMissingValues

##missing data will be imputed by replacing the missing value with
##the mean steps per day

##make a copy of the data set for imputing

Imputedsteps <- activity

str(Imputedsteps)
##calculate mean per time interval
avgPerinterval <- tapply(Imputedsteps$steps, Imputedsteps$interval, mean, na.rm=TRUE)

##Identify NA

MissstepVal <- is.na(Imputedsteps$steps) 
 
##replace NA with Value

Imputedsteps$steps[MissstepVal] <- avgPerinterval[as.character(Imputedsteps$interval[MissstepVal])]

##recalculate steps now that have iputed missing values
  
  TotImputedsteps <- Imputedsteps %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

##create histogram for this new total steps
  
  ggplot(TotImputedsteps, aes(x = steps)) +
  geom_histogram(fill = "royalblue2", binwidth = 1000) +
  labs(title = "Imputed Steps per day", x = "Steps per day", y = "Frequency")

##calculate mean and median of steps from imputed data set

meanImputedsteps <- mean(TotImputedsteps$steps, na.rm = TRUE)
medianImputedsteps <- median(TotImputedsteps$steps, na.rm = TRUE)

meanImputedsteps
medianImputedsteps

##Differences in weekends and weekdays?
##1.	Create a new factor variable in the dataset with two levels - 
##"weekday" and "weekend" indicating whether a given date is a weekday or weekend day
##2.	Make a panel plot containing a time series plot 
##and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

Imputedsteps$day <- weekdays(as.Date(Imputedsteps$date))

Imputedsteps$wday <- ifelse(weekdays(Imputedsteps$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
str(Imputedsteps)
##recalculate steps by weekend/weekday

ImputedStepsW <- Imputedsteps %>%
  group_by(interval, wday) %>%
  summarise(steps = mean(steps))

s <- ggplot(ImputedStepsW, aes(x=interval, y=steps, color = wday)) +
  geom_line() +
  facet_wrap(~wday, ncol = 1, nrow=2)
print(s)