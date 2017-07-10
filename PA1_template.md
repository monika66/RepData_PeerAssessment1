# Reproducible Research: Peer Assessment 1

##Introduction

Course: Data Science Specification,
Level:  Reproducible Research,
Project: Peer Assessment 1,
Student: Monika Hunkeler

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. More information at:
https://github.com/monika66/RepData_PeerAssessment1/blob/master/README.md


## Loading and preprocessing the data 

This data analysis was originaly executed with RStudio, R version 3.3.3 (2017-03-06) 
Windows platform: x86_64-w64-mingw32/x64 (64-bit).
CPU: Intel(R) Core(TM) i7-7500U
RAM: 16 GB 

For reproduction set environment workspace to your own workdirectory !

```r
# Set workdirectory
setwd("C:/Users/acer17/Desktop/coursera/RR_Peer_Project1/RepData_PeerAssessment1")
```
The  activity monitoring data "activity.zip" for this report are cloned from forked GitHub repository: https://github.com/monika66/RepData_PeerAssessment1/blob/master/activity.zip 
(Original https://github.com/rdpeng/RepData_PeerAssessment1, state of 11 Feb 2014)

```r
# Read  activity data and load libraries
if (!file.exists("activity.csv")) { unzip(zipfile="./activity.zip") }
Activity <- read.csv("activity.csv")
subActivity <- subset(Activity, Activity$steps != "NA")
library(readr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(chron)
library(lattice)
```
The loaded row data "Activity"" contains 17568 observations of 3 variables. The cleaned data "subActivity" with removed "NA" values contains 15264 observations of  3 variables.


```r
str(subActivity)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## 1. What is mean total number of steps taken per day?

###Introduction 
First calculate the sum of number of steps taken in 5 minute intervals taken per day. Second calculate the mean and median of the total number of steps taken per day. On target of this project is to investigate the influence from ignored missing values "NA", compared with filled missing values. For this part of the analysis, the missing values "NA"" where ignored.

###Question: What is the mean total number of steps taken per day ?

###Answer: 
The mean of the total number of steps taken per day is: 10766.1886792453. The median of the total number of steps taken per day differs with 10765 a bit. 



###Description

Calculate the total numbers of steps taken each day:

```r
sumSteps <- tapply(subActivity$steps,subActivity$date,sum) 
stepsPerDays <- as.data.frame(cbind(sumSteps, names(sumSteps)))
```
The distribution of the histogram with "total number of steps taken per day", shows us as a first estimation the highest frequency from 10'000 to 11'000 steps taken per day: 

```r
## Make a histogram of the total number of steps taken each day
hist(as.numeric(as.character(stepsPerDays$sumSteps)), 25,  xlab = "Total number steps taken per day", main= "Highest frequency between 10'000 to 11'000 total steps per day", sub = "RR1_plot1: missing values NA ignored", col = "lightblue", freq = TRUE)
```

![](PA1_template_files/figure-html/histTotSteps-1.png)<!-- -->

```r
dev.copy(png, file = "RR1_plot1.png") 
```

```
## png 
##   3
```
Calculate and report mean and median of total number of steps taken per day:

```r
print(paste("Mean total number of steps taken per day:", mean(as.numeric(as.character(stepsPerDays$sumSteps)), na.rm = TRUE)))
```

```
## [1] "Mean total number of steps taken per day: 10766.1886792453"
```

```r
print(paste("Median total number of steps taken per day:", median(as.numeric(as.character(stepsPerDays$sumSteps)), na.rm = TRUE)))
```

```
## [1] "Median total number of steps taken per day: 10765"
```


## 2. What is the average daily activity pattern?

###Introduction
Makes a time serie plot of the mean number of steps taken per 5-minute interval, across all days in the dataset,

###Question: Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps ?

###Answer: 
The 5-minute interval containing the highest number of steps is: 835



###Description

The time serie plot of the mean number of steps taken per interval, across all days in the dataset, shows increased activities from intervals 800 to 900, and nearly no activities from interval 0 to 525: 


```r
meanPerInterval <- tapply(subActivity$steps,subActivity$interval, mean) 
meanPerInterval <- as.data.frame(cbind(meanPerInterval, names(meanPerInterval)))
names(meanPerInterval) <- c("meanStepsInterval", "interval")
plot(as.numeric(as.character(meanPerInterval$interval)), as.numeric(as.character(meanPerInterval$meanStepsInterval)), xlab = "5-minute interval", ylab= "Mean of steps", col = "blue", type="l", main = "Intervals with highest activities: 800 ~ 900", sub = "RR1_plot2: interval 835 has maximum number of steps")
```

![](PA1_template_files/figure-html/dayliAveragePlot-1.png)<!-- -->

```r
dev.copy(png, file = "RR1_plot2.png") 
```

```
## png 
##   4
```
Calculate the 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps:

```r
stepsMax <- as.integer(as.character(meanPerInterval[meanPerInterval$meanStepsInterval==max(as.numeric(as.character(meanPerInterval$meanStepsInterval))), 2]))
print(paste("5-minute interval cotaining the maximum number of steps, calculated on average across all days:", stepsMax))
```

```
## [1] "5-minute interval cotaining the maximum number of steps, calculated on average across all days: 835"
```


## 3. Imputing missing values

###Introduction
There are days/intervals where there are missing values (coded as NA). The presence of this missing values may introduce bias into some calculation or summaries of the data.From total 17568 observations there are 2304 observations with missing values.For this reason a new dataset "ActivityCorr" with the missing values (number of steps) where filled with the mean of steps for that interval. "ActivityCorr has the equal variables and numbers of observations like the origin "Activity" data. The variables included in this dataset are:
    steps       Number of steps taking in a 5-minute interval 
    date        The date on which the measurement was taken in YYYY-MM-DD format
    interval    Identifier for the 5-minute interval in which measurement was taken

###Question: What is the impact of imputing missing data on the estimates of the total daily number of steps ?

###Answer: 
The distribution of the total daily number of steps tend now clearer to the mean value: 10766.1886792453. The histogram Plot3 has with 19 a higher frequency then histogram Plot1 with frequency 10. And the mean value do not longer differ from the median value: 10766.1886792453. With ignored missing values, see Question 1, there was a difference between mean value: 10766.1886792453 and median value: 10765.



###Description

Calculate and report the total number of missing values NA in the dataset, i.e. number of rows:

```r
missingValue <- nrow(subset(Activity[is.na(Activity$steps) | is.na(Activity$interval) | is.na(Activity$date), ]))
print(paste("Total number of rows with missing values NA:", missingValue))
```

```
## [1] "Total number of rows with missing values NA: 2304"
```
Replace missing values of steps with mean of number of steps for this interval, across all days. Create a new dataset that has equal data structure like the original dataset Activity with filled missing values:

```r
ActivityCorr <- merge(Activity, meanPerInterval, by="interval")
ActivityCorr[which(is.na(ActivityCorr$steps)) , 2] <- as.numeric(as.character(ActivityCorr[which(is.na(ActivityCorr$steps)) , 4])) 
ActivityCorr <- cbind(ActivityCorr, ActivityCorr$interval)
ActivityCorr <- subset(ActivityCorr[, c(2,3,5)])
colnames(ActivityCorr) <- c("steps", "date", "interval")
```
Calculate the total number of steps taken each day and plot a histogram of it:

```r
sumStepsCorr <- tapply(ActivityCorr$steps,ActivityCorr$date,sum) 
stepsPerDaysCorr <- as.data.frame(cbind(sumStepsCorr, names(sumStepsCorr)))
hist(as.numeric(as.character(stepsPerDaysCorr$sumStepsCorr)), 25, xlab = "Total number of steps taken per day", main= "Highest frequency between 10'000 to 11'000 total steps per day", sub = " RR1_plot3: missing values filled by mean steps of interval", col = "grey", freq = TRUE)
```

![](PA1_template_files/figure-html/plotTotSteps-1.png)<!-- -->

```r
dev.copy(png, file = "RR1_plot3.png")
```

```
## png 
##   5
```
Calculate mean and median of total number of steps taken per day, with missed values filled by mean of that interval:

```r
print(paste("Mean of total number of steps taken per day with missed values filled by mean of interval:", mean(as.numeric(as.character(stepsPerDaysCorr$sumSteps)), na.rm = TRUE)))
```

```
## [1] "Mean of total number of steps taken per day with missed values filled by mean of interval: 10766.1886792453"
```

```r
print(paste("Median of total number of steps taken per day with missed values filled by median of interval:", median(as.numeric(as.character(stepsPerDaysCorr$sumSteps)), na.rm = TRUE)))
```

```
## [1] "Median of total number of steps taken per day with missed values filled by median of interval: 10766.1886792453"
```

## 4. Are there differences in activity patterns between weekdays and weekends?

###Introduction
Average number of steps per 5-minute interval, taken across all weekday or weekends calculated. Dataset "ActivityCorr" with filled-in missing values used.

###Question: Are there differences in activity patterns between weekdays and weekends?

###Answer: 
Yes, there are differences between weekdays and weekend activity pattern. The activity pattern for weekdays shows fast increasing activities from  interval 500 to 600. At weekend there are very slow increasing activities from 500 to 800. So it seams the most persons don't like to stand up early if they dont have do work and their activitiy levels are more individual at weekend. 
From interval 900 to 2200 there is a higher activity level at weekend. People seams to use their freetime do move arround and less persons may be sitting in the office.
At weekday there is earlier in the evening less activity (~ 10 steps), at interval 2000 instead of interval 2200 at weekend. 



###Description

Create a factor variable with two levels "weekday" and "weekend":

```r
wdays <- factor(is.weekend(as.Date(ActivityCorr$date)), labels = c("weekday", "weekend"))
```
Make a panel plot of average number of steps per interval, taken across all weekday or weekends:

```r
meanStepsWdayInt <- summarise(group_by(cbind(ActivityCorr, wdays), wdays, interval), meanSteps= mean(steps))
p <- xyplot(meanSteps ~ interval | wdays, data = meanStepsWdayInt, type="l", xlab = "Interval", ylab = "Number of steps", sub = "RR1_plot4", layout = c(1, 2))   
print(p)
```

![](PA1_template_files/figure-html/plotTotWeekdays-1.png)<!-- -->

```r
dev.copy(png, file = "RR1_plot4.png") 
```

```
## png 
##   6
```

```r
dev.off()
```

```
## png 
##   2
```


