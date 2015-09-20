---
title: "Reproducible Research - Peer Assessment 1"
author: "Keith Hoffman"
date: "September 19, 2015"
output: html_document
---

# Peer Assessment 1

## Loading and preprocessing the data

Set options and load data.

File has been downloaded, unzipped and is placed in working directory.




```r
knitr::opts_chunk$set(cache=TRUE, results='asis')

setwd('~/coursera/r/C5/RepData_PeerAssessment1')

activity <- read.csv('activity.csv')
# convert activity$date from a Factor of a string to a date.
activity$date = as.Date(activity$date)
```

##What is mean total number of steps taken per day?

Compute data and plot information.



```r
# summarize by day
steps.per.day = aggregate(formula=steps ~ date, data=activity,  FUN=sum, na.rm=TRUE)

# and plot a histogram.
library(ggplot2)
histo = ggplot(steps.per.day, aes(x=date, y=steps)) + 
  geom_bar(stat='identity') + 
  ylab("Steps") + xlab("Date") +
  theme_bw() + 
  ggtitle("Daily Steps")
histo
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# compute the mean and median steps per day.

mean(steps.per.day$steps, na.rm=TRUE)
```

[1] 10766.19

```r
median(steps.per.day$steps, na.rm=TRUE)
```

[1] 10765

##What is the average daily activity pattern?

1. create a time series graph over average steps per interval over a 24 hour period.
2. determine interval with the highest average number of steps.


```r
# summarize by time interval
avg.steps.per.interval = aggregate(formula=steps ~ interval, data=activity,  FUN=mean, na.rm=TRUE)

# and plot a time series for a 24 hours period (12AM to 12AM)
time.series = ggplot(avg.steps.per.interval, aes(x=interval, y=steps)) + 
  geom_line() + 
  ylab("Average Steps") + xlab("Hours x 100 Past Midnight") +
  theme_bw() + 
  ggtitle("Average Steps per 5 Minute Time Intervals over 24 Hours")
time.series
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# determine which interval contains the average number of steps that is highest.

max.steps = max(avg.steps.per.interval$steps)
time.interval=avg.steps.per.interval$interval[match(max.steps, avg.steps.per.interval$steps)]
message("highest step count of ",max.steps," occurred at interval ", time.interval)
```

```
## highest step count of 206.169811320755 occurred at interval 835
```

##Imputing missing values



```r
# How many rows are coded as NA in column 'steps'
number.of.NA.intervals = length(which(is.na(activity$steps)))
message("There are ",number.of.NA.intervals," intervals that are reported as NA")
```

```
## There are 2304 intervals that are reported as NA
```

```r
# use avg.steps.per.interval to fill in a step count wherever it is missing
revised.activity = merge(activity,avg.steps.per.interval,
                         by.x='interval',by.y='interval',
                         all.y=TRUE)
revised.activity$steps = ifelse(is.na(revised.activity$steps.x),revised.activity$steps.y,revised.activity$steps.x)

# summarize by day
steps.per.day2 = aggregate(formula=steps ~ date, data=revised.activity,  FUN=sum, na.rm=TRUE)

# and plot a histogram.
histo = ggplot(steps.per.day2, aes(x=date, y=steps)) + 
  geom_bar(stat='identity') + 
  ylab("Steps") + xlab("Date") +
  theme_bw() + 
  ggtitle("daily steps after missing observations have been imputed")
histo
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# compute the mean and median steps per day.

mean(steps.per.day2$steps)
```

[1] 10766.19

```r
median(steps.per.day2$steps)
```

[1] 10766.19

```r
# after missing values are imputed, mean steps per day has changed.
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lubridate)

# create a factor variable to identify weekend and weekday
revised.activity$day.type = factor( wday(revised.activity$date) %in% c(1,7), levels=c(TRUE,FALSE),
                                 labels = c('weekend','weekday'))

# summarize by time interval and weekend/weekday status
avg.steps.per.interval = aggregate(steps ~ interval + day.type, data=revised.activity,  FUN=mean, na.rm=TRUE)

# and plot a time series for a 24 hours period (12AM to 12AM), segregated by weekend and weekday.
time.series = ggplot(avg.steps.per.interval, aes(x=interval, y=steps)) + 
  geom_line() +
  facet_grid(day.type ~ .) +
  ylab("Average Steps") + xlab("Hours x 100 Past Midnight") +
  theme_bw() + 
  ggtitle("Average Steps per 5 Minute Time Intervals over 24 Hours")
time.series
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Answer:  

There is a difference.  

Weekends have more steps throughout the day and fewer steps in the early morning.
