Peer Assessments /Peer Assessment 1
========================================================
By: Valter Cruz Perez
Date: August - 16- 2014
output: HTLM 

Load libraries required for grphics

```r
library(datasets)
library(nortest)
```

```
## Warning: package 'nortest' was built under R version 3.1.1
```

```r
library(ggplot2)
library(grDevices)
library(RColorBrewer)
```

```
## Warning: package 'RColorBrewer' was built under R version 3.1.1
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
library(lattice)
library(utils)
library(R.utils)
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.32.4 (2014-05-14) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```
Download data frame and unziped

```r
zipfile <- "activity.zip"
if(!file.exists(zipfile)) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, destfile = "zipfile")             
}
```

```
## Error: unsupported URL scheme
```

```r
csvfile <- unzip("zipfile.zip")
```

```
## Warning: error 1 in extracting from zip file
```

```r
csvfile <- "activity.csv"
if(!file.exists(csvfile)) { stop("Expected activity.csv") }

activityData <- read.csv(csvfile)
```

## What is mean total number of steps taken per day?
First identify the steps by day


```r
steps_by_day <- function(data, column) {
  dates <- split(data, data$date)
  sapply(dates, function(byDate) {
    sum(byDate[[column]])
  })
}
byday <- steps_by_day(activityData, "steps")
```
Create histogram plot with rug 


```r
hist(byday, col = "green", 
     breaks = 10, main = "Histogram Steps by Day with Meam", xlab = "Steps By Day")
rug(byday)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
Compute mean and media value

```r
mean(byday, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(byday, na.rm = TRUE)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
Add the data by mean for each interval across all dates

Transform the data to add steps per day


```r
series <- split(activityData, activityData$interval)
mean_series <- sapply(series, function(byInterval) {
  mean(byInterval$steps, na.rm = TRUE)
})
```
Make plot of mean across days for each interval:


```r
plot(names(mean_series), mean_series, type='l', xlab = "Interval", ylab = "Mean")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The interval with the most steps on average:


```r
as.numeric(names(which.max(mean_series)))
```

```
## [1] 835
```
## Imputing missing values
Compute (summarize) total number of missing  observations for all intervals and days


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

Add a column where NA steps observations where substituted by mean values of intervals


```r
activityData$imputedSteps <-
  with(activityData,
       ifelse(is.na(steps), mean_series[as.character(interval)], steps))
```

Get the total steps by date for the imputed dataset, show a histogram and get the mean and median:


```r
imp_by_date <- steps_by_day(activityData, "imputedSteps")
hist(imp_by_date, col = "blue")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
mean(imp_by_date)
```

```
## [1] 10766
```

```r
median(imp_by_date)
```

```
## [1] 10766
```
## Are there differences in activity patterns between weekdays and weekends?

Add a new variable that differentiates between weekdays and weekends:


```r
timming <- weekdays(as.POSIXct(activityData$date))
activityData$dayType <-
  ifelse(timming %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')
```


```r
mean_type_timming <- ddply(activityData, .(interval, dayType), function(group) {
  data.frame(mean = mean(group$steps, na.rm = TRUE))
})
with(mean_type_timming,
     xyplot(mean ~ interval | dayType,
            type = 'l',
            xlab = 'Interval',
            ylab = 'Number of steps',
            layout = c(1,2)))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
## conclutions
Plot show more moving during morning and afternoon I assume that it is because they are workers and those are the period of work commute.
Weekend show similar activity in the morning tha weekdays and reduce during afternoon, that make sence with the first statement, if they are workers most of them redure their activities during afternoon weekends
