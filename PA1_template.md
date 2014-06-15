Introduction
========================================================

In this study use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Variables
==================
- **steps:** Number of steps taking in a 5-minute interval
- **date:** The date on which the measurement was taken in YYYY-MM-DD format
- **interval:** Identifier for the 5-minute interval in which measurement was taken

```r
amData <- read.csv( "activity.csv" )
amData$date <- as.Date( amData$date )
amNoNA <- amData[complete.cases (amData ), ]
```

Histogram of the total number of steps taken each day:


```r
###1. Make a histogram of the total number of steps taken each day
amStepsNoNA <- tapply( amNoNA$steps, amNoNA$date, sum )
hist( amStepsNoNA, main = "Total number of steps taken each day", xlab = "", col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Mean and median of total number of steps taken per day as follows:

```r
mean( amStepsNoNA )
```

```
## [1] 10766
```

```r
median( amStepsNoNA )
```

```
## [1] 10765
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days


```r
amAvgSteps <- tapply( amNoNA$steps, amNoNA$interval, mean )
amIntervals <- as.numeric( unlist( attributes( amAvgSteps )[2]) )
plot( amIntervals, amAvgSteps, type = "l", col = "blue", xlab = "", ylab = "")
title( main = "Average number of steps vs 5-minute interval" )
title( xlab = "5-minute interval" )
title( ylab = "Average number of steps" )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

On average across all the days in the dataset, following 5-minute interval contains the maximum number of steps.

```r
sum( as.numeric( amAvgSteps == max( amAvgSteps ) ) * amIntervals )
```

```
## [1] 835
```

Following display the  total number of missing values in the dataset.

```r
dim( amData )[1] - dim( amNoNA )[1]
```

```
## [1] 2304
```
Medain of the 5-minute interval, was used to replace the missing values.

```r
amMedianSteps <- tapply( amNoNA$steps, amNoNA$interval, median )
amIntervalLookup <- data.frame( amMedianSteps, amIntervals )
names( amIntervalLookup )<-c( "MedianSteps", "interval" )
amNew <- ( merge( amIntervalLookup, amData, by = 'interval' ) )
amNew$MedianSteps <- is.na(amNew$steps) * amNew$MedianSteps
amNew[is.na( amNew )] <- 0
amNew2 <- data.frame( amNew$interval, amNew$date, amNew$MedianSteps + amNew$steps )
names( amNew2 )<-c( "interval", "date", "steps" )
```
Histogram

```r
amSteps <- tapply( amNew2$steps, amNew2$date, sum )
hist( amSteps, main = "Total number of steps taken each day", xlab = "", col = "blue")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Mean and median of total number of steps taken per day as follows:


```r
mean( amSteps )
```

```
## [1] 9504
```

```r
median( amSteps )
```

```
## [1] 10395
```

Since we used the median to replace the missing values, the mean and median with those replacements are different from when we ignore the missing values from the dataset.

**Activity patterns between weekdays and weekends**

```r
day <- weekdays( amNew2$date )
day <-( day == "Sunday" | day == "Saturday") * 1
day <- factor( day, labels = c( "weekday", "weekend" ) )

wd <- subset( amNew2 , day == 'weekday' )
we <- subset( amNew2 , day == 'weekend' )

par(mfrow = c(2, 1))
wdAvgSteps <- tapply( wd$steps, wd$interval, mean )
weAvgSteps <- tapply( we$steps, we$interval, mean )
newDay <- factor(rep( 0:1, each = 288 ), label = c( "weekday", "weekend" ) )
AvgSteps <- c( wdAvgSteps, weAvgSteps )
Intervals <- as.numeric( unlist( attributes( weAvgSteps )[2]) )
Intervals <- c(Intervals,Intervals)


library(lattice)
xyplot(AvgSteps  ~ Intervals  | newDay, layout = c(1, 2), xlab = "Interval" , ylab = "Number of steps", panel = function(x, y, ...) {
panel.xyplot(x, y, type = "l", ...)
})
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
