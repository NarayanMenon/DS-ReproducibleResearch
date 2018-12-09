---
title: "PA1_template"
author: "Narayan Menon"
date: "December 8, 2018"
---





```r
library(dplyr)
library(lattice)
```

Load the file from working directory


```r
act <- read.csv("activity.csv")

#convert the date from factor
act$date <- as.Date(act$date)
```


Calculate total steps in a day

```r
daily_act <- aggregate(steps ~ date,data=act,sum)
```

Draw a histogram of total steps in a day

```r
hist(daily_act$steps,breaks=50)
```

![plot of chunk histogram without NAs](figure/histogram without NAs-1.png)



```r
daily_mean <- mean(daily_act$steps,na.rm=TRUE)
```
Mean number of steps in a day: 1.0766189 &times; 10<sup>4</sup>



```r
daily_median <- median(daily_act$steps,na.rm=TRUE)
```
Median number of steps in a day: 10765

Calculate total steps by interval across all days

```r
interval_act <- aggregate(steps ~ interval,data=act,mean,na.rm=TRUE)
```

Graph showing average number of steps by intervals

```r
plot(x=interval_act$interval,y=interval_act$steps,type="l",xaxt="n",
     xlab="Intervals",ylab="Average steps")
axis(1, at=seq(0, 2355, by=50))
```

![plot of chunk graph of steps vs intervals](figure/graph of steps vs intervals-1.png)




```r
maxInterval <- interval_act[interval_act$steps==max(interval_act$steps),]$interval
```

Interval that has highest average number of steps: 835



```r
missingCount <- sum(is.na(act$steps))
```
Count of missing values in the dataset: 2304


Function to return mean steps of an interval

```r
getIntMean <- function(interval) {
    interval_act[interval_act$interval ==
                         as.numeric(interval),][[2]]}
```

Function to get adjusted steps to remove all NAs

```r
getAdjustedSteps <- function(steps,intMean) {
    if (is.na(steps)) {
        intMean
    }
    else {
        steps
    }
}
```

Create a new data frame that includes a column that has no NAs for steps

```r
act1 <- act %>% rowwise() %>% mutate(intMean=getIntMean(interval))
act2 <- act1 %>% rowwise() %>% mutate(adjSteps=getAdjustedSteps(steps,intMean))
```


Histogram of total steps each day

```r
daily_adj_act <- aggregate(adjSteps ~ date,data=act2,sum)
hist(daily_adj_act$adjSteps,breaks=50)
```

![plot of chunk Histogram with imputed values](figure/Histogram with imputed values-1.png)



```r
adjMean <- mean(daily_adj_act$adjSteps)
```

Mean with imputed data: 1.0766189 &times; 10<sup>4</sup>


```r
adjMedian <- median(daily_adj_act$adjSteps)
```
Median with imputed data: 1.0766189 &times; 10<sup>4</sup>

Add a factor variable to indicate if it is weekday or weekend

```r
act2$dayInd <- factor(weekdays(act2$date) %in% c('Saturday','Sunday'),
                      levels=c('TRUE','FALSE'), labels=c('weekend','weekday'))
```

Total steps by interval across all weekdays & weekends 

```r
imputedAvgs <- aggregate(adjSteps~interval+dayInd,data=act2,mean)
```

Graph of average steps for weekdays and weekends

```r
xyplot(adjSteps ~ interval|dayInd,data=imputedAvgs,type="l")
```

![plot of chunk Panel plot showing activity between weekdays and weekends](figure/Panel plot showing activity between weekdays and weekends-1.png)

