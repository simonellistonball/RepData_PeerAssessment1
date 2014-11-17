# Reproducible Research: Peer Assessment 1
First we need to load some libraries, and set some global options.

```r
library(knitr)
opts_chunk$set(fig.width=12, fig.height=8)
library(lubridate)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(reshape2)
```

## Loading and preprocessing the data
The data has been downloaded from the coursera site, using the download.R script, which downloads and unzips the data file to data/activity.csv. First we must load, and convert this data to appropriate types. 


```r
d <- read.csv(file='data/activity.csv', header=T)
d$date <- ymd(d$date)
d$steps <- as.numeric(d$steps)
d$interval <- as.numeric(d$interval)
```

## What is mean total number of steps taken per day?
The total steps per day are the plotted here:

```r
ggplot(d, aes(x=date, y=steps)) + geom_histogram(stat="identity") + 
  ylab('Total number of steps') + 
  xlab('Date') +
  ggtitle('Total Steps by Day')
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Mean and median total steps per day, ignoring missing values

```r
d %>% group_by(date) %>% summarize(total_steps_in_day=sum(steps)) -> d_per_day
summarize(d_per_day, mean=mean(total_steps_in_day, na.rm=T), median=median(total_steps_in_day, na.rm=T))
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
## 1 10766.19  10765
```

## What is the average daily activity pattern?

To investigate the daily activity pattern, we summarise by interval and extract the mean and median steps for that interval across the whole date period.


```r
d %>% group_by(interval) %>% summarize(mean_steps=mean(steps, na.rm=T), median_steps=median(steps, na.rm=T)) -> d_per_interval
p = ggplot(d_per_interval, aes(x=interval)) + geom_line(aes(y=mean_steps))+ 
  ylab('Number of steps') + 
  xlab('Interval') +
  ggtitle('Mean steps by 5 minute interval')

max_steps <- max(d_per_interval$mean_steps)
d_per_interval %>% filter(mean_steps==max_steps) -> max_steps_interval

p <- p + geom_text(data = d_per_interval[d_per_interval$interval == max_steps_interval$interval ,], aes(interval,mean_steps,hjust=0,label=paste('Max at interval: ', interval, 'is', format(round(mean_steps,2)))))

print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The interval with the greatest average number of steps is: **835**

## Imputing missing values
The number of missing values for each column is given in the table below:

```r
colSums(is.na(d))
```

```
##    steps     date interval 
##     2304        0        0
```

There are **2304** missing values in the data set over all. All of these are in the steps column.

These missing values will be replaced by the corresponding mean value for the given interval across the rest of the data set. 


```r
d %>% left_join(d_per_interval, by=c('interval')) %>% transform(steps_filled=ifelse(is.na(steps),mean_steps, steps)) %>% select(date, interval, steps=steps_filled)-> d_filled
```

Having filled in the missing values with the mean of the given interval, we recreate the plot of totals by day.


```r
ggplot(d_filled, aes(x=date, y=steps)) + geom_histogram(stat="identity")+ 
  ylab('Number of steps') + 
  xlab('Date') +
  ggtitle('Total Steps by Day with imputed missing values')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Mean and median total steps per day, ignoring missing values

```r
d_filled %>% group_by(date) %>% summarize(total_steps_in_day=sum(steps)) -> d_per_day
summarize(d_per_day, mean=mean(total_steps_in_day, na.rm=T), median=median(total_steps_in_day, na.rm=T))
```

```
## Source: local data frame [1 x 2]
## 
##       mean   median
## 1 10766.19 10766.19
```

Adding the mean values to missing values has filled in a number of the missing days from the original data set. It has also moved the mean closer to the median. The overall pattern remains broadly similar. 

## Are there differences in activity patterns between weekdays and weekends?

```r
d_filled %>% mutate(weekday=weekdays(date), weekend=(weekday=="Sunday"|weekday=="Saturday")) -> d_filled

d_filled %>% group_by(weekend, interval) %>% summarize(mean_steps=mean(steps, na.rm=T)) -> d_filled_per_interval

d_filled_per_interval$weekend<-as.factor(d_filled_per_interval$weekend)
levels(d_filled_per_interval$weekend) <- c('Weekday', 'Weekend')

ggplot(d_filled_per_interval, aes(x=interval)) + 
  geom_line(aes(y=mean_steps)) + 
  ylab('Number of steps') + 
  xlab('Interval') +
  ggtitle('Comparison of mean steps per interval on weekends and weekdays') +
  facet_grid(weekend ~ .)   
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

It would seem that there are overall fewer steps at the weekends during most intervals, and that the steps during earlier intervals in the day are considerably lower at weekends. Weekend steps also seem more evenly distributed over the length of the day.
