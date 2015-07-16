---
title: "Peer Assessment 1"
author: "Damià Valero"
date: "Wednesday, July 16, 2015"
output: html_document
---

##Loading and preprocessing the data

This section includes the data loading in order to perform the analysis. The code is presented as follows: 


```r
setwd("C:/Users/Damià/Desktop/Data science specialization/5.Reproducible Research/Project 1")
data <- read.csv("./activity.csv",
                 stringsAsFactors = FALSE,
                 sep = ",", 
                 header = TRUE,
                 na.string = "NA")
```

Now it is possible to have a look at the data:


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##Mean total number of steps taken per day

This section tries to answer the  following question: What is mean total number of steps taken per day?
It is necessary to follow this steps: 

1. Calculate the total number of steps taken per day

2. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
#Calculate the total number of steps for each day
  steps_per_day <- with(data, tapply(steps, date, sum))

#Remove the NA values
  steps_per_day[complete.cases(steps_per_day)]
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```

```r
#Histogram
  hist(steps_per_day, breaks=20, xlab="Steps per day", 
                main="Number of steps taken per day",col="red",
                ylim=c(0,15))
```

<img src="figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

```r
#Mean and median
  mean_steps <- round(mean(steps_per_day, na.rm=T),2)
  median_steps <- round(median(steps_per_day, na.rm=T),0)
  mean_steps;median_steps
```

```
## [1] 10766.19
```

```
## [1] 10765
```

Finally, the following results are obtained: 

- Mean total number of steps per day:   **10766.19**
- Median of number of steps per day:    **10765**


##Average daily activity pattern

This section presents the study of the daily activity pattern. The next steps are requiered:

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#For this part, the NA values won't be considered in the analysis: 
  clean_data <- data[complete.cases(data$steps),]

#New data frame which contains the average number of steps for each time interval
  data_interval <- as.data.frame(matrix(nrow=length(levels(as.factor(data$interval))),ncol=2))
  colnames(data_interval)=c("steps","interval")
  data_interval$steps <- as.numeric(with(clean_data, tapply(steps, interval, mean)))
  data_interval$interval <- names(with(clean_data, tapply(steps, interval, mean)))
  
  rm(clean_data)

#Plot:
  plot(y=data_interval$steps, x=data_interval$interval, type="l",col="red",
       xlab="Intervals", ylab="Steps", main="Average number of steps for each 5-minute interval")
```

<img src="figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />

```r
#Interval with the maximum number of steps
  row_position <- which(data_interval$steps %in% max(data_interval$steps))
  max_steps_interval <- data_interval[row_position,"interval"]; max_steps_interval
```

```
## [1] "835"
```

After the analysis, we can conclude that:

- The 5-minute time interval with the highest average steps is: **835** 
- Which corresponds to:                                         **8 hours 35 minutes** time interval.


##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The choosen strategy is to take the mean for that 5-minute interval. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Total number of missing values (NA) in the dataset: 
  NA_rows <- sum(as.numeric(colSums(is.na(data)))); NA_rows
```

```
## [1] 2304
```

```r
#Filling all missing values
  rownames(data_interval)=data_interval$interval #date_interval is the set used in the previous part 2, which contains the avg step value for each 5-minute time interval. 
  NA_data <- data[is.na(data$steps),]
  head(NA_data)  #look at the data
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
  for(i in 1:nrow(NA_data)){
    if(is.na(NA_data[i,"steps"])==TRUE){
      int <- as.character(NA_data[i,"interval"])
      NA_data[i,"steps"] <- data_interval[int,"steps"]
    }
  }
  head(NA_data)#taking a look at the replaced data: 
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
#Creating the new dataset with the mean values of each interval replacing the NAs.
  clean_data <- rbind(NA_data, data[!is.na(data$steps),])
  clean_data <- clean_data[order(clean_data$date, clean_data$interval),]

#Histogram
  steps_per_day2 <- with(clean_data, tapply(steps, date, sum))
  hist(steps_per_day2, breaks=20, xlab="Steps per day",
       main="Number of steps taken per day",col="red", ylim=c(0,15))
```

<img src="figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

```r
#Mean and median
  mean_steps2 <- round(mean(steps_per_day2),2)
  median_steps2 <- round(median(steps_per_day2),0)
  mean_steps2;median_steps2
```

```
## [1] 10766.19
```

```
## [1] 10766
```


Using the approach of substituding the **2304** NA values for the mean values for that 5-minute interval, the results are almost equal, showing that the approach is good enough:

- Means (1st with NA, 2nd without NA): **10766.19** = **10766.19**. 
- Medians (1st with NA, 2nd without NA): **10765** != **10766**. Almost equal median. 
- Histogram (1st with NA, 2nd without NA): There is a clear difference, as it can be seen in the next figures. The peak of steps is at the same interval, but higher. The other values are still the same for both plots. 


```r
par(mfrow=c(1,2))
  hist(steps_per_day, breaks=20, xlab="Steps per day", 
                main="Steps per day: With NAs",col="red",
                ylim=c(0,20))
  hist(steps_per_day2, breaks=20, xlab="Steps per day",
       main="Steps per day: Without NAs",col="red", ylim=c(0,20))
```

<img src="figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />

##Differences in activity patterns between weekdays and weekends

For this part, the clean dataset without NA values will be used. We will follow the next steps: 

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#1:Creating the new factor variable:
  clean_data$date <- as.Date(clean_data$date, "%Y-%m-%d")
  clean_data$day_of_week <- weekdays(clean_data$date) #Names are in spanish

  for(i in 1:nrow(clean_data)){
    if(clean_data[i,"day_of_week"] %in% c("sábado","domingo")){
      clean_data[i,"day"] <- "weekend"
    }
    else{
      clean_data[i,"day"] <- "weekday" 
    }
  }
  clean_data$day <- as.factor(clean_data$day)

#2:Panel plot containing a time series plot
  #Data containing the avg value of steps for weekends and weekdays: 
  avg_week_steps <- with(clean_data, tapply(steps, list(day,interval), mean))
  week_interval <- as.data.frame(t(avg_week_steps))
  week_interval$interval <- rownames(week_interval)

  week_avg <- data.frame(Steps = c(week_interval$weekday,week_interval$weekend),
                         Day = c(rep("Weekday",nrow(week_interval)),
                                 rep("Weekend",nrow(week_interval))),
                         Interval = c(as.numeric(rownames(week_interval)),
                                      as.numeric(rownames(week_interval))))
  week_avg$Day <- as.factor(week_avg$Day)
  
  #Plotting in ggplot2
  library(ggplot2)
  ggplot(week_avg, aes(Interval, Steps)) + geom_line(size=1, col="red")+
    facet_grid(. ~ Day)+
    theme_bw()+
    theme(panel.grid.minor=element_line(colour="lightgrey"))+
    theme(panel.grid.major=element_line(colour="grey"))
```

<img src="figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />

The two patterns are different. Whereas in the weekdays there is a peak on the moorning and the activity starts at the interval 500, on the weekends the activity begins at interval 750 and the steps are more distributed across the day. 
