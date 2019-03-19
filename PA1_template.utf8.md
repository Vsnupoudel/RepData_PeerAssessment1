---
title: "PA1_template.Rmd"
author: "Bishnu Poudel"
date: "March 19, 2019"
output: html_document
---

# Week 2 Assignment for Reporducible Research



## Task 1

What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
data<-read.csv("activity.csv")
#calculate sum of each day
daysum<- data.frame(  steps_per_day= tapply( data$steps, data$date, sum ) 
                , date= names(  tapply( data$steps,data$date, sum )  ) ) 
row.names(daysum)<-NULL
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
#Histogram
library(ggplot2)
g<- ggplot(daysum, aes(daysum$steps_per_day) )
g+geom_histogram( binwidth=2500, color="red" )+geom_vline(xintercept
=mean(daysum$steps_per_day, na.rm=TRUE),lwd=3, color="gray" ) +geom_vline(xintercept
=median(daysum$steps_per_day, na.rm=TRUE),lwd=1, color="green" )
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" width="672" />

3. Calculate and report the mean and median of the total number of steps taken per day


```r
#display the mean and median which are very close
mean(daysum$steps_per_day, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(daysum$steps_per_day, na.rm=TRUE)
```

```
## [1] 10765
```

## Task 2
What is the average daily activity pattern?

1. Make a time series plot ( type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#Plot of each five minute interval. ( 12*24) There should be 288 in the x axis 
# could've replaced NA in steps with zeros
# tapply with na.rm=TRUE, this gives different mean
inter1<-tapply( data$steps, data$interval, function(x) mean(x, na.rm=TRUE) ) 
inter1 <- data.frame( interval= names(inter1), steps=inter1)
row.names(inter1)<-NULL
inter<-inter1
inter1$interval <-as.numeric ( as.character(inter1$interval))
inter$interval<- sprintf( "%04d", as.numeric ( as.character(inter$interval)) )

#plot
g<- ggplot(inter, aes(interval, steps, group=1))
br<-c("0100","0400","0700","1000","1300","1600","1900","2200")
g+geom_line(color="red")+geom_point(lwd=0.3)+xlab("interval")+scale_x_discrete(breaks=br)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" width="672" />

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#Interval where steps are max
inter[inter$steps==max(inter$steps ),]$interval 
```

```
## [1] "0835"
```

## Task 3
### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with (NAs)

```r
sum( apply ( !is.na(data), 1, all ) )
```

```
## [1] 15264
```

```r
#Total rows without any NA values     
dim(data)[1] - sum( apply ( !is.na(data), 1, all ) )
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#Imputing missing value by putting 
#Average for that time interval as calculated in the inter dataset in task 2
# 1. merge with inter first 
mrge<-merge( data.frame(data[ which( is.na(data$steps)),]) , inter1, by.x="interval", by.y="interval", all.x=TRUE )
#2. merge the above dataset with the main dataset 
m<- merge(data, mrge, by.x=c("interval","date"), by.y=c("interval","date") , all.x=TRUE)
#in the dataset 'm' now, put the NAs in the $step column
m[ which( is.na(m$steps)),]$steps<-m[ which( is.na(m$steps)),]$steps.y
m$steps.x<-NULL ; m$steps.y<-NULL
## m is the dataset for question 3 of this task
```
### *m is the dataset for question 3 of this task*

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## Now for task 4
#calculate sum of each day
dsum<- data.frame(  total.steps= tapply( m$steps, m$date, sum ) 
                      , date= names( tapply( m$steps,m$date, sum )  )) 
row.names(dsum)<-NULL
#Histogram
g<- ggplot(dsum, aes(dsum$total.steps) )
g+geom_histogram( binwidth=2500, color="red" )+geom_vline(xintercept
=mean(dsum$total.steps, na.rm=TRUE),lwd=3, color="gray" ) +geom_vline(xintercept
=median(dsum$total.steps, na.rm=TRUE),lwd=1, color="green" )+labs(title=
"Sum of Steps Daywise")+xlab("steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-18-1.png" width="672" />

```r
#display the mean and median which are very close
mean(dsum$total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(dsum$total.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#Oops!!! Means are same as before as 
#I imputed missing values with the means !!!!!
```
##### *Oops!!! Means are same as before as *
#####  *I imputed missing values with the means !!!!! *

## Task 4

Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. 
Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
#function to return weekend or weekday based on result of the already present weekdays() function
wkend_wkday<- function (x=data.frame){
    wd<-c ("Monday"  ,  "Tuesday" ,  "Wednesday" ,"Thursday",  "Friday" )
      if (x %in% wd) { return("WeekDay") }
    else{ return("WeekEnd")   }
}

#use the dsum dataset from the previous solution ( with imputed NAs )
weekd<- dsum
weekd$date<- as.Date( as.character(weekd$date))
weekd$dayofweek<-weekdays( weekd$date )
weekd$wd.we <- sapply(weekd$dayofweek,  wkend_wkday)
head(weekd)
```

```
##   total.steps       date dayofweek   wd.we
## 1    10766.19 2012-10-01    Monday WeekDay
## 2      126.00 2012-10-02   Tuesday WeekDay
## 3    11352.00 2012-10-03 Wednesday WeekDay
## 4    12116.00 2012-10-04  Thursday WeekDay
## 5    13294.00 2012-10-05    Friday WeekDay
## 6    15420.00 2012-10-06  Saturday WeekEnd
```

2. Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#plot
g<- ggplot(weekd, aes(date, total.steps, group=1))
g+facet_grid(.~wd.we)+geom_line(color="red")+geom_point(lwd=0.3)+xlab("
interval")+scale_x_date()+theme(axis.text.x=element_text(size=8)   )
```

<img src="PA1_template_files/figure-html/unnamed-chunk-20-1.png" width="672" />


