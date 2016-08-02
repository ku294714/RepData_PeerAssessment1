# PA1_template
Ankit  
July 31, 2016  
## Load the libraries

```r
library(ggplot2)
library(plyr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:plyr':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```
## Loading and preprocesssing the data

```r
unzip("repdata%2Fdata%2Factivity.zip")
activitydata<-read.csv("activity.csv")
#head(activitydata)
```

## What is mean total number of steps taken per day?

```r
#sumofstepperday<-aggregate(activitydata$steps,by=list(activitydata$date),FUN=sum)
sumofstepperday<-ddply(activitydata, c("date"),summarise,
                   totalsteps=sum(steps,na.rm=TRUE)
                   )
hist(sumofstepperday$totalsteps,xlab="SumOfStepsPerDay",main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


The mean total number of steps taken per day is 9354.2295082. The median number of steps taken per day is 10395(NA's omitted).


## What is the average daily activity pattern?

```r
#meanofstepsperday<-aggregate(activitydata$steps,by=list(activitydata$date),FUN=mean)
stepsper5min<-ddply(activitydata, c("interval"),summarise,
                    meansteps = mean(steps,na.rm=TRUE)
                    )
plot(stepsper5min$interval,stepsper5min$meansteps,type="l",ylab = "Mean_ Steps",xlab="5-minute interval",main="Average steps for each 5-min interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


The five minute interval with the highest mean step-count is interval:
835


## Imputing missing values

There are 2304 missing values in the dataset.

Strategy to fill all the mssing values.


```r
step_interpolation <- function(rownumber){
  prevrow=rownumber;
  nextrow=rownumber;
  while(is.na(activitydata$steps[prevrow])){
    prevrow=prevrow-1
    if(prevrow<1)return(mean(activitydata[activitydata$interval==activitydata$interval[rownumber],"steps"],na.rm=TRUE))
  }
  while(is.na(activitydata$steps[nextrow])){
    nextrow=nextrow+1
    if(nextrow>nrow(activitydata))return(mean(activitydata[activitydata$interval==activitydata$interval[rownumber],"steps"],na.rm=TRUE))
  }
  return(
    (activitydata$steps[prevrow]+activitydata$steps[nextrow])/2
  )
}

activity_guessNA <-activitydata
for(n in 1:nrow(activitydata)){
  if(is.na(activitydata$steps[n])){
    activity_guessNA$steps[n]=step_interpolation(n);
  }
}
```
Histogram of the total number of steps taken each day


```r
sumofstepperday2<-ddply(activity_guessNA,c("date"),summarise,
                        totalsteps=sum(steps,na.rm = TRUE))
hist(sumofstepperday2$totalsteps,xlab="SumOfStepsPerDay",main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


Mean 9707.219301 and Median 1.0571\times 10^{4} of total number of steps per day after merging NAs.

Impact: Mean 9354.2295082 and Median 10395 of total number of steps per day with NAs not merged.


## Are there differences in activity patterns between weekdays and weekends?


```r
#head(activity_guessNA)

paindays= c("Monday","Tuesday","Wednesday","Thursday","Friday")

date_1<-ymd(activity_guessNA$date)

activity_guessNA$weekday<-as.factor(ifelse(weekdays(date_1)%in%paindays,"weekday","weekend"))

#head(activity_guessNA$weekday)

stepsperinterval.weekdaysplit<-ddply(activity_guessNA, c("interval","weekday"),summarise,
                    meansteps = mean(steps,na.rm=TRUE))

weekdayplot<-ggplot(stepsperinterval.weekdaysplit,aes(x=interval,y=meansteps))+
  facet_wrap(~weekday,nrow=2,ncol=1)+
  geom_line()+
  ggtitle("Mean steps over each 5min interval split by weekday/weekend")+
  ylab("Mean steps")+
  xlab("Interval number")
print(weekdayplot)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


