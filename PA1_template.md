# Reproducible Research: Peer Assessment 1

#### This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. 

#### Through this research we are attempting to use the existing data to analyse patterns through the whole time the data was collected, specific periods as well as daily patterns. In order to ensure reproducibility, all code used to reach the results is made available and the dataset can be obtained from: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip 

## Loading and preprocessing the data

#### The first step needed is to read the data, eliminate the data that is not available and prepare the data for the analysis. 


```r
require(ggplot2) 
```

```
## Loading required package: ggplot2
```

```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
require(graphics)
require(chron)
```

```
## Loading required package: chron
```

```r
require(lattice)
```

```
## Loading required package: lattice
```

```r
fileRead <- na.omit(read.csv('activity.csv', header = TRUE, na.strings = "NA"))
dayAggregate <- aggregate(fileRead$steps, list(date = fileRead$date), sum)
colnames(dayAggregate) <- c("dates", "steps")
```

## What is mean total number of steps taken per day?


```r
dayAggregate$dates <- as.Date(dayAggregate$dates, "%Y-%m-%d") # we first ensure the date format is propper
dayAggregate$steps <- as.numeric(dayAggregate$steps) # and we also ensure is can be treated as numbers
dates <- dayAggregate$dates # we then fix the columns
steps <- dayAggregate$steps
qplot(dates, steps) #to finally be able to graph the total mean number of steps per day
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
summary(dayAggregate)
```

```
##      dates                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

#### As seen in the graph above, the subject of study did not present a major change of trend in the total number of stpes per day during the time the data was analysed. It is however observed that there are some isolated days in which the level of activity increased or decresed dratically for which we will need to do further data analysis.  


## What is the average daily activity pattern?

```r
stepAggregate <- aggregate(fileRead$steps, list(interval = fileRead$interval), FUN = "mean") # Calculate the mean per interval
colnames(stepAggregate) <- c("interval", "averageSteps") 
plot(stepAggregate$interval, stepAggregate$averageSteps, type="l", xlab = "5-minute intervals", ylab = "Average number of steps taken") #plot the results 
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
summary(stepAggregate)
```

```
##     interval     averageSteps   
##  Min.   :   0   Min.   :  0.00  
##  1st Qu.: 589   1st Qu.:  2.49  
##  Median :1178   Median : 34.11  
##  Mean   :1178   Mean   : 37.38  
##  3rd Qu.:1766   3rd Qu.: 52.83  
##  Max.   :2355   Max.   :206.17
```

```r
maximumAverage <- arrange(stepAggregate, stepAggregate$averageSteps, decreasing = TRUE) # reorganise the results  
head(maximumAverage,1) # Display the result 
```

```
##   interval averageSteps
## 1      835        206.2
```

#### As seen in the daily activity pattern, we can observe that after a slow start, at 835 minute interval, the number of steps has its maximum value 

## Imputing missing values

```r
missingValues <- read.csv('activity.csv', header = TRUE, na.strings = "NA") #Read file again
sum(is.na(missingValues)) #count the number of missing values 
```

```
## [1] 2304
```

```r
# missingValues$steps[is.na(missingValues$steps)] <- with(missingValues, ave(steps, interval, FUN = function(x) median(x, na.rm = TRUE)))[is.na(missingValues$steps)]

# Replace NA values with the median number of steps 
newValues <- as.data.frame(ddply(missingValues, .(interval), transform, steps=ifelse(is.na(steps), median(steps, na.rm=TRUE), steps))) 

# Aggregate per date
newAggregate <- aggregate(newValues$steps, list(date = newValues$date), sum) 
colnames(newAggregate) <- c("dates2", "steps2")
dates2 <- newAggregate$dates2
steps2 <- newAggregate$steps2

# And plot new graph
qplot(dates2, steps2)
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

#### As it can in the plot above, there isn't a major difference between the results in the number of steps taken per day if the NA values are replaced with the media of the number of steps per interval 


## Are there differences in activity patterns between weekdays and weekends?

#### We will use the date from the previous point as a starting point 

#### For this analysis we will try to separate weekeend from weekndays so in the graph, TRUE represents days that are weekends and FALSE days that are not weekends. 


```r
weekend <- is.weekend(newAggregate$dates2) #Let's find out which dates are weekend
isWeekend <- as.data.frame(cbind(newAggregate, weekend)) #let's joing into one 
allWeekdays <- subset(isWeekend, weekend == "FALSE")
allWeekends <- subset(isWeekend, weekend == "TRUE")
barchart(dates2 ~ steps2 | weekend, data = isWeekend, xlab = "Number of steps for weekends and weekdays", ylab = "Dates analysed")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

#### As it can be seen in the graph above, there isn't a major difference between the number of steps taken during weekends and during weekdays 

