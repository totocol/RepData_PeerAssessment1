## Loading and preprocessing the data

require(ggplot2) 
require(plyr)
require(graphics)

fileRead <- na.omit(read.csv('activity.csv', header = TRUE, na.strings = "NA"))
dayAggregate <- aggregate(fileRead$steps, list(date = fileRead$date), sum)
colnames(dayAggregate) <- c("dates", "steps")

#head(dayAggregate)

# We then need to convert the dates to a proper date format

dayAggregate$dates <- as.Date(dayAggregate$dates, "%Y-%m-%d")

dayAggregate$steps <- as.numeric(dayAggregate$steps)

#head(dayAggregate)

#lapply(dayAggregate,colnames)

dates <- dayAggregate$dates
steps <- dayAggregate$steps
qplot(dates, steps)

#summary(fileRead)


## What is mean total number of steps taken per day?

# First we aggregate total number of steps by day 


#png(file = "histogram.png") #Create file 
#hist(dayAggregate$steps, col = 'red', xlab = "Date (year-month-day)", ylab = "Total number of steps (steps)", main = paste("Histogram of days per step"))
#dev.off() #close device


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
