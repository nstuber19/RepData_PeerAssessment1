## Reproducible Research
## Course Project 1

## Nick Stuber




## Setting working directory to where data is located
getwd()
setwd("C:/Users/nstuber/Desktop/Optum Data Management/Data Science/Coursera/Reproducible Research/Week2/hw1")
getwd()

## Load in raw data into dataframe
rawData <- read.csv("activity.csv", 
                      sep = ',', 
                      header = TRUE,
                      na.strings = "NA"
)




# Q: What is mean total number of steps taken per day?

## Removing rows with 'NA' values in the steps column
updatedData <- rawData[!(is.na(rawData$steps)), ]

## 1.
## Summing steps by the dates
stepsByDate <- aggregate(steps ~ date, updatedData, sum)

## Changing date column from 'factor' to 'date' (POSIXt)
stepsByDate$date <- strptime(stepsByDate$date, format = "%Y-%m-%d")

## 2.
## Create histogram of steps taken per day
hist(stepsByDate$steps, 
     main = "Histogram of total number of steps per day",
     xlab = "Steps per day")

## 3. (a)
## Calculating the mean of total number of steps taken per day
mean(stepsByDate$steps)

## 3. (b)
## Calculating the median of total number of steps taken per day
median(stepsByDate$steps)




# Q: What is the average daily activity pattern?

## Summing steps by the interval
stepsByInterval <- aggregate(steps ~ interval, updatedData, mean)

## 1.
## Graphing time series of average steps taken (averaged across all days) by 5-minute intervals
with(stepsByInterval, 
     plot(interval, 
          steps, 
          type = "l", 
          main = "Avg number of steps over 5-min intervals",
          ylab = "Avg steps taken (averaged across all days)",
          xlab = "5-minute Intervals"))

## 2.
## Identifying 5-minute interval where the most steps (on average) occur
stepsByInterval[which(stepsByInterval$steps == max(stepsByInterval$steps)),]




## Imputing missing values

## 1.
## Calculating the number of rows with NA values in the 'steps' column
sum(is.na(rawData$steps))


## 2. & 3.
## Create a new dataset that is equal to the original dataset but with the missing data filled in.
completeData <- rawData

## Creating loop to fill in NA values with the average value of its specific interval value
for (i in 1:nrow(completeData)) {
    
    if (is.na(completeData[i, 1]) == TRUE) {
        
        rowInterval <- completeData[i, 3]
        dataSubset <- subset(completeData, interval == rowInterval)
        completeData[i, 1] <- round(mean(dataSubset$steps, na.rm = TRUE))
        
    }
    
}


## 4.
## Summing steps by the dates
stepsByDateComplete <- aggregate(steps ~ date, completeData, sum)

## Create histogram of steps taken per day
hist(stepsByDateComplete$steps, 
     main = "Histogram of total number of steps per day",
     xlab = "Steps per day")




## Are there differences in activity patterns between weekdays and weekends?

## 1.
## Changing date column from 'factor' to 'date' (POSIXt)
completeData$date <- strptime(completeData$date, format = "%Y-%m-%d")

## Creating list of possible weekdays
weekdaysList <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

## Adding column to dataset that will list if date is a weekday/weekend (initially setting value to NA)
completeData$day <- NA

## Creating loop to evaluate row date and add corresponding weekday/weekend value
for (i in 1:nrow(completeData)) {
    
    rowDate <- completeData[i, 2]
    
    if (weekdays(rowDate) %in% weekdaysList) {
        
        completeData[i, 4] <- "weekday"
        
    } 
    
    else {
        
        completeData[i, 4] <- "weekend"
        
    }
    
}


## 2.

library(lattice)

## Creating subsets of the complete data, weekday vs weekend
weekdayData <- subset(completeData, day == "weekday")
weekendData <- subset(completeData, day == "weekend")

stepsByIntervalWeekday <- aggregate(steps ~ interval , weekdayData, mean)
stepsByIntervalWeekday$day <- "weekday"
stepsByIntervalWeekend <- aggregate(steps ~ interval , weekendData, mean)
stepsByIntervalWeekend$day <- "weekend"

stepsByIntervalDays <- rbind(stepsByIntervalWeekday, stepsByIntervalWeekend)

## Plotting weekend and weekday data
xyplot(steps ~ interval | day, 
       data = stepsByIntervalDays, 
       type = "l", 
       ylab = "Number of Steps", 
       xlab = "Interval", 
       ylim = c(0, 250), 
       layout = c(1, 2))






