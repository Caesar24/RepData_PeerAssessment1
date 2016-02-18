library(lattice)

# Remove all objects from the current workspace
rm(list = ls())

# Set local directory
setwd("C:/dev/R/reproducible-research/RepData_PeerAssessment1")

# download the file to our local local folder only if we haven't yet
print("Loading Data...")
if ( !file.exists("activity.csv") )
{
    localFile <- paste(getwd(),"/activity.zip",sep = "")
    
    if ( !file.exists("activity.zip") )
    {
        
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile = localFile)
    }
        
    # and unzip it to our local folder as well.
    unzip(localFile)
}

## 1. Loading and preprocessing the data (Code for reading in the dataset and/or processing the data)
inputData <- read.csv('activity.csv')
print("Data loaded.")

aggrStepsByDay <- with( inputData,
                        aggregate(steps ~ date, 
                                  data = inputData, 
                                  FUN = "sum", 
                                  na.rm = TRUE ) )          # Calculate the total number of steps taken per day
print("1. Data read and pre-processed.")


## 2. Histogram of the total number of steps taken each day
hist(aggrStepsByDay$steps,
     main = "2. Total Number of Steps by day",
     xlab = "Steps" )                               # Plot the histogram
print("2. Histogram Created")

## 3. Mean and median number of steps taken each day
meanSteps <- mean(aggrStepsByDay$steps)             # Mean steps per day
print( paste("3. Mean Steps taken per day:", meanSteps))
medianSteps <- median(aggrStepsByDay$steps)         # Median steps taken per day
print( paste("3. Median Steps taken per day:", medianSteps))

## 4. Time series plot of the average number of steps taken per time interval
fiveMinIntervalStepsAverage <- with( inputData, 
                                     aggregate(steps ~ interval, 
                                               data = inputData, 
                                               FUN = "mean", 
                                               na.rm = TRUE) )
plot(fiveMinIntervalStepsAverage$interval,
     fiveMinIntervalStepsAverage$steps,
     type = "l",
     main = "4. Time Series: Avg. Steps taken",
     xlab = "Interval",
     ylab = "Steps" )
print("4. Plot Created")

## 5. The 5-minute interval that, on average, contains the maximum number of steps
avgMaxStepsInterval <- fiveMinIntervalStepsAverage$interval[ which.max(fiveMinIntervalStepsAverage$steps) ]
print( paste("5. Interval Avg. Max Steps:", avgMaxStepsInterval) )

## 6. Code to describe and show a strategy for imputing missing data
## 6.1 Calculate and report the total number of missing values in the dataset 
##     (i.e. the total number of rows with NAs)
missingValCount <- sum(is.na(inputData$steps))
print( paste("6.1 Missing Values Count:", missingValCount) )

## 6.2 Devise a strategy for filling in all of the missing values in the dataset. 
##     The strategy does not need to be sophisticated. For example, you could use 
##     the mean/median for that day, or the mean for that 5-minute interval, etc.
##     In this example we use the average for that interval to fill out the missing 
##     values
filledData <- inputData
filledData$steps[is.na(filledData$steps)] <- fiveMinIntervalStepsAverage$steps

## 6.3 Create a new dataset that is equal to the original dataset but with the 
##     missing data filled in.
complStepsByDay <- with( filledData, 
                         aggregate(steps ~ date, 
                                   data = filledData, 
                                   FUN = "sum", 
                                   na.rm = TRUE))   # Calculate the total number of steps taken per day
## 7 Make a histogram of the total number of steps taken each day and Calculate 
##     and report the mean and median total number of steps taken per day. Do 
##     these values differ from the estimates from the first part of the s
##     assignment? What is the impact of imputing missing data on the estimates of 
##     the total daily number of steps?
hist(complStepsByDay$steps,
     main = "7. Total Number of Steps by day (Filled Data)",
     xlab = "Steps" )                               # Plot the histogram
print("7. Histogram Created")
complMeanSteps <- mean(aggrStepsByDay$steps)             # Mean steps per day
print( paste("7. Mean Steps taken per day (Filled Data):", complMeanSteps))
complMedianSteps <- median(aggrStepsByDay$steps)         # Median steps taken per day
print( paste("7. Median Steps taken per day (Filled Data):", complMedianSteps))

## 8. Panel plot comparing the average number of steps taken per 5-minute 
##    interval across weekdays and weekends

## 8.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
##     indicating whether a given date is a weekday or weekend day.
filledDataLen <- length(filledData$date)                # GEt size of data frame
filledData$DayType <- 0                                 # add a column
for ( pos in 1:filledDataLen )                          # TODO: apply might be more efficient than a for loop
{
    # Fille the new column value depending on the day type
    filledData$DayType[pos] <- ifelse( with(filledData, weekdays( as.Date( date[pos] ) ) ) < "Saturday", "Weekday", "Weekend") 
}
dayTypeNewAvg <- aggregate(steps ~ interval + DayType,
                           data = filledData, 
                           FUN = "mean", 
                           na.rm = TRUE)                # Aggregate using interval and daytpe.
print( "8.1 New Average calculated (Day Type)")

with( dayTypeNewAvg, 
      xyplot(steps ~ interval | DayType, 
             data = dayTypeNewAvg, 
             type = "l", 
             layout = c(1, 2), 
             xlab = "Interval", 
             ylab = "Steps" ) )
print("8.2. Plot Created")
