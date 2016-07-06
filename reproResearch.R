## Include packages
library(dplyr)

## Download and read source data

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode = "wb")
data <- unzip(temp, "activity.csv")
data <- read.csv(data)
unlink(temp)

## Data processing
data[,2] <- as.Date(data[,2], format = "%Y-%m-%d")

## Histogram of the total number of steps taken each day
steps <- aggregate(data$steps, list(data$date), FUN = "sum")

hist(steps[ ,2], breaks = 10, main = "Histogram of number of steps taken per day", xlab = "Number of steps", ylab = "Number of days")

## Mean number of steps taken per day
mean(steps[ ,2], na.rm = TRUE)
median(steps[ ,2], na.rm = TRUE)

## Time series plot
interval.steps <- aggregate(data$steps, list(data$interval), FUN = "mean", na.rm = TRUE)
plot(interval.steps[,1], interval.steps[,2], type = "l", main = "Average steps taken by interval", xlab = "Interval (time of day)", ylab = "Number of steps taken")

## Number of missing step values in data
sum(is.na(data$steps))

## Strategy; use median value for each relevant time interval as closest approximation
### 1. Calculate median steps for each interval
interval.steps.median <- aggregate(data$steps, list(data$interval), FUN = "median", na.rm = TRUE)
colnames(interval.steps.median) <- c("interval", "median.steps")

### 2. Merge data and interval.steps.median dataframes
merged <- dplyr::full_join(data, interval.steps.median, by = "interval")

### 3. Replace NA's in steps with values in interval.steps.median
merged$steps[is.na(merged$steps)] <- merged$median.steps[is.na(merged$steps)]

### 4. Delete interval.steps.median variable
merged <- merged[,1:ncol(merged) - 1]


