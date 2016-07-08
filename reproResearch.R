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

## Maximum number of steps in which interval
max(interval.steps)

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

## Updated histogram, mean, and median
steps <- aggregate(merged$steps, list(merged$date), FUN = "sum")
hist(steps[ ,2], breaks = 10, main = "Histogram of number of steps taken per day", xlab = "Number of steps", ylab = "Number of days")
mean(steps[ ,2], na.rm = TRUE)
median(steps[ ,2], na.rm = TRUE)

## Set weekdays
merged <- cbind(merged, day.of.week = weekdays(merged$date))

## Create factor with weekday/weekend
merged$day.of.week <- as.character(merged$day.of.week)
merged$day.of.week[merged$day.of.week == "Sunday" | merged$day.of.week == "Saturday"] <- "Weekend"
merged$day.of.week[merged$day.of.week != "Weekend"] <- "Weekday"
merged$day.of.week <- as.factor(merged$day.of.week)

## Mean number of steps taken for each interval during weekdays
steps.weekend <- aggregate(merged$steps[merged$day.of.week == "Weekend"], list(merged$interval[merged$day.of.week == "Weekend"]), FUN = "mean")
steps.weekday <- aggregate(merged$steps[merged$day.of.week == "Weekday"], list(merged$interval[merged$day.of.week == "Weekday"]), FUN = "mean")

## Panel plot
par(mfrow = c(2, 1),  oma = c(0, 1, 0, 0), mar = c(4, 4, 2, 2), cex = 0.8)
par(mai = c(0, 0.5, 0.6, 0.4))
plot(steps.weekend[ ,1], steps.weekend[ ,2], ylim = c(0, 200), type = "l", axes = FALSE, frame.plot = TRUE,
     xlab = "", ylab = "")
title("Weekend", line = -2)
Axis(side = 4, labels = TRUE)
mtext("Comparison of weekend vs. weekday step counts", side = 3, line = 1.5, cex = 1.1)
mtext("Mean steps taken", side = 2, line = -.5, cex = .9, outer = TRUE)

par(mai = c(0.6, 0.5, 0, 0.4))
plot(steps.weekday[ ,1], steps.weekday[ ,2], ylim = c(0, 200), type = "l", xlab = "", ylab = "")
title("Weekdays", line = -2)
mtext("Interval period (T.O.D.)", side = 1, line =1.8, cex = .9)
