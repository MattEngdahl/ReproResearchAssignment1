## Download and read source data

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode = "wb")
data <- unzip(temp, "activity.csv")
data <- read.csv(data)
unlink(temp)

