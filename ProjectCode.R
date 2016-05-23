
library(readr) ; library(ggplot2) ; library(plyr) ; library(lattice) ; library(knitr)
# Getting/organizing the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, "./data/repdata%2Fdata%2Factivity.zip")
unzip("./data/repdata%2Fdata%2Factivity.zip", exdir = "./data")
fpath <- file.path("./data")
filelist <- list.files(fpath, recursive = TRUE)
filelist
fileName <- "./data/activity.csv"

# Reading/reviewing of data source
activ <- read.csv(fileName)
summary(activ)
rows <- nrow(activ) ; cols <- ncol(activ)
rows ; cols

# convert date field to date type with only weekdays
activ$day <- weekdays(as.Date(activ$date))
activ$DateTime<- as.POSIXct(activ$date, format="%Y-%m-%d")

myCleanData <- activ[!is.na(activ$steps),]

# mean number of steps take per day/date
sumData <- aggregate(activ$steps ~ activ$date, FUN = sum, )
colnames(sumData)<- c("Date", "Steps")

# plot data to histogram of steps per day/date
hist(sumData$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day")

## Mean of Steps
as.integer(mean(sumData$Steps))
## Median of Steps
as.integer(median(sumData$Steps))

##create average number of steps per interval
interData <- ddply(myCleanData, .(interval), summarize, Avg = mean(steps))

##Create line plot of average number of steps per interval
p <- ggplot(interData, aes(x = interval, y = Avg), xlab = "Interval", ylab = "Avg Number of Steps")
p + geom_line() + xlab("Interval") + ylab("Avg Number of Steps")+ggtitle("Avg Number of Steps per Interval")

##Maximum steps by interval
maxStep <- max(interData$Avg)

##Which interval contains the maximum average number of steps
interData[interData$Avg == maxStep, 1]

##Number of NAs in original data set
nrow(activ[is.na(activ$steps),])

## Create the average number of steps per weekday and interval
avgData <- ddply(myCleanData, .(interval, day), summarize, Avg = mean(steps))

## Create dataset with all NAs for substitution
naData<- activ[is.na(activ$steps),]
## Merge NA data with average weekday interval for substitution
newData <- merge(naData, avgData, by = c("interval", "day"))

## Reorder the new substituded data in the same format as clean data set
newData2<- newData[,c(6,4,1,2,5)]
colnames(newData2)<- c("steps", "date", "interval", "day", "DateTime")

##Merge the NA averages and non NA data together
mergedData <- rbind(myCleanData, newData2)

##Create sum of steps per date to compare with step 1
sumData2 <- aggregate(mergedData$steps ~ mergedData$date, FUN = sum, )
colnames(sumData2)<- c("Date", "Steps")

## Mean of Steps with NA data taken care of
as.integer(mean(sumData2$Steps))

## Median of Steps with NA data taken care of
as.integer(median(sumData2$Steps))

## Creating the histogram of total steps per day, categorized by data set to show impact
hist(sumData2$Steps, breaks = 5, xlab = "Steps", 
     main = "Total Steps per Day with NAs Fixed", col = "Blue")
hist(sumData$Steps, breaks = 5, xlab = "Steps", 
     main = "Total Steps per Day with NAs Fixed", col = "Dark Grey", add = T)
legend("topleft", c("Imputed Data", "Non-NA Data"), fill = c("Blue", "Dark Grey") )

## Create new category based on the days of the week
mergedData$DayCategory <- ifelse(mergedData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

## Summarize data by interval and type of day
interData2 <- ddply(mergedData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg ~ interval|DayCategory, data = interData2, type = "l",  layout = c(1,2),
       main="Avg Steps per Interval Based on Type of Day", 
       ylab="Avg Number of Steps", xlab = "Interval")
