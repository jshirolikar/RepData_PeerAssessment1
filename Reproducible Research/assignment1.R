require("xlsx")
library("xlsx")
library("plyr")
library("stringr")
library("dplyr")

activitydocorig <- read.csv2("C:\\coursera\\coursera\\Reproducible Research\\activity.csv", na.string = "", sep = ",")
activitydoc <- activitydocorig[complete.cases(activitydoc),]


activitydoc <- activitydoc[289:17568,]

activitydoc$steps <- as.numeric(activitydoc$steps)
activitydoc$date <- as.Date(activitydoc$date)


sum_activitydoc <- aggregate(activitydoc$steps, list(activitydoc$date), sum)
hist(sum_activitydoc$x, xlab = "Total steps per day")

print(mean(sum_activitydoc$x))
print(median(sum_activitydoc$x))

average_dailyactivitydoc <- aggregate(activitydoc$steps, list(activitydoc$interval), mean)
plot(x=average_dailyactivitydoc$x,y= average_dailyactivitydoc$Group1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_dailyactivitydoc$Group1))
axis(2, at = c(average_dailyactivitydoc$x))

#print interval with max steps
average_dailyactivitydoc$Group.1[which(average_dailyactivitydoc$x == max(average_dailyactivitydoc$x))]


newactivitydoc <- activitydocorig[1:288,]
newactivitydoc$steps <- as.numeric(newactivitydoc$steps)
newactivitydoc$date <- as.Date(newactivitydoc$date)
newactivitydoc$steps[1:288] <- average_dailyactivitydoc$x[1:288]

newactivitydoc <- rbind(newactivitydoc, activitydoc)

sum_activitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$date), sum)
hist(sum_activitydoc$x, xlab = "Total steps per day")
print(mean(sum_activitydoc$x))
print(median(sum_activitydoc$x))


newactivitydoc$weekday <- weekdays(newactivitydoc$date)
newactivitydoc <- subset(newactivitydoc, newactivitydoc$weekday == "Sunday" | newactivitydoc$weekday== "Saturday")

average_weekendactivitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$interval), mean)

plot(x=average_weekendactivitydoc$x,y= average_weekendactivitydoc$Group1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_weekendactivitydoc$Group1))
axis(2, at = c(average_weekendactivitydoc$x))


#weekdays
newactivitydoc <- activitydocorig[1:288,]
newactivitydoc$steps <- as.numeric(newactivitydoc$steps)
newactivitydoc$date <- as.Date(newactivitydoc$date)
newactivitydoc$steps[1:288] <- average_dailyactivitydoc$x[1:288]

newactivitydoc <- rbind(newactivitydoc, activitydoc)

sum_activitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$date), sum)
hist(sum_activitydoc$x, xlab = "Total steps per day")
print(mean(sum_activitydoc$x))
print(median(sum_activitydoc$x))


newactivitydoc$weekday <- weekdays(newactivitydoc$date)
newactivitydoc <- subset(newactivitydoc, newactivitydoc$weekday != "Sunday" & newactivitydoc$weekday != "Saturday")

average_weekdayactivitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$interval), mean)

plot(x=average_weekdayactivitydoc$x,y= average_weekdayactivitydoc$Group1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_weekdayactivitydoc$Group1))
axis(2, at = c(average_weekdayactivitydoc$x))


