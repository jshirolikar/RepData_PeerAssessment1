library("stats")
library("xlsx")
library("plyr")
library("stringr")
library("dplyr")

activitydocorig <- read.csv2("C:\\coursera\\coursera\\Reproducible Research\\activity.csv", na.string = "", sep = ",")
activitydoc <- subset(activitydocorig, activitydocorig$steps != "NA")

missingactivitydoc <- subset(activitydocorig, activitydocorig$steps == "NA")


activitydoc$steps <- as.numeric(activitydoc$steps)
activitydoc$date <- as.Date(activitydoc$date)


sum_activitydoc <- aggregate(activitydoc$steps, list(activitydoc$date), sum)
hist(sum_activitydoc$x, xlab = "Total steps per day")

print(mean(sum_activitydoc$x))
print(median(sum_activitydoc$x))

average_dailyactivitydoc <- aggregate(activitydoc$steps, list(activitydoc$interval), mean)
plot(y=average_dailyactivitydoc$x, x=average_dailyactivitydoc$Group.1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_dailyactivitydoc$Group.1))
axis(2, at = c(average_dailyactivitydoc$x))

#print interval with max steps 
average_dailyactivitydoc$Group.1[which(average_dailyactivitydoc$x == max(average_dailyactivitydoc$x))]




missingactivitydoc$steps <- as.numeric(missingactivitydoc$steps)
missingactivitydoc$interval <- as.numeric(missingactivitydoc$interval)

j = 0

for (j in 0:7)
{
    missingactivitydoc$steps[((j*288) + 1) : ((j+1)*288)] <- average_dailyactivitydoc$x[1:288]
    
 
}


newactivitydoc <- rbind(activitydoc, missingactivitydoc)
newactivitydoc$steps <- as.numeric(newactivitydoc$steps)
newactivitydoc$interval <- as.numeric(newactivitydoc$interval)

sum_activitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$date), sum)
hist(sum_activitydoc$x, xlab = "Total steps per day", main = "Histogram of total steps per day")
print(mean(sum_activitydoc$x))
print(median(sum_activitydoc$x))


newactivitydoc$weekday <- weekdays(newactivitydoc$date)
newactivitydoc2 <- newactivitydoc
newactivitydoc <- subset(newactivitydoc, newactivitydoc$weekday == "Sunday" | newactivitydoc$weekday== "Saturday")

average_weekendactivitydoc <- aggregate((newactivitydoc$steps), list(newactivitydoc$interval), mean)

plot(y=average_weekendactivitydoc$x,x= average_weekendactivitydoc$Group.1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_weekendactivitydoc$Group.1))
axis(2, at = c(average_weekendactivitydoc$x))


#weekdays
newactivitydoc <- newactivitydoc2
newactivitydoc <- subset(newactivitydoc, newactivitydoc$weekday != "Sunday" & newactivitydoc$weekday != "Saturday")

average_weekdayactivitydoc <- aggregate(newactivitydoc$steps, list(newactivitydoc$interval), mean)

plot(y=average_weekdayactivitydoc$x,x= average_weekdayactivitydoc$Group.1,   type = "l", xlab = "Interval", ylab = "Steps", xaxt = "n", yaxt = "n")
axis(1, at=c(average_weekdayactivitydoc$Group.1))
axis(2, at = c(average_weekdayactivitydoc$x))


