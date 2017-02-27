
## 1 Reading in the activity file, formatting the Date column

activity = read.csv('C:/Users/swastikaa/Documents/Documents/DataScience/Course5/Week2/repdata%2Fdata%2Factivity/activity.csv',colClasses = c("numeric","character","numeric"))
head(activity)
activity$date = as.Date(activity$date,'%Y-%m-%d')

## 2.1 Calculating the total number of steps 

activitysum = aggregate(steps~ date, data=activity, sum, na.RM = TRUE)
activitysum


## 2.2 histogram of the total number of steps taken each day

hist(activitysum$steps, ylab="Steps", xlab="Day", main = "Steps by day")

# 2.3 Calculate mean and median of total steps per day
mean(activitysum$steps)

median(activitysum$steps)

# 3.1 Average Daily Pattern 

timeseries_plot <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
timeseries_plot

plot(row.names(timeseries_plot), timeseries_plot, type = "l", xlab = "5-min interval", 
     ylab = "Average for all Days", main = "Average number of steps taken"
)

max = which.max(timeseries_plot)
max

## 3.2 Imputing missing values with mean value
summary(activity)

library(Hmisc)
activity$imputed_steps = with(activity,impute(activity$steps, mean))
summary(activity)

activity1 = activity

activitysum1 = aggregate(imputed_steps~ date, data=activity1, sum, na.RM = TRUE)
activitysum1

hist(activitysum1$imputed_steps, ylab="Steps", xlab="Day", main = "Steps by day")

# 3.3 Calculate mean and median of total steps per day
mean(activitysum1$imputed_steps)

median(activitysum1$imputed_steps)



## 4 Are there differences in activity patterns between weekdays and weekends?

day <- weekdays(activity1$date)
daylevel <- vector()
for (i in 1:nrow(activity1)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activity1$daylevel <- daylevel
activity1$daylevel <- factor(activity1$daylevel)

stepsByDay <- aggregate(imputed_steps ~ interval + daylevel, data = activity1, mean)
names(stepsByDay) <- c("interval", "daylevel", "imputed_steps")

##4.2 Panel Plot

xyplot(imputed_steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")


