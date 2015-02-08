##loading and reprocessing data
 
setwd("E:\\Coursera\\Reproducible Research\\assignment1")
data<-read.csv(file="activity.csv", header = TRUE, sep = ",")
head(data)

data$date<-as.Date(data$date)
dataComplete<-na.omit(data)

summary(dataComplete)


##mean total number of steps taken per day
steps_per_day<-aggregate( steps~date,data=dataComplete, sum)
plot<-hist(steps_per_day$steps , breaks = 7,
        main = "Frequency of number of steps per day", 
        xlab = "Number of steps per day", 
        ylab = "Frequency", col = "grey"
)

mean(steps_per_day$steps)
median(steps_per_day$steps)


##the average daily activity pattern

steps_by_interval <- aggregate(steps ~ interval, dataComplete, mean)

max<-steps_by_interval[which.max(steps_by_interval$steps),]
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", 
        main = "Average Number of Steps by Interval",
        xlab = "Interval", ylab="Number of Steps",
)
abline(v = max$interval, h = max$steps,
        untf = TRUE, col="blue", lty=2
)

max


##deal with missing value
sapply(data, function(x) sum(is.na(x)))
incomplete <- sum(!complete.cases(data))

aggregate( steps~date,data=dataComplete, mean, na.rm = TRUE)


for (i in 1:nrow(data)){
        if (is.na(data$steps[i])){
            interval_val <- data$interval[i]
            row_id <- which(steps_by_interval$interval == interval_val)
            steps_val <- steps_by_interval$steps[row_id]
            data$steps[i] <- steps_val
        }
}


head(data)
head(steps_by_interval)

##mean total number of steps taken per day
steps_per_day_IM<-aggregate( steps~date,data=data, sum)

hist(steps_per_day_IM$steps , breaks = 7,
        main = "Frequency of number of steps per day", 
        xlab = "Number of steps per day", 
        ylab = "Frequency", col = "blue"
)

hist(steps_per_day$steps, main = paste("Total Steps Each Day"), 
        col="grey",  density=50, xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "grey"), lwd=10)


mean(steps_per_day_IM$steps)
median(steps_per_day_IM$steps)

##differences in activity patterns between weekdays and weekends
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data$day = as.factor(ifelse(is.element(weekdays(as.Date(data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + day, data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$day, 
        main="Average Steps per Day by Interval",
        xlab="Interval", ylab="Steps",layout=c(1,2), type="l")


