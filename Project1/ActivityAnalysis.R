data <- read.csv("activity.csv", na.strings = "NA")
data$date <- as.Date(as.character(data$date))
library(dplyr)
total_steps_per_day <- summarise(group_by(data, date), 
                                 sum(steps, na.rm = TRUE))
names(total_steps_per_day) <- c("date", "total_steps")
hist(total_steps_per_day$total_steps, xlab = "Total Steps", 
     main = "Total Steps per Day", 
     col = c("blue", "green", "red", "yellow", "orange"),
     ylim = c(1,30))
mean(total_steps_per_day$total_steps)
median(total_steps_per_day$total_steps)
mean_steps_per_interval <- tapply(data$steps, data$interval, 
                                  mean, na.rm=TRUE)
plot(mean_steps_per_interval, 
     type="l", 
     main="Means Steps for Each Time Interval", 
     ylab="Number of Steps", xlab = "Time Interval")
mean_steps_per_interval <- aggregate(steps ~ interval, data, mean)
max_row <- which.max(mean_steps_per_interval$steps)
max_interval <- mean_steps_per_interval[max_row,]
bad <- data[is.na(data),]
num_bad <- nrow(bad)
percent_bad <- num_bad/nrow(data)
data_with_impute <- data
for ( i in 1:nrow(data_with_impute)) {
  if ( is.na(data_with_impute$steps[i])){
    interval <- data_with_impute$interval[i]
    row_id <- which(mean_steps_per_interval$interval==interval)
    impute_val <- mean_steps_per_interval$steps[row_id]
    data_with_impute$steps[i] <- impute_val
  }
}
##verify
sum(is.na(data_with_impute$steps))
sum(is.na(data$steps))
daily_totals_imputed <- summarise(group_by(data_with_impute, date), sum(steps, na.rm = TRUE))
names(daily_totals_imputed) <- c("date", "total_steps")

hist(daily_totals_imputed$total_steps, xlab = "Total Steps", main = "Total Steps per Day with NA Values Imputed to Time Interval Average", col = c("blue", "green", "red", "yellow", "orange"), ylim = c(1,30))
mean(daily_totals_imputed$total_steps)
median(daily_totals_imputed$total_steps)
data_with_impute$day <- weekdays(data_with_impute$date)
for ( i in 1:nrow(data_with_impute) ) {
  if ( data_with_impute$day[i] == "Saturday" || 
       data_with_impute$day[i] == "Sunday" ) 
  { data_with_impute$weekend[i] <- "weekend" } 
  else { data_with_impute$weekend[i] <- "weekday" }
}
data_with_impute$weekend <- as.factor(data_with_impute$weekend)
library(ggplot2)
qplot(interval, steps, 
      data=data_with_impute, geom=c("line"),
      xlab="Interval", ylab="Number of Steps", 
      main="Comparison of Weekday to Weekend") + 
  facet_wrap(~ weekend, ncol=1)


