Peer-graded Assignment: Course Project 1

1.set option and load package

    knitr::opts_chunk$set(warning=FALSE)


    library(ggplot2)
    library(readr)
    activity <- read_csv("activity.csv")

    ## Parsed with column specification:
    ## cols(
    ##   steps = col_integer(),
    ##   date = col_date(format = ""),
    ##   interval = col_integer()
    ## )

2.Process/transform the data (if necessary) into a format suitable for
your analysis

    activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
    weekday <- weekdays(activity$date)
    activity <- cbind(activity,weekday)

What is mean total number of steps taken per day? 1. Calculate the total
number of steps taken per day

    activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
    names(activity_total_steps) <- c("date", "steps")

1.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

<!-- -->

    activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
    names(activity_total_steps) <- c("date", "steps")
    hist(activity_total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "red", ylim = c(0,20), breaks = seq(0,25000, by=2500))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

3.Calculate and report the mean and median of the total number of steps
taken per day

    mean(activity_total_steps$steps)

    ## [1] 9354.23

    median(activity_total_steps$steps)

    ## [1] 10395

What is the average daily activity pattern?

1.  Make a time series plot (i.e. ) of the 5-minute interval (x-axis)
    and the average number of steps taken, averaged across all days
    (y-axis)

<!-- -->

    average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
    names(average_daily_activity) <- c("interval", "mean")
    plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="red", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    average_daily_activity[which.max(average_daily_activity$mean), ]$interval

    ## [1] 835

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    average_daily_activity[which.max(average_daily_activity$mean), ]$interval

    ## [1] 835

Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as ). The presence of missing days may introduce bias into
some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with s)

<!-- -->

    sum(is.na(activity$steps))

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]

    activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
    names(total_steps_imputed) <- c("date", "daily_steps")

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    hist(total_steps_imputed$daily_steps, col = "red", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    mean(total_steps_imputed$daily_steps)

    ## [1] 10766.19

    median(total_steps_imputed$daily_steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and
weekends?

For this part the function may be of some help here. Use the dataset
with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
    activity$datetype <- sapply(activity$date, function(x) {
            if (weekdays(x) == "Sunday" | weekdays(x) =="Saturday") 
            {y <- "Weekend"} else 
            {y <- "Weekday"}
            y
    })

1.  Make a panel plot containing a time series plot (i.e. ) of the
    5-minute interval (x-axis) and the average number of steps taken,
    averaged across all weekday days or weekend days (y-axis). See the
    README file in the GitHub repository to see an example of what this
    plot should look like using simulated data.

<!-- -->

    activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
    plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
            geom_line() +
            labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
            facet_wrap(~datetype, ncol = 1, nrow=2)
    print(plot)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
