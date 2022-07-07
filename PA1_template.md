---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data





```r
# Read data
data <- read.csv("C:/Users/bpu313718/Desktop/coursera/RepData_PeerAssessment1/RepData_PeerAssessment1-master/activity/activity.csv", header = TRUE)

# Convert date to Date
data$date <- as.Date(data$date)

# Convert interval to HH:MM format by padding with leading zeros
data$interval <- 
  format(strptime(formatC(data$interval, width = 4, format = "d", flag = "0"), format="%H%M"), format = "%H:%M")
```

## What is mean total number of steps taken per day?


```r
## Calculate total steps per day

sum_Steps <- data %>%  
          group_by(date) %>%
          summarise(total = sum(steps))


# Plot histogram of mean steps per day

ggplot(sum_Steps, aes(total)) + 
  geom_histogram(binwidth = 500) +
  labs(x = "Total Daily Steps", 
       y = "Frequency", 
       title = "Daily Steps")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
## Mean and median of number of steps per day
print("Mean number of steps per day")
```

```
## [1] "Mean number of steps per day"
```

```r
mean(sum_Steps$total, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
print("Median number of steps per day")
```

```
## [1] "Median number of steps per day"
```

```r
median(sum_Steps$total, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
interval_Steps <- data %>% 
  group_by(interval) %>%
  summarise(mean = mean(steps, na.rm = TRUE))

ggplot(interval_Steps, aes(as.POSIXct(interval, format = "%H:%M"), mean)) + 
  geom_line() +
  scale_x_datetime(labels = date_format("%H:%M"), 
                   date_breaks = "4 hours") +
  labs(x = "Five Minute Interval", 
       y = "Number of Steps", 
       title = "Average steps by time of day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## maximum number of average steps
print("maximum number of average steps")
```

```
## [1] "maximum number of average steps"
```

```r
interval_Steps[which.max(interval_Steps$mean),]
```

```
## # A tibble: 1 x 2
##   interval  mean
##   <chr>    <dbl>
## 1 08:35     206.
```


## Imputing missing values



```r
## total missing values 
print("total missing values")
```

```
## [1] "total missing values"
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
## New dataset that is equal to the original dataset but with the missing data filled in

impute_data <- data %>%
               group_by(interval) %>%
               mutate(steps = ifelse(is.na(steps),mean(steps,na.rm = TRUE), steps))

## Histogram of the total number of steps taken each day and Calculate and mean and median of total number of steps taken per day (computed using the new imputed data).

impute_sum_Steps <- impute_data %>% 
  group_by(date) %>%
  summarise(total = sum(steps))

ggplot(impute_sum_Steps, aes(total)) + 
  geom_histogram(binwidth = 500) +
  labs(x = "Total Daily Steps", 
       y = "Frequency", 
       title = "Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## Mean and median number of steps per day
print("Mean number of steps per day")
```

```
## [1] "Mean number of steps per day"
```

```r
mean(impute_sum_Steps$total, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
print("Median number of steps per day")
```

```
## [1] "Median number of steps per day"
```

```r
median(impute_sum_Steps$total, na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
# Create weekdays/weekend variable

wkday_impute_data <- impute_data %>%
  mutate(wkday = weekdays(date),wkday = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday" ))

# Turn this into Factor

wkday_impute_data <- wkday_impute_data %>%
  mutate( wkday = as.factor(wkday) )

# Group by interval and wkday (factor) and again calculate mean number of steps

interval_wkday_impute_data <- wkday_impute_data %>% 
  group_by(interval,wkday) %>%
  summarise(mean = mean(steps, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
# Create time series plot with faceting on the new wkday factor

ggplot(interval_wkday_impute_data, aes(as.POSIXct(interval, format = "%H:%M"), mean, col = wkday)) + 
  geom_line(show.legend = F) +
  facet_grid(rows = interval_wkday_impute_data$wkday) +
  scale_x_datetime(labels = date_format("%H:%M"), 
                   date_breaks = "4 hours") +
  labs(x = "Five Minute Interval", 
       y = "Number of Steps", 
       title = "Average steps by time of day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



