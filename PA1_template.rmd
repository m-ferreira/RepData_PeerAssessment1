##Reproducible research Week 2 - Course Project 1

###1 Loading and preprocessing the data
```{r, echo=TRUE, results="hide"} 
data <- read.csv("C:/Users/gate/Documents/coursera/reproducible_research/week2/repdata%2Fdata%2Factivity/activity.csv")
```
```{r, echo=TRUE} 
summary(data)
```
###2 What is mean total number of steps taken per day?

**1. Histogram of the total number of steps taken each day**

```{r, echo=TRUE}
#install.packages("magrittr")
require(magrittr)
#install.packages("dplyr")
require(dplyr)

date_data <- data %>% select(date, steps) %>% group_by(date) %>%
	summarize(tsteps= sum	(steps)) %>%na.omit()
hist(date_data$tsteps, xlab = "Total number of steps taken each day",main="Histogram of the total number of steps taken each day", breaks = 20)
```


**2. Calculate and report the mean and median total number of steps taken per day**

```{r, echo=TRUE}
mean(date_data$tsteps)
```
```{r, echo=TRUE}
median(date_data$tsteps)
```

###3 What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```{r, echo=TRUE, results="hide"}
#install.packages("ggplot2")
require(ggplot2)
```

```{r, echo=TRUE}
five_minute <- aggregate(steps ~ interval, data = data, FUN =mean)
graph1 <- ggplot(data = five_minute, aes(x = interval, y = steps)) + 
    geom_line() +
    xlab("5-Minute Intervals") + 
    ylab("Total Number of Steps") +
    ggtitle("5-minute intervals and the average number of steps taken")
print(graph1)
```

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```{r, echo=TRUE}
five_minute[which(five_minute$steps == max(five_minute$steps)),]
```

###4 Imputing missing values

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```{r, echo=TRUE}
sapply(X = data, FUN = function(x) sum(is.na(x)))
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.** & **3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

Using the mean for that 5-minute interval to replace the missing values:

```{r, echo=TRUE, results="hide"}
#install.packages("magrittr")
require(magrittr)
#install.packages("dplyr")
require(dplyr)
```

```{r, echo=TRUE}
replace_na <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_data <- data%>% group_by(interval) %>% mutate(steps= replace_na(steps))
head(mean_data)
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

Creating a dataset with total number of steps taken per day:

```{r, echo=TRUE}
total_day <- aggregate(mean_data$steps, by=list(mean_data$date), sum)

names(total_day)[1] <- "date"
names(total_day)[2] <- "totalsteps"
summary(total_day)
```
Histogram of the total number of steps taken each day:

```{r, echo=TRUE}
hist(total_day$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total number of steps taken each day", breaks = 20)

```


Comparing the histograms before/after imputing missing data:


```{r, echo=TRUE}
par(mfrow=c(1,2))

hist(date_data$tsteps, xlab = "Total number of steps taken each day",main="Before Imput", breaks = 20)
hist(total_day$totalsteps, xlab = "Total number of steps taken each day", ylab = "Frequency", main = "After Imput", breaks = 20)
```


Comparing the mean and median before/after imputing missing data:


```{r, echo=TRUE}
mean_before <- mean(date_data$tsteps, na.rm = TRUE)
mean_after <- mean(total_day$totalsteps)
median_before <- median(date_data$tsteps, na.rm = TRUE)
median_after <- median(total_day$totalsteps)
```
Means:

```{r, echo=TRUE}
mean_before
mean_after
```
Medians:

```{r, echo=TRUE}
median_before
median_after
```
###5 Are there differences in activity patterns between weekdays and weekends?

**1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

```{r, echo=TRUE}
mean_data$date <- as.Date(mean_data$date)
mean_data$weekday <- weekdays(mean_data$date)
mean_data$weekend <- ifelse(mean_data$weekday=="Saturday" | mean_data$weekday=="Sunday", "Weekend", "Weekday" )

mean_data_week_weekend <- aggregate(mean_data$steps , by= list(mean_data$weekend, mean_data$interval), na.omit(mean))
names(mean_data_week_weekend) <- c("weekend", "interval", "steps")
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:**

```{r, echo=TRUE, results="hide"}
#install.packages("ggplot2")
require(ggplot2)
```

```{r, echo=TRUE}
ggplot(mean_data_week_weekend, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```