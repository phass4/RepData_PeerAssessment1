# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```{r load_data, echo=TRUE}
# Read in the Activity data as A
A <- read.csv("activity.csv")

# Generate a summary of A
summary(A)
```


## What is mean total number of steps taken per day?
```{r steps, echo=TRUE, fig.height=5, results=TRUE}
# Aggregate the data by day
A.byDate <- aggregate(A$steps, by=list(A$date), sum)

# Change the column names for ease of use
colnames(A.byDate) <- c("date", "steps")

# Generate the histogram for number of steps taken per day
hist(A.byDate$steps, main="Histogram of Steps Taken Per Day", xlab="Number of Steps")

# Compute the mean and median steps taken per day from tha data, excluding NA values
meanSteps <- mean(A.byDate$steps, na.rm=TRUE)
medianSteps <- median(A.byDate$steps, na.rm=TRUE)
```

Mean number of steps: `r meanSteps`  
Median number of steps: `r medianSteps`

## What is the average daily activity pattern?
Accounts for NA values being removed.
```{r daily, echo=TRUE, fig.height=5, results='asis'}
# Aggregate all data on the time intervals
A.byinterval <- aggregate(A$steps, by=list(A$interval), mean, na.rm=TRUE)

# Change the column names for ease of use
colnames(A.byinterval) <- c("time_of_day", "steps")

# Create a line graph of average activity over the daily time intervals
plot(A.byinterval$time_of_day, A.byinterval$steps, type="l", main="Average Daily Activity Pattern", ylab="Mean Number of Steps", xlab="Daily 5 Minute Time Interval")

#Compute the time interval with the average maximum number of steps
step_max <- A.byinterval$time_of_day[which.max(A.byinterval$steps)]
```

Interval with maximum number of steps: `r step_max`

## Imputing missing values

Strategy for filling in the data is to use the meadian for the time interval for all days to fill in the specific interval for A on the day in question.  
For example:
A$steps[1] <- A.byinterval$steps[A.byinterval$time_of_day == A.new$interval[1]]
```{r missing, echo=TRUE, fig.height=5, results='asis'}
# Determine the missing cases in A
missing <- !complete.cases(A)

# Compute the total number of missing items in A
missing_sum <- sum(!complete.cases(A))

# Create a new data set where we will fill in missing items
A.new <- A

# For each item missing a steps value, fill in with the average at that time interval for all other days
for (i in 1:nrow(A)) {
    if (is.na(A.new$steps[i])) {
        A.new$steps[i] <- A.byinterval$steps[A.byinterval$time_of_day == A.new$interval[i]]
    }
}

# Compute the number of missing cases in A.new
sum(!complete.cases(A.new))

# Aggregate the A.new data by date
A.new.byDate <- aggregate(A.new$steps, by=list(A.new$date), sum)

# Change the column names for ease of use
colnames(A.new.byDate) <- c("date", "steps")

# Create a histogram of the total steps taken per day with the fixed data
hist(A.new.byDate$steps, main="Histogram of Steps Taken Per Day (Fixed Data)", xlab="Number of Steps")

# Re-compute the mean and median with the new data set
meanFixedSteps <- mean(A.new.byDate$steps)
medianFixedSteps <- median(A.new.byDate$steps)

```

Missing cases in dataset: `r missing_sum`

Mean number of steps (fixed): `r meanFixedSteps`  
Median number of steps (fixed): `r medianFixedSteps`

## Mean and Median Comparison

The computed median value is slightly different than before, filling in the missing data with the mean from the time interval across all days added `r missing_sum` data points to the set.

Mean Steps: `r meanSteps`  
Mean Steps Fixed:  `r meanFixedSteps`  

Median Steps: `r medianSteps`  
Meadin Steps Fixed:  `r medianFixedSteps`


## Are there differences in activity patterns between weekdays and weekends?
```{r weekends}
# Convert dates to actual R dates
A.new$date <- as.Date(A.new$date)

# Convert to days of the week in word format
A.new$day_of_week <- weekdays(A.new$date)

# Create subsets for weekdays and weekends
weekends <- subset(A.new, A.new$day_of_week == "Saturday" | A.new$day_of_week == "Sunday")
weekdays <- subset(A.new, A.new$day_of_week != "Saturday" & A.new$day_of_week != "Sunday")

# Compute the meand over the intervals for weekends and weekdays
weekends.byinterval <- aggregate(weekends$steps, by=list(weekends$interval), mean)
weekdays.byinterval <- aggregate(weekdays$steps, by=list(weekdays$interval), mean)

# Change the column names for ease of use
colnames(weekends.byinterval) <- c("time_of_day", "steps")
colnames(weekdays.byinterval) <- c("time_of_day", "steps")

#Setup for two stacked plots
par(mfrow=c(2,1))

plot(weekends.byinterval$time_of_day, weekends.byinterval$steps, type="l", main="Average Daily Weekend Activity Pattern", ylab="Mean Number of Steps", xlab="Daily 5 Minute Time Interval")

plot(weekdays.byinterval$time_of_day, weekdays.byinterval$steps, type="l", main="Average Daily Weekday Activity Pattern", ylab="Mean Number of Steps", xlab="Daily 5 Minute Time Interval")

```
