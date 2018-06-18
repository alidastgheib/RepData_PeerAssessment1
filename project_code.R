# Importing Data
# --------------
code_dir = "C:/Users/Ali_D/Desktop/D_S/my_codes/Reproducible Research/w2"
setwd(code_dir)

# Reading Data
# ------------
my_ds_orig <- read.csv("activity.csv")
my_ds = my_ds_orig

my_ds$date = as.Date(my_ds$date)
my_ds$month = as.numeric(format(my_ds$date, format = "%m"))
my_ds$day = as.numeric(format(my_ds$date, format = "%d"))
my_ds$dayPlusMonth = my_ds$day + my_ds$month


# What is mean total number of steps taken per day?
# -------------------------------------------------
total_steps = aggregate(formula =  steps ~ dayPlusMonth, 
                        data = my_ds, FUN = sum, na.action = na.omit)
png("plot1.png", width=640, height=640)
barplot(total_steps$steps, beside = TRUE, xlab = "Day's Number", ylab = "Total Steps",
        main = "Total Steps per Each Day (na.action = na.omit)",
        names.arg = total_steps$dayPlusMonth - total_steps$dayPlusMonth[1] + 1)
grid(); dev.off()

steps_summary = summary(total_steps$steps); steps_summary
steps_mean = as.numeric(steps_summary["Mean"]); 
names(steps_mean) = "steps_mean"; steps_mean
steps_median = as.numeric(steps_summary["Median"]); 
names(steps_median) = "steps_median"; steps_median


# What is the average daily activity pattern?
# -------------------------------------------
mean_steps = aggregate(formula =  steps ~ interval, 
                        data = my_ds, FUN = mean, na.action = na.omit)
png("plot2.png", width=640, height=640)
plot(x = mean_steps$interval, y = mean_steps$steps, type = "l",
     xlab = "Interval (min)", ylab = "Steps' Mean Across All Days", 
     main = "Average Daily Activity Pattern")
grid(); dev.off()

int_for_max_steps = mean_steps$interval[which.max(mean_steps$steps)];
names(int_for_max_steps) = "maximum (average) number of steps across all the days in the dataset"
int_for_max_steps


# Imputing missing values
# -----------------------
my_ds_imp = my_ds_orig
logic_ds = is.na.data.frame(my_ds_orig)
and_res = logic_ds[, 1] | logic_ds[, 2] | logic_ds[, 3]
numOfNA_row = sum(and_res)
NA_steps = sum(logic_ds[,1]); NA_date = sum(logic_ds[,2]); NA_interval = sum(logic_ds[,3])
NA_index = which(logic_ds[,1])
for (idx in NA_index){
  idx_invl = my_ds_orig$interval[idx]
  newSteps = mean_steps$steps[which(idx_invl == mean_steps$interval)]
  my_ds_imp$steps[idx] = newSteps
}

my_ds_imp$date = as.Date(my_ds_imp$date)
my_ds_imp$month = as.numeric(format(my_ds_imp$date, format = "%m"))
my_ds_imp$day = as.numeric(format(my_ds_imp$date, format = "%d"))
my_ds_imp$dayPlusMonth = my_ds_imp$day + my_ds_imp$month
total_steps2 = aggregate(formula =  steps ~ dayPlusMonth, data = my_ds_imp, FUN = sum, na.action = na.omit)
steps_summary2 = summary(total_steps2$steps); steps_summary2; 
steps_mean2 = as.numeric(steps_summary2["Mean"]); names(steps_mean2) = "steps_mean2"; steps_mean2; 
steps_median2 = as.numeric(steps_summary2["Median"]); 
names(steps_median2) = "steps_median2"; steps_median2


# Are there differences in activity patterns between weekdays and weekends?
# -------------------------------------------------------------------------
week_info  = weekdays(my_ds_imp$date)
weekend_idx = which(week_info == "Saturday" | week_info == "Sunday")
my_ds_imp$weekend = "No" 
my_ds_imp$weekend[weekend_idx] = "Yes"
my_ds_imp$weekend = as.factor(my_ds_imp$weekend)


mean_steps2 = aggregate(formula =  steps ~ interval + weekend, 
                       data = my_ds_imp, FUN = mean, na.action = na.omit)
png("plot4.png", width=640, height=640)
par(mfrow=c(2, 1)) 
plot(x = mean_steps2[mean_steps2$weekend == "No", 1], y = mean_steps2[mean_steps2$weekend == "No", 3],
     type = "l", xlab = "Interval (min)", ylab = "Steps' Mean Across All Weekdays",
     main = "Average Daily Activity Comparison (weekday vs. Weekend")
grid();

plot(x = mean_steps2[mean_steps2$weekend == "Yes", 1], y = mean_steps2[mean_steps2$weekend == "Yes", 3],
     type = "l", xlab = "Interval (min)", ylab = "Steps' Mean Across All Weekends")
grid(); dev.off()
