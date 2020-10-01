library(data.table)
library(dplyr)
library(ggplot2)


#https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip


unzip("activity.zip")
df <- fread("activity.csv")

df$date <- as.Date(df$date, format="%Y-%m-%dæ")





totals <- df %>% group_by(date) %>% 
      summarise(total_steps=sum(steps, na.rm = TRUE))


ggplot(totals, aes(x=total_steps)) + 
      geom_histogram(color="black", fill = "blue") +
      ggtitle("Histogram - Total Steps")


mean_steps = mean(totals$total_steps)
median_steps = median(totals$total_steps)




intervals <- df %>% group_by(interval) %>% 
      summarise(average_steps=mean(steps, na.rm = TRUE))

ggplot(intervals, aes(interval, average_steps)) + 
      geom_point() +
      geom_line() +
      ggtitle("Average Steps by Time Interval")

max_steps <- max(intervals$average_steps)
max_interval <- intervals$interval[intervals$average_steps==max_steps]






total_na <- sapply(df, function(x) sum(is.na(x)))

df_imputed <- data.frame(df)

for (i in 1:nrow(df_imputed)){
      if(is.na(df_imputed$steps[i])){
            na_interval <- df_imputed$interval[i]
            df_imputed$steps[i] <- intervals$average_steps[intervals$interval==na_interval] 
      }
}

totals_imputed <- df_imputed  %>% group_by(date) %>% 
      summarise(total_steps=sum(steps, na.rm = TRUE))


ggplot(totals_imputed , aes(x=total_steps)) + 
      geom_histogram(color="black", fill = "blue") +
      ggtitle("Imputed Data - Histogram - Total Steps")


mean_steps_imputed  <- mean(totals_imputed$total_steps)
median_steps_imputed  <- median(totals_imputed$total_steps)






weekday_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

df_imputed$weekday <- factor((weekdays(df_imputed$date) %in% weekday_names), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

intervals_imputed <- df_imputed %>% group_by(interval, weekday) %>% 
      summarise(average_steps=mean(steps, na.rm = TRUE))


ggplot(intervals_imputed, aes(x=interval, y=average_steps, group=weekday)) + 
      geom_line(aes(color=weekday)) + 
      facet_wrap(~ weekday, nrow=2) +
      ggtitle("Weekend v Weekday - Average Steps by Time Interval")