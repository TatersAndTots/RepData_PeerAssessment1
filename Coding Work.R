setwd("~/R_Coding/Mod5/RepData_PeerAssessment1")
rm(list=ls())  
library(ggplot2)
library(knitr)

() function.
data <- read.csv("activity.csv")



days <- split(data$steps, data$date)
dayssum <- unlist(lapply(days, sum))
daysme <- unlist(lapply(days, mean))
daysmd <- unlist(lapply(days, median))
daysdf <- data.frame(cbind(dayssum, daysme, daysmd))
names(daysdf) <- c("Sum", "Mean", "Median")
hist(data$steps)


int <- na.omit(data)
intin <- split(int$steps, int$interval)
intinm <- unlist(lapply(na.omit(intin), mean))
plot(names(intinm), intinm, type = "l", main = "Average Number of Steps Taken per 5 Minute Interval"
     , xlab = "Interval", ylab = "Steps")


range(intinm)
which(intinm %in% max(intinm))
intinm[104]


c(sum(is.na(data$steps)), sum(is.na(data$interval)), sum(is.na(data$date)))




med <- function(x) {
  if (is.na(x)) 
    {x <- 37.3826}
  else {x <- x}
  }

hist(data$steps)

datana <- cbind(sapply(data$steps, med), data[,2:3])
names(datana) <- c("steps", "date", "interval")
days2 <- split(datana$steps, datana$date)
daysme2 <- unlist(lapply(days2, mean))
daysmd2 <- unlist(lapply(days2, median))
daysdf2 <- data.frame(cbind(daysme2, daysmd2))
names(daysdf2) <- c("Mean", "Median")


datana$date <- as.Date(datana$date)

week <- function(x) {
  if (weekdays(as.Date(x)) == "Saturday" | weekdays(as.Date(x)) == "Sunday" ) 
  {x <- "Weekend"}
  else {x <- "Weekday"}
}

datana <- cbind(datana, sapply(data$date, week))
names(datana) <- c("steps", "date", "interval", "weekday_or_weekend")

dataday <- split(datana, datana$weekday_or_weekend)
dataday1 <- dataday[[1]]
dataday2 <- dataday[[2]]
dataday11 <- split(dataday1$steps, dataday1$interval)
dataday22 <- split(dataday2$steps, dataday2$interval)
datamean1 <- unlist(lapply(dataday11, mean))
datamean2 <- unlist(lapply(dataday22, mean))
datadf <- data.frame(cbind(datamean1, datamean2))
names(datadf) <- c("Weekday_Average", "Weekend_Average")

par(mfrow = c(2,1))
plot(row.names(datadf), datadf$Weekday_Average, type = "l", main = "Average Weekday Steps"
     , xlab = "Interval", ylab = "Steps")

plot(row.names(datadf), datadf$Weekend_Average, type = "l", main = "Average Weekend Steps"
     , xlab = "Interval", ylab = "Steps")


