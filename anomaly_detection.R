library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(xts)
library(forecast)
library(devtools)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#set up directories
setwd("C:/Users/victor/Documents/R")
data_file <- 'artificialWithAnomaly/art_daily_jumpsup.csv'

#import machine data and groupby mean to remove duplicate timestamps
data <- read.csv(data_file, header = TRUE)
data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%OS")
data <- dplyr::summarize(group_by(data,timestamp),value=mean(value))
data <- data[order(data$timestamp),]
# print(periodicity(data$timestamp))

ggplot(data,aes(x=timestamp,y=value)) + 
  geom_line(color = 'grey34')
#how to determine whether there's a seasonal component?

#when to use log vs not?

#convert to time-series
x <- ts(data$value,freq=24*60/5)
#http://faculty.washington.edu/eliezg/teaching/StatR301/2015/Lab07/Lab07c_SeasonalDecompositionWithLoess.html
ts_stl <- stl(log(x), s.window = "periodic", robust=TRUE)
autoplot(ts_stl)
#can we model using just the trend and seasonal? does the remainder give us the anomalies?
autoplot(forecast(ts_stl, method = "arima", h = 500))

#how to analyze the acf?
resid <- ts_stl$time.series[,3]
acf(resid)

#autoregressive model to model the noise component
ar(resid)

### http://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series
resid_q <- quantile(ts_stl$time.series[,3],prob=c(0.25,0.75))
iqr <- diff(resid_q)
limits <- resid_q + 1.5*iqr*c(-1,1)
score <- abs(pmin((ts_stl$time.series[,3]-limits[1])/iqr,0) + pmax((ts_stl$time.series[,3] - limits[2])/iqr,0))
autoplot(x)
x2 <- ts(rep(NA,length(x)))
x2[score>0] <- x[score>0]
tsp(x2) <- tsp(x)
points(x2,pch=19,col="red")


### https://blog.twitter.com/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series
#http://stackoverflow.com/questions/17783686/solution-how-to-install-github-when-there-is-a-proxy
res = AnomalyDetectionTs(data, max_anoms=0.008, direction='both', plot=TRUE)
res$plot
