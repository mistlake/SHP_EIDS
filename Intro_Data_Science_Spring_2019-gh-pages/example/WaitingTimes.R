###############
# The Waiting Time Paradox #
###############

# The Waiting Time Paradox is well known. If a train is supposed to arrive, on average, every 
# 10 minutes, you would expect to be waiting less than 10 minutes for a train. This is
# in fact only true if trains arrive exactly every 10 minutes.

#Author:  Owen Ward
#Summary: 
## 1. Simulate some data
## 2. Analyse real NYC data
## 3. Compare the performances


## Simulate Arrival Times of Train
N = 1000 # number of trains
tau = 10 # average time between trains
train_times = sort(runif(N))*tau*N
hist(train_times)


# this simulates

mean(diff(train_times))

# shows that the average time between trains is indeed close to 10 minutes...

# can then simulate people arriving at that particular station and determine how long
# they each have to wait for the next train

n = 100000

max_time = max(train_times)
passenger_arrival_times = sort(runif(n)*max_time)

hist(passenger_arrival_times)

intervals = findInterval(passenger_arrival_times,train_times)
wait_time = train_times[intervals+1]-passenger_arrival_times
hist(wait_time)

mean(wait_time)
summary(wait_time)

# exponential times

train_times = rexp(N, rate = 0.1)
hist(train_times)
mean(train_times)

# then want to do a cum sum here
train_times = cumsum(train_times)
hist(train_times)
summary(train_times)

max_time = max(train_times)
passenger_arrival_times = sort(runif(n)*max_time)

hist(passenger_arrival_times)

intervals = findInterval(passenger_arrival_times,train_times)
wait_time = train_times[intervals+1]-passenger_arrival_times
hist(wait_time)

mean(wait_time)
summary(wait_time)

# this more likely, poisson process, then runif. it doesn't really matter,
# will still be waiting the same length of time, on average, for 
# a train, although longest possible wait different.



# what if the trains come exactly every 10 minutes, the ideal scenario with no delays.

exact_train_times = seq(from=0, to = N*tau, by = tau)

max_time = max(exact_train_times)
passenger_arrival_times = sort(runif(n)*max_time)

hist(passenger_arrival_times)

intervals = findInterval(passenger_arrival_times,exact_train_times)
wait_time = exact_train_times[intervals+1]-passenger_arrival_times
hist(wait_time)

mean(wait_time)
summary(wait_time)


# https://jakevdp.github.io/blog/2018/09/13/waiting-time-paradox/
# try find some real data to use for this if possible.
# investigate if poisson process assumption is reasonable, etc


### Analysing NYC turnstyle data ####

# have turnstyle data
# try verify if a Poisson process is reasonable
# data can be downloaded from http://web.mta.info/developers/turnstile.html
# or take file I downloaded from https://github.com/lydiahsu/Intro_Data_Science_Spring_2019/blob/master/turnstile_190105.txt
install.packages("readr")
library(readr)
setwd("C:/Users/owenw/Google Drive/Teaching_TA/Data Science Course_SHP/Simulation/Spring 19/")
turnstyle <- read_csv("turnstile_190105.txt")

# convert the dates to R formatting

summary(turnstyle$DATE)
turnstyle$DATE <- as.Date(turnstyle$DATE, format = "%m/%d/%Y")
summary(as.Date(turnstyle$DATE, format = "%m/%d/%Y"))
# convert to numeric

turnstyle$ENTRIES <- as.numeric(turnstyle$ENTRIES)
turnstyle$EXITS <- as.numeric(turnstyle$EXITS)

# Investigate poisson Process for a single station
# pick 116th on the 1 line (Columbia University)


stat_index <- which(turnstyle$STATION=="116 ST-COLUMBIA")
head(turnstyle[stat_index,])

columbia_station <- turnstyle[stat_index,]
head(columbia_station)

# now want to mutate this so that we just have the total 
# station counts for each 4 hour period
library(dplyr)
totals <- columbia_station %>% group_by(DATE,TIME,) %>%
  summarise(people_in = sum(ENTRIES),
            people_out = sum(EXITS))

tail(totals)
# don't know how many came in in the period starting with the last time point, 
# so need to remove that


in_per_time <- diff(totals$people_in)
out_per_time <- diff(totals$people_out)

totals <- totals[-nrow(totals),]
dim(totals)
totals$in_per <- in_per_time
totals$out_per <- out_per_time


# then check if these look to be poisson ditributed, etc
# this would mean that the average number of people in in each time window would
# be the same. does this seem reasonable?
library(ggplot2)
ggplot(totals,aes(in_per)) + geom_histogram()
ggplot(totals,aes(out_per)) + geom_histogram()

# poisson MLE
lambda_hat_in = base::mean(totals$in_per)
lambda_hat_out = base::mean(totals$out_per)

lambda_hat_in
lambda_hat_out


totals$samples_in <- rpois(n = nrow(totals),lambda =  lambda_hat_in)
totals$samples_out <- rpois(n = nrow(totals), lambda = lambda_hat_out)

totals$Time_Date <-as.POSIXct(paste(totals$DATE, totals$TIME), format="%Y-%m-%d %H:%M",tz = "America/New_York")

# compare the plots

ggplot(totals,aes(in_per)) + 
  geom_histogram(fill="blue", alpha = 0.2)

ggplot(totals,aes(samples_in)) + 
  geom_histogram(fill="red", alpha = 0.2)


# investigate the relationship between people coming in and out and the time
# what could be causing the relationship to change over time?

ggplot(totals,aes(TIME,in_per)) + geom_point()
ggplot(totals,aes(TIME,out_per)) + geom_point()

ggplot(totals,aes(as.factor(as.numeric(TIME)/3600),in_per)) + geom_boxplot() +
  xlab("End of Time Period") +
  ylab("Passengers entering")


ggplot(totals,aes(as.factor(as.numeric(TIME)/3600),out_per)) + geom_boxplot() +
  xlab("End of Time Period") +
  ylab("Passengers exiting")

# fit a Poisson regression without and with the time and see how it looks

model <- glm(in_per ~ TIME+DATE,data = totals, family = poisson(link="log") )
summary(model)


### To Do ###
# Repeat the above analysis for another station. Pick one that would be 
# interesting (Times Square, Wall Street), and see what you can discover





