#Load code for plotting
library(ggplot2)

#Read in daily NYC covid data
daily_covid <- read.csv(file="tests.csv")

#Function that can plot our COVID time series
plot_data <- function(cov_dat,label_breaks = 15,logarithm=FALSE){
  breaks <- seq(1,nrow(cov_dat),label_breaks)
  labels <- sapply(as.character(cov_dat$DATE),function(s) substr(s,1,5))
  labels <- labels[breaks]
  day_count <- seq(1,nrow(cov_dat),1)
  if (logarithm){
    covid_plot <- ggplot(data = cov_dat,aes(x = day_count, y = log(POSITIVE_TESTS)))
  } else {
    covid_plot <- ggplot(data = cov_dat,aes(x = day_count, y = POSITIVE_TESTS))
  }
  covid_plot <- covid_plot +
    geom_line(size=1) + 
    geom_point() + 
    scale_x_continuous(breaks=breaks,labels=labels) +
    xlab("Date") + 
    ylab("# Positive Tests") + 
    ggtitle("Daily Positive Tests in NYC") +
    theme_bw()
  return(covid_plot)
}

#Plot the imported COVID data
daily_plot <- plot_data(daily_covid)
daily_plot

#Take just the first two weeks of data and plot
covid_early <- daily_covid[1:14,]
early_plot <- plot_data(covid_early,label_breaks = 1)
early_plot

#Now plot the log case numbers instead
log_plot <- plot_data(covid_early,label_breaks=1,logarithm=TRUE)
log_plot

#Fit a simple linear regression of the log case numbers against the day
day_count <- seq(1,nrow(covid_early),1)
early_mod <- lm(log(covid_early$POSITIVE_TESTS)~day_count)
log_plot <- log_plot + geom_smooth(method='lm',se=FALSE,color='red')
log_plot

#Define function for fitted exponential curve
coefs <- early_mod$coefficients
exp_fit <- function(x) {
  fit <- exp(coefs[1] + coefs[2]*x)
  return(fit)
}

#Plot this exponential curve through the early data
early_plot <- early_plot + stat_function(fun = exp_fit,size=1.2,color='red',alpha=0.7)
early_plot

#Plot this exponential curve through the full data
daily_plot <- daily_plot +  ylim(c(0,7000)) + stat_function(fun = exp_fit,size=1.2,color='red',alpha=0.7)
daily_plot

#Take and plot the data from after the first two weeks - how might you model this?
covid_bulk <- daily_covid[15:nrow(daily_covid),]
plot_data(covid_bulk,label_breaks=10,logarithm=TRUE)

