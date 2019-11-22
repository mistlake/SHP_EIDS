#############################
# Spatiotemporal Regression #
#############################

#Name: 
#Date: 
#Summary: This assignment
#         1. demonstrates ggplot
#         2. demonstrates spatiotemporal regression

library("splines")
library("tidyverse")
library("cowplot")

#pick a location
location <- c("Longitude" = -73.9625581, "Latitude" = 40.8069665)

#New York City Incident Level Data
url <- "https://github.com/jauerbach/LMIS/blob/master/NYPD_Complaint_Data_Historic.csv?raw=true"
crime <- read.csv(url)
crime <-
  crime %>% mutate(time_stamp = as.POSIXct(x = paste(CMPLNT_FR_DT,
                                                   CMPLNT_FR_TM),
                                         tz= "EST",
                                         format =  "%m/%d/%y %H:%M:%S"),
                 Hour = parse_number(format(time_stamp, "%H")),
                 Minute = parse_number(format(time_stamp, "%M")))

## rounding errors, unit of analysis should be hour
hist(x = crime$Hour)
hist(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes")
hist(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes", breaks = 10)

qplot(x = crime$Hour, xlab = "Hour of Day", ylab = "Number of Crimes", bins = 10)
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day")
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day") + theme_bw()
qplot(x = crime$Hour, bins = 10) + labs(x = "Hour of Day") + theme_classic()
qplot(x = crime$Minute, bins = 10) + labs(x = "Minute of Day",
                                          y ="Number of Crimes") + theme_bw()

theme_set(theme_bw())

ggplot(data = crime) +
  aes(Minute) +
  geom_histogram(bins = 10) +
  labs(x = "Minute of Day")

crime <- crime %>%
  mutate(weekdays = factor(weekdays(time_stamp),
                           levels = c("Monday", "Tuesday", "Wednesday",
                                      "Thursday", "Friday", "Saturday", 
                                      "Sunday")),
                           week_num = as.numeric(weekdays))

#TIME
ssp <- crime %>% 
  group_by(format(time_stamp, "%Y-%m-%d %H")) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  spectrum(x = ., plot = FALSE)
period <- 1/ssp$freq[ssp$spec == max(ssp$spec)]

daily_crime <-
  crime %>% 
  mutate(time = Hour + period * (week_num - 1)) %>%
  group_by(time) %>%
  summarize(n = n())

plot(x = daily_crime$time, y = daily_crime$n)
qplot(x = daily_crime$time, y = daily_crime$n)
qplot(x = time, y = n, data = daily_crime)

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point() 

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(formula = y ~ sin(2 * pi * x / period) + 
                cos(2 * pi * x / period), 
              color = "blue",
              method = "lm", 
              linetype = 2, 
              alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") 

expnd  <- function(n) paste("y ~ ",
                            paste0("sin(",2^(1:n)," * pi * x / period)", 
                                   collapse = " + "),
                            "+",
                            paste0("cos(",2^(1:n)," * pi * x / period)", 
                                   collapse = " + "))

expnd(1)
expnd(2)
expnd(3)

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(formula = expnd(1), color = "blue",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  geom_smooth(formula = expnd(2), color = "orange",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  geom_smooth(formula = expnd(10), color = "red",
              method = "lm", linetype = 1, alpha = .5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports")

ggplot(data = daily_crime) +
  aes(time, n) +
  geom_point(alpha = .1) +
  geom_smooth(color = "black", linetype = 2, alpha = .5, se = FALSE, span = .2) +
  geom_smooth(formula = y ~ ns(x, 20), method = "lm",
              color = "grey", alpha = .5, se = FALSE, span = .2) +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports")

# investigate the breakdown of the types of crimes
crime <-
crime %>% 
  mutate(quarter = cut(time_stamp, "quarter"),
         month   = format(parse_date(as.character(quarter)), "%m"),
         month   = factor(month, labels = 1:4)) 

top_crime <-
  crime %>%
  group_by(OFNS_DESC) %>%
  summarize(n = n()) %>%
  top_n(4)

crime %>% 
  filter(OFNS_DESC %in% top_crime$OFNS_DESC) %>%
  mutate(time = Hour + period * (week_num - 1)) %>%
  group_by(time, OFNS_DESC) %>%
  summarize(n = n()) %>%
  ggplot() +
  aes(time, n) +
  geom_point(alpha = .1) +
  theme_bw() +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") +
  facet_wrap(~ OFNS_DESC, scales = "free")

# then stratify over period of the year, by quarters
crime %>% 
  filter(OFNS_DESC %in% top_crime$OFNS_DESC) %>%
  mutate(time = Hour + period * (week_num - 1)) %>%
  group_by(time, OFNS_DESC, month) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  ggplot( ) +
  aes(time, n) +
  geom_point(alpha = .1) +
  theme_bw() +
  geom_smooth(formula = expnd(4), color = "red",
              method = "lm", linetype = 2, alpha = .5) +
  scale_x_continuous(breaks = seq(0, period * 7, period)) +
  labs(x = "hours", y = "number of reports") +
  facet_grid(month ~ OFNS_DESC, scales = "free", drop = TRUE)

#space
plot(x = crime$Longitude, y = crime$Latitude)
qplot(x = crime$Longitude, y = crime$Latitude)

ggplot(data = crime) + 
  aes(x = Longitude, y = Latitude) +
  geom_point()

ggplot(crime) + 
  geom_point(aes(Longitude, Latitude))

ggplot(crime) + 
  geom_density2d(aes(Longitude, Latitude))

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..density..), 
                  geom = "raster", contour = FALSE)

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon")

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon") +
  scale_fill_continuous(low = "red", high = "yellow")

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon") +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom")

ggplot(crime) + 
  stat_density_2d(aes(Longitude, Latitude, fill = ..level..), 
                  geom = "polygon") +
  scale_fill_continuous(low = "red", high = "yellow") +
  theme_nothing(legend = TRUE) +
  theme(legend.position = "bottom") +
  geom_point(aes(Longitude, Latitude), 
             data = tibble(Longitude = location[1],
                           Latitude  = location[2]))

selected_crimes <- 
  crime$Longitude >= location[1] - .001 &
  crime$Longitude <= location[1] + .001 &
  crime$Latitude  >= location[2] - .001 &
  crime$Latitude  <= location[2] + .001

table(selected_crimes)
table(crime$OFNS_DESC[which(selected_crimes)])
View(crime[which(selected_crimes), ])

