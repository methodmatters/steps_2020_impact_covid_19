library(plyr); library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# read in the raw data
in_dir = 'D:\\Directory\\'
daily_data <- read_csv(paste0(in_dir, 'daily_data.csv'))


head(daily_data, 10)

# set up the palette for the first plot
library(Polychrome)
mypal <- kelly.colors(14)[3:14]
swatch(mypal)
names(mypal) <- NULL


# Plot Weekly Averages
# Plot Weekly Averages
# Plot Weekly Averages
# Plot Weekly Averages
# Plot Weekly Averages

# barplot for week with color by month
daily_data %>% 
  # create a "week" variable from the date
  # using the lubridate package
  mutate(week = week(date)) %>%
  # group the data by week of the year
  group_by(week) %>% 
  # for each week, calculate the average step count
  # and which month the week was in (used for color)
  summarize(avg_steps = mean(daily_total),
            month = min(as.numeric(month))) %>% 
  # turn the month variable into a factor
  mutate(month = factor(month.abb[month],levels=month.abb)) %>% 
  # pass the data to ggplot
  ggplot(data = ., aes( x = week, y = avg_steps, fill = month)) + 
  # set the range of the y axis
  coord_cartesian(ylim = c(8000, 20000))+
  # specify we want a bar chart
  geom_bar(stat = 'identity') +
  # draw a dashed vertical line during the week
  # that the first lockdown started
  geom_vline(xintercept = 11, linetype="dashed", 
             color = "red", size=1.5) +
  # draw a dashed vertical line during the week
  # of switch from Fitbit to Mi Band
  geom_vline(xintercept = 32, linetype="dashed", 
             color = "darkblue", size=1.5) +
  # annotations on plot
  annotate(geom = 'text', x = 5, y = 19500,
           label = 'First COVID-19 \n Lockdown',
           color = 'darkred', size = 4.5) +
  annotate(geom = 'rect', xmin = -1.5, xmax = 11,
           ymin = 18500, ymax = 20250, fill = 'red',
           alpha = .15) +
  annotate(geom = 'text', x = 38, y = 19500,
           label = 'Switched from \n Fitbit to Mi Band',
           color = 'darkblue', size = 4.5) +
  annotate(geom = 'rect', xmin = 32, xmax = 44.5,
           ymin = 18500, ymax = 20250, fill = 'blue',
           alpha = .15) +
  # set the axis labels and title
  labs(fill='Month',
       x = "Week of the Year", 
       y = "Average Daily Steps Per Week", 
       title = 'Average Daily Steps Per Week: 2020') +
  # specify the black and white theme
  theme_bw() +
  # set the colors according to the above palette
  scale_fill_manual(values = mypal)



### Simple Model
### Simple Model
### Simple Model
### Simple Model
### Simple Model

# basic regression: predict daily total from:
# time period, device, week/weekend
lm_1 <- lm(daily_total ~ 1 + time_period + device + week_weekend, data = daily_data)
# examine model results
summary(lm_1)

# function to calculate model error
compute_model_performance <- function(true_f, pred_f){
  # and calculate model performance metrics   
  # error   
  error_f <- true_f - pred_f   
  # root mean squared error   
  rmse_f <- sqrt(mean(error_f^2))   
  print('RMSE:')   
  print(rmse_f)   
  # mean absolute error   
  mae_f <- mean(abs(error_f))   
  print('MAE:')   
  print(mae_f)  
}

# calculate the model error
compute_model_performance(daily_data$daily_total, 
                          predict(lm_1, daily_data))

### Plot Raw Data + Model Predictions
### Plot Raw Data + Model Predictions
### Plot Raw Data + Model Predictions
### Plot Raw Data + Model Predictions
### Plot Raw Data + Model Predictions

# add the predictions to the main dataset
# https://stackoverflow.com/questions/44865508/using-ggplot2-to-plot-an-already-existing-linear-model
daily_data_predict <- cbind(daily_data, predict(lm_1, interval = 'confidence'))

head(daily_data_predict, 10) 

# set up the palette for the prediction plot
pal_2 <- c("#40830D", "#BD002E")
swatch(pal_2)

# plot the actual and predicted values 
# from the regression model
# different lines weekday / weekend  
ggplot(data = daily_data_predict, 
       aes(x = date, y = daily_total, 
           color = week_weekend)) +
  # specify that we want points
  geom_point(size = 1, alpha = .85) + 
  # for the predictions, we will use black crosses
  # instead of points
  # http://www.sthda.com/english/wiki/ggplot2-point-shapes
  geom_point(aes(y = fit), color = 'black',
             size = 2, shape = 4) +
  # draw the predicted values and connect with a line
  geom_line(aes(date, fit), size = 1)  +
  # can also add error shading, but turned off 
  # for this plot because it's too much!
  # geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.1) +
  # set the limits of the y axis to focus on 
  # range where most of the data lies
  coord_cartesian(ylim = c(5000, 25000))   +
  # add the vertical line indicating the date
  # that the first lockdown began
  geom_vline(xintercept = date('2020-03-14'), 
             linetype="dashed", 
             color = "red", size=1.5)  +
  # draw a dashed vertical line during the week
  # of switch from Fitbit to Mi Band
  geom_vline(xintercept = date('2020-08-08'), linetype="dashed", 
             color = "darkblue", size=1.5) +
  # annotations on plot
  annotate(geom = 'text', x = date('2020-01-30'), y = 24500,
           label = 'First COVID-19 \n Lockdown',
           color = 'darkred', size = 3.5) +
  annotate(geom = 'rect', xmin = date('2019-12-25'), xmax = date('2020-03-05'),
           ymin = 23000, ymax = 25700, fill = 'red',
           alpha = .15) +
  annotate(geom = 'text', x = date('2020-09-20'), y = 24500,
           label = 'Switched from \n Fitbit to Mi Band',
           color = 'darkblue', size = 3.5) +
  annotate(geom = 'rect', xmin = date('2020-08-10'), xmax = date('2020-10-30'),
           ymin = 23000, ymax = 25700, fill = 'blue',
           alpha = .15) +
  # set the axis labels and title
  labs(x = "Date", 
       y = "Daily Total Step Count", 
       title = '2020 Daily Total Step Count: Actual and Fitted Values', 
       color = 'Weekday / Weekend') +
  # choose black and white theme
  theme_bw() +
  # scale the x axis - month tick marks
  # and labelled with abbreviated month name
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  # use the color palette we specify above
  scale_color_manual(values = pal_2)

