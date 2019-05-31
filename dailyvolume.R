library(tidyquant)
library(timekit)
FB_tbl <- FANG %>% filter(symbol == "FB") %>% select(date, volume)

# Everything before 2016 will be used for training (2013-2015 data)
train <- FB_tbl %>% filter(date < ymd("2016-01-01")) 
# Everything in 2016 will be used for comparing the output
actual_future <- FB_tbl %>% filter(date >= ymd("2016-01-01"))

train <- tk_augment_timeseries_signature(train)

fit_lm <- lm(volume ~ ., data = train[,-1])

# US trading holidays in 2016
holidays <- c("2016-01-01", "2016-01-18", "2016-02-15", "2016-03-25", "2016-05-30",
              "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-23", "2016-12-26",
              "2016-12-30") %>% 
  ymd()
# Build new data for prediction: 3 Steps
new_data <- train %>%
  tk_index() %>%
  tk_make_future_timeseries(n_future = 252, skip_values = holidays, inspect_weekdays = TRUE) %>%
  tk_get_timeseries_signature()
# New data should look like this
new_data


# Prediction using a linear model, fit_lm, on future index time series signature
pred_lm <- predict(fit_lm, newdata = new_data)

# Add predicted values to actuals data
actual_future <- actual_future %>%
  add_column(yhat = pred_lm) 
# Plot using ggplot
actual_future %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = volume), data = train, color = palette_light()[[1]]) +
  geom_line(aes(y = volume), color = palette_light()[[1]]) +
  geom_line(aes(y = yhat), color = palette_light()[[2]]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Forecasting FB Daily Volume: New Methods Using Data Mining",
       subtitle = "Linear Regression Model Applied to Time Series Signature",
       x = "",
       y = "Volume",
       caption = "Data from Yahoo! Finance: 'FB' Daily Volume from 2013 to 2016.") +
  theme_tq(base_size = 12)