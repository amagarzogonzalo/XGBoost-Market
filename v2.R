install.packages("xgboost")
install.packages("tidyquant")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("caret")
install.packages("ggplot2")
install.packages("lattice")
install.packages("TTR")
install.packages("quantmod")
install.packages("DiagrammeR")

library(DiagrammeR)
library(ggplot2)
library(lattice)
library(TTR)
library(dplyr)
library(tidyr)
library(lubridate)
library(caret)
library(quantmod)
library(xgboost)
library(tidyquant)


symbol <- "GOOGL"
start_date <- "2020-01-01"
end_date <- "2023-01-01"

data <- tq_get(symbol, from = start_date, to = end_date)


data <- data %>%
  mutate(ma_5 = rollapplyr(close, 5, mean, fill = NA),
         ma_30 = rollapplyr(close, 30, mean, fill = NA),
         vol_5 = rollapplyr(close, 5, sd, fill = NA),
         vol_30 = rollapplyr(close, 30, sd, fill = NA)) %>%
  mutate(rsi = RSI(close, n = 14, maType = "WMA"),
         sar = SAR(data[, c("high", "low")], accel = c(0.02, 0.2)),
         trend = close - sar)

adx <- data.frame(ADX(data[, c("high", "low", "close")]))
data$adx <- adx$ADX

data <- na.omit(data)


train_frac <- 0.8
split_index <- floor(nrow(data) * train_frac)

train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

x_train <- train_data %>%
  select(ma_5, ma_30, vol_5, vol_30, rsi, adx, trend) %>%
  as.matrix()

y_train <- train_data$close

x_test <- test_data %>%
  select(ma_5, ma_30, vol_5, vol_30, rsi, adx, trend) %>%
  as.matrix()

y_test <- test_data$close

params <- list("objective" = "reg:linear",
               "eval_metric" = "rmse",
               "eta" = 0.2,
               "max_depth" = 5,
               "min_child_weight" = 1,
               "subsample" = 0.8,
               "colsample_bytree" = 0.8)

xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_test, label = y_test)

xgb_model <- xgb.train(params = params,
                       data = xgb_train,
                       nrounds = 100,
                       watchlist = list(train = xgb_train, test = xgb_test),
                       early_stopping_rounds = 10,
                       print_every_n = 10)

y_pred <- predict(xgb_model, xgb_test)

rmse <- sqrt(mean((y_test - y_pred)^2))
cat("RMSE:", rmse, "\n")

predictions <- data.frame(Date = test_data$date,
                          Actual = y_test,
                          Predicted = y_pred)

ggplot(predictions, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Predicciones vs. Valores reales",
       x = "Fecha",
       y = "Retornos",
       color = "Leyenda") +
  theme_minimal()


