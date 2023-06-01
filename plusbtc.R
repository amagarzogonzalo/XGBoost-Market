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
library(lubridate)

library(TTR)
library(caret)
library(tidyr)
library(dplyr)
library(xgboost)
library(tidyquant)
library(quantmod)


symbol <- "BTC-USD"
start_date <- "2012-01-01"
end_date <- "2023-01-01"
#start_date <- "2020-01-01"
#end_date <- "2023-01-01"

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


train_frac <- 0.93
split_index <- floor(nrow(data) * train_frac)

train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

train_control <- trainControl(method = "cv", number = 5)

grid <- expand.grid(max_depth = c(4, 5,6, 7, 8,10),
                    eta = c( 0.04, 0.05, 0.06, 0.1, 0.15, 0.2, 0.3),
                    nrounds = c(100, 120, 150, 200, 220),
                    #gamma = 1,
                    gamma = c(0, 1),
                    min_child_weight = 0.1,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
#subsample = c(0.6, 0.8, 0.9))
#colsample_bytree = c(0.6, 0.8, 1))

train_data_caret <- train_data

xgb_model_cv <- train(close ~ ma_5 + ma_30 + vol_5 + vol_30 + rsi + adx + trend,
                      data = train_data_caret,
                      method = "xgbTree",
                      trControl = train_control,
                      tuneGrid = grid,
                      metric = "RMSE")

print(xgb_model_cv)
y_pred <- predict(xgb_model_cv, newdata = test_data)

rmse <- sqrt(mean((test_data$close - y_pred)^2))
cat("RMSE:", rmse, "\n")

predictions <- data.frame(Date = test_data$date,
                          Actual = test_data$close,
                          Predicted = y_pred)

ggplot(predictions, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Predicciones vs. Valores reales",
       x = "Fecha",
       y = "Retornos",
       color = "Leyenda") +
  theme_minimal()

