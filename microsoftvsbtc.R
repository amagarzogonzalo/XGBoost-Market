library(tidyquant)
library(DiagrammeR)
library(ggplot2)
library(lattice)
library(lubridate)
library(gridExtra)

library(TTR)
library(caret)
library(tidyr)
library(dplyr)
library(xgboost)
library(tidyquant)
library(quantmod)

symbols <- c("BTC-USD", "MSFT")
start_date <- "2020-01-01"
end_date <- "2023-01-01"

results <- data.frame()


for (symbol in symbols) {
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

  train_control <- trainControl(method = "cv", number = 5)

  grid <- expand.grid(max_depth = c(4, 5, 6),
                      eta = c(0.04, 0.05, 0.06),
                      nrounds = c(100, 120, 150),
                      gamma = 1,
                      min_child_weight = 0.1,
                      subsample = 0.8,
                      colsample_bytree = 0.8)

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
  cat("RMSE para", symbol, ":", rmse, "\n")

  predictions <- data.frame(Date = test_data$date,
                            Actual = test_data$close,
                            Predicted = y_pred)

  result <- predictions %>% mutate(Symbol = symbol)
  results <- rbind(results, result)
}

prediction_plot_btc <- ggplot(results[results$Symbol == "BTC-USD", ], aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Predicciones vs. Valores reales (BTC)",
       x = "Fecha",
       y = "Precio de cierre",
       color = "Leyenda") +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
  theme_minimal()

prediction_plot_msft <- ggplot(results[results$Symbol == "MSFT", ], aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Predicciones vs. Valores reales (Microsoft)",
       x = "Fecha",
       y = "Precio de cierre",
       color = "Leyenda") +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
  theme_minimal()



grid.arrange(prediction_plot_btc, prediction_plot_msft, ncol = 2)


volatility <- data.frame()

for (symbol in symbols) {
  data <- tq_get(symbol, from = start_date, to = end_date)

  data$log_return <- log(data$close / lag(data$close))

  data$volatility <- rollapplyr(data$log_return, width = 30, sd, fill = NA)

  volatility <- rbind(volatility, data %>% select(date, volatility) %>% mutate(symbol = symbol))
}

volatility <- na.omit(volatility)

volatility_plot <- ggplot(volatility, aes(x = date, y = volatility, color = symbol)) +
  geom_line() +
  labs(title = "Volatilidad de BTC y Microsoft",
       x = "Fecha",
       y = "Volatilidad",
       color = "Símbolo") +
  scale_color_manual(values = c("BTC-USD" = "green", "MSFT" = "grey")) +
  theme_minimal()

volatility_plot


