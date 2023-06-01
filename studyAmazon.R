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


symbol <- "AMZN"
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


train_frac <- 0.95
split_index <- floor(nrow(data) * train_frac)

train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

train_control <- trainControl(method = "cv", number = 5)

grid <- expand.grid(max_depth = c(4, 5,6),
                    eta = c( 0.04, 0.05, 0.06),
                    nrounds = c(100, 120, 150),
                    gamma = 1,
                    #gamma = c(0, 1),
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

# Calcular el RMSE
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

price_plot <- ggplot(test_data, aes(x = date)) +
  geom_line(aes(y = close, color = factor("Precio"))) +
  geom_line(aes(y = ma_5, color = factor("Ma 5"))) +
  geom_line(aes(y = ma_30, color = factor("Ma 30"))) +
  labs(x = "Date", y = "Price",
       title = paste("Precio y medias móviles de", symbol),
       color = "Indicador") +
  scale_color_manual(values = c("Precio" = "blue", "Ma 5" = "red", "Ma 30" = "green")) +
  theme_minimal()


rsi_plot <- ggplot(test_data, aes(x = date)) +
  geom_line(aes(y = rsi), color = "purple") +
  labs(x = "Date", y = "RSI", title = "RSI") +
  theme_minimal()

library(gridExtra)
grid.arrange(price_plot, rsi_plot, ncol=1)

# Extra ADX - SAR

sar_plot <- ggplot(test_data, aes(x = date)) +
  geom_line(aes(y = sar), color = "blue") +
  labs(x = "Date", y = "SAR",
       title = paste("SAR", symbol)) +
  theme_minimal()

adx_plot <- ggplot(test_data, aes(x = date)) +
  geom_line(aes(y = adx), color = "orange") +
  labs(x = "Date", y = "ADX",
       title = paste("ADX", symbol)) +
  theme_minimal()
grid.arrange(sar_plot, adx_plot, ncol=1)

