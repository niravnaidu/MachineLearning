install.packages("forecast")
install.packages("ggplot2")

library(forecast)
library(ggplot2)

Kaggle <- read.csv("train.csv")
Kaggle$Day <- as.numeric(Kaggle$Day)

# Create a time series object 
ts_data <- ts(Kaggle$Difference, start = min(Kaggle$Day), frequency = 365)

# Fit ARIMA model
arima_model <- auto.arima(ts_data)
summary(arima_model) 

# Forecast next 49 days
forecast_days <- 49
arima_forecast <- forecast(arima_model, h = forecast_days)
predicted_stock_diff <- arima_forecast$mean

# Prepare prediction data
predicted_data <- data.frame(Day = seq(from = max(Kaggle$Day) + 1, to = max(Kaggle$Day) + forecast_days, by = 1),
                             PredictedStockPrice = predicted_stock_diff)

# Combine historical and predicted data
historical_data <- Kaggle[, c("Day", "Difference")]
colnames(historical_data) <- c("Day", "Difference")
# Ensure column names are the same for both data frames
colnames(historical_data) <- c("Day", "Difference")
colnames(predicted_data) <- c("Day", "Difference")


# Now we can safely bind the data frames


# Add a new column to distinguish between historical and predicted data
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# View the combined data
head(combined_data)


# Add a new column to distinguish between historical and predicted data
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# View combined data
head(combined_data)

combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# Plot results
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(aes(color = type), size = 1.5) +
  geom_point(aes(color = type), size = 2, shape = 16) +
  labs(title = "Stock Price Prediction Using ARIMA",
       x = "Day", y = "Stock Price Difference") +
  scale_color_manual(values = c("Historical" = "#1f77b4", "Predicted" = "#ff7f0e")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Export to CSV
write.csv(combined_data, "kaggle_stock_predictions_arima.csv", row.names = FALSE)
write.csv(predicted_data, "ARIMAStocksPredictions.csv", row.names = FALSE)
