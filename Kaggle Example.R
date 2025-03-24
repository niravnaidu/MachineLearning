# Install and load necessary libraries
install.packages("forecast")
library(forecast)
library(ggplot2)

# Load the dataset
Kaggle <- read.csv("train.csv")

# Convert 'Day' to Date if it is not already in proper format
Kaggle$Date <- as.Date(Kaggle$Day, origin = "1970-01-01")

# Ensure the stock prices are in time series format (Replace 'Difference' with your stock price column if needed)
ts_data <- ts(Kaggle$Difference, frequency = 365)  # Assuming daily data, adjust frequency if needed

# Apply the Holt-Winters model with beta and gamma (smoothing parameters)
hw_model <- HoltWinters(ts_data, beta = TRUE, gamma = TRUE)

# Print the model to check the smoothing parameters
print(hw_model)

# Forecasting the next 49 days
forecasted_values <- forecast(hw_model, h = 49)

# Plot the Holt-Winters forecast
plot(forecasted_values, main = "Holt-Winters Forecast for Stock Prices")

# Alternative: Apply auto.arima to optimize the model (for comparison)
auto_hw_model <- auto.arima(ts_data, seasonal = TRUE)

# Forecast using the ARIMA-based model
forecasted_values_auto_arima <- forecast(auto_hw_model, h = 49)

# Plot the ARIMA-based forecast
plot(forecasted_values_auto_arima, main = "ARIMA-based Forecast for Stock Prices")

# Evaluate accuracy of both models
cat("Holt-Winters Model Accuracy:\n")
print(accuracy(hw_model))

cat("ARIMA-based Model Accuracy:\n")
# Generate forecasts from the ARIMA model (auto_hw_model)
forecast_auto_hw_model <- forecast(auto_hw_model, h = 49)

# Now calculate the accuracy using the forecasted values
accuracy_metrics <- accuracy(forecast_auto_hw_model)
print(accuracy_metrics)


# Combine historical data with predicted future data
forecasted_data <- data.frame(Day = seq(max(Kaggle$Day) + 1, by = 1, length.out = 49),
                              PredictedStockPrice = forecasted_values$mean)

historical_data <- Kaggle[, c("Day", "Difference")]

# Ensure both data frames have the same column names
colnames(historical_data) <- c("Day", "Difference")
colnames(forecasted_data) <- c("Day", "Difference")

# Combine the two data frames
combined_data <- rbind(historical_data, forecasted_data)


# Add a new column to distinguish between historical and predicted data
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(forecasted_data)))

# Plot the combined historical and predicted data
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(aes(color = type), size = 1) +  # Plot both historical and predicted data
  labs(title = "Stock Price Prediction (Holt-Winters vs ARIMA)",
       x = "Day", y = "Stock Price Difference") +
  scale_color_manual(values = c("Historical" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Save the combined data with predictions to CSV
write.csv(combined_data, "kaggle_stock_predictions.csv", row.names = FALSE)
write.csv(forecasted_data, "kaggle_forecasted_stock_predictions.csv", row.names = FALSE)
