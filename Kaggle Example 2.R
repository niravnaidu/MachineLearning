# Install and load necessary libraries
install.packages("forecast")
install.packages("ggplot2")
library(forecast)
library(ggplot2)

# Load data
Kaggle <- read.csv("train.csv")

# Ensure 'Day' is numeric
Kaggle$Day <- as.numeric(Kaggle$Day)

# Convert 'Difference' into a time-series object
ts_data <- ts(Kaggle$Difference, frequency = 1)  # Assuming daily stock price changes

# Fit Holt-Winters model
hw_model <- HoltWinters(ts_data, beta = FALSE, gamma = FALSE)

# Forecast the next 49 days
future_predictions <- forecast(hw_model, h = 49)

# Create a dataframe for predicted values
predicted_data <- data.frame(
  Day = seq(max(Kaggle$Day) + 1, max(Kaggle$Day) + 49, by = 1),
  Difference = as.numeric(future_predictions$mean)
)

# Combine historical and predicted data
historical_data <- Kaggle[, c("Day", "Difference")]
colnames(predicted_data) <- colnames(historical_data)
combined_data <- rbind(historical_data, predicted_data)

# Label historical and predicted data
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# Plot results
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2, shape = 16) +  
  labs(title = "Stock Price Forecast (Holt-Winters)",
       x = "Day", y = "Stock Price Difference") +
  scale_color_manual(values = c("Historical" = "#1f77b4", "Predicted" = "#ff7f0e")) +  
  theme_minimal() +
  theme(legend.title = element_blank())  

# Save predictions
write.csv(combined_data, "kaggle_stock_predictions_holt_winters.csv", row.names = FALSE)
write.csv(predicted_data, "kaggle_holt_winters_predictions.csv", row.names = FALSE)
