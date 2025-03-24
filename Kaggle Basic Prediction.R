library(regclass)

Kaggle <- read.csv("train.csv")

Stocks <- lm(Difference~Day, data = Kaggle)
visualize_model(Stocks)

max_day <- max(Kaggle$Day)

# Create a dataframe for the next 49 days
next_49_days <- data.frame(Stocks$model)


# Predict for the next 49 days
future_predictions <- predict(Stocks, newdata = next_49_days)



print(future_predictions)

# Load necessary libraries
library(ggplot2)

# Step 1: Prepare the data (assuming 'Kaggle' has 'Day' and 'StockPrice')
# If 'Kaggle$Day' is not numeric, convert it to numeric
Kaggle$Day <- as.numeric(Kaggle$Day)

# Step 2: Build the linear model
Stocks <- lm(Difference ~ Day, data = Kaggle)

# Step 3: Make predictions for the next 49 days
max_day <- max(Kaggle$Day, na.rm = TRUE)  # Get the max day from the existing data
next_49_days <- data.frame(Day = seq(from = max_day + 1, to = max_day + 49, by = 1))

# Predict stock prices for the next 49 days
future_predictions <- predict(Stocks, newdata = next_49_days)

# Step 4: Prepare the prediction data
predicted_data <- data.frame(Day = next_49_days$Day, PredictedStockPrice = future_predictions)

# Combine with the original data for plotting
colnames(Kaggle)

# If the columns are different, use the correct names
historical_data <- Kaggle[, c("Day", "Difference")]

# Rename columns if needed
colnames(historical_data) <- c("Day", "Difference")

colnames(historical_data)
colnames(predicted_data)

# Ensure column names are the same in both dataframes
colnames(predicted_data) <- colnames(historical_data)

# Now combine the dataframes
combined_data <- rbind(historical_data, predicted_data)


# Step 5: Visualize the data with ggplot2
# Add a new column to combined_data to distinguish between historical and predicted
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# Now plot using ggplot2
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(aes(color = type), size = 1) +  # Plot both historical and predicted data
  labs(title = "Stock Price Prediction",
       x = "Day", y = "Stock Price") +
  scale_color_manual(values = c("Historical" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title

install.packages("ggfortify")


library(ggfortify)
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# Plot with ggfortify
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(aes(color = type), size = 1.5) +  # Plot both historical and predicted data with thicker lines
  geom_point(aes(color = type), size = 2, shape = 16) +  # Add points to the lines for clarity
  labs(title = "Stock Price Prediction", x = "Day", y = "Stock Price Difference") +
  scale_color_manual(values = c("Historical" = "#1f77b4", "Predicted" = "#ff7f0e")) +  # Use custom colors
  theme(legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title styling
        axis.title = element_text(size = 14),  # Axis title styling
        axis.text = element_text(size = 12))  # Axis text styling

write.csv(combined_data, "kaggle stock predictions.csv")

write.csv(predicted_data, "kaggle3.csv")


# Load necessary libraries



#predict()

# make a new data frame to predict the x-variable

#