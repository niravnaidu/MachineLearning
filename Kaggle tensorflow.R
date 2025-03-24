# Install and load necessary libraries
install.packages("keras")
install.packages("tensorflow")
install.packages("ggplot2")
library(keras)
library(tensorflow)
library(ggplot2)

install_tensorflow()


# Set random seed for reproducibility
tensorflow::tf$random$set_seed(42)

# Load dataset
Kaggle <- read.csv("train.csv")

# Ensure 'Day' is numeric
Kaggle$Day <- as.numeric(Kaggle$Day)

# Normalize the 'Difference' column for LSTM stability
max_diff <- max(Kaggle$Difference, na.rm = TRUE)
min_diff <- min(Kaggle$Difference, na.rm = TRUE)
Kaggle$Normalized_Diff <- (Kaggle$Difference - min_diff) / (max_diff - min_diff)

# Create sequences for LSTM (using past 10 days to predict next day)
create_sequences <- function(data, seq_length) {
  X <- list()
  Y <- c()
  for (i in seq_length:(nrow(data) - 1)) {
    X[[i - seq_length + 1]] <- as.matrix(data[(i - seq_length + 1):i, "Normalized_Diff"])
    Y <- c(Y, data[i + 1, "Normalized_Diff"])
  }
  return(list(X = abind::abind(X, along = 1), Y = Y))
}

# Set sequence length
seq_length <- 10

# Normalize data
Kaggle$Normalized_Diff <- (Kaggle$Difference - min(Kaggle$Difference)) / 
  (max(Kaggle$Difference) - min(Kaggle$Difference))

# Create sequences for LSTM
library(abind)
dataset <- create_sequences(Kaggle, seq_length)

# Split data into training (80%) and testing (20%)
train_size <- floor(0.8 * length(dataset$Y))
X_train <- dataset$X[1:train_size,,drop=FALSE]
Y_train <- dataset$Y[1:train_size]
X_test <- dataset$X[(train_size+1):length(dataset$Y),,drop=FALSE]
Y_test <- dataset$Y[(train_size+1):length(dataset$Y)]

# Reshape input for LSTM (samples, timesteps, features)
X_train <- array(X_train, dim = c(nrow(X_train), seq_length, 1))
X_test <- array(X_test, dim = c(nrow(X_test), seq_length, 1))

# Build LSTM Model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(seq_length, 1), return_sequences = TRUE) %>%
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dense(units = 25) %>%
  layer_dense(units = 1, activation = "linear")

# Compile the model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("mae")
)



# Train the model
history <- model %>% fit(
  X_train, Y_train,
  epochs = 50,
  batch_size = 16,
  validation_data = list(X_test, Y_test),
  verbose = 1
)

# Predict future 49 days
future_predictions <- numeric(49)
last_sequence <- X_test[nrow(X_test),,drop=FALSE]  # Start with last known sequence

for (i in 1:49) {
  pred <- model %>% predict(array(last_sequence, dim = c(1, seq_length, 1)))
  future_predictions[i] <- pred
  last_sequence <- c(last_sequence[-1], pred)  # Shift sequence and append new prediction
}

# Convert predictions back to original scale
future_predictions <- future_predictions * (max_diff - min_diff) + min_diff

# Create dataframe for predictions
predicted_data <- data.frame(
  Day = seq(max(Kaggle$Day) + 1, max(Kaggle$Day) + 49, by = 1),
  Difference = future_predictions
)

# Combine historical and predicted data
historical_data <- Kaggle[, c("Day", "Difference")]
combined_data <- rbind(historical_data, predicted_data)

# Add type labels
combined_data$type <- c(rep("Historical", nrow(historical_data)), rep("Predicted", nrow(predicted_data)))

# Plot results
ggplot(combined_data, aes(x = Day, y = Difference, color = type)) +
  geom_line(size = 1.5) +  
  geom_point(size = 2, shape = 16) +  
  labs(title = "Stock Price Forecast (LSTM)",
       x = "Day", y = "Stock Price Difference") +
  scale_color_manual(values = c("Historical" = "#1f77b4", "Predicted" = "#ff7f0e")) +  
  theme_minimal() +
  theme(legend.title = element_blank())

# Save predictions
write.csv(combined_data, "kaggle_stock_predictions_lstm.csv", row.names = FALSE)
write.csv(predicted_data, "kaggle_lstm_predictions.csv", row.names = FALSE)
