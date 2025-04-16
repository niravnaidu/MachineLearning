library(regclass)
library(ggplot2)
library(dplyr)
library(flextable)

cars <- read.csv("Automobile_data.csv")
flextable(head(cars, 5))

library(kableExtra)

kable(head(cars, 5), caption = "First Five Rows of Automobile Data") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE)


cars$fuel.type <- as.factor(cars$fuel.type)

cars1 <- glm(cars$fuel.type~cars$compression.ratio, data = cars,
             family = binomial)
visualize_model(cars1)

cars$predicted_prob <- predict(cars1, type = "response")
# Create the plot
ggplot(cars, aes(x = compression.ratio, y = predicted_prob)) +
  geom_point(aes(color = fuel.type), size = 3, alpha = 0.7) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "steelblue", fill = "lightblue", linetype = "solid") +
  labs(
    title = "Predicted Probability of Fuel Type by Compression Ratio",
    x = "Compression Ratio",
    y = "Probability of Fuel Type (e.g., Diesel)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
summary(cars1)
library(kableExtra)
library(broom)
library(knitr)

cars1 %>%
  tidy() %>%
  kable(caption = "Logistic Regression Summary: fuel.type ~ compression.ratio", digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
cars$peak.rpm <- as.numeric(cars$peak.rpm)

cars2 <- glm(fuel.type~compression.ratio+peak.rpm, data = cars,
             family = binomial)
visualize_model(cars2)

# Pick a representative peak.rpm value
peak_fixed <- median(cars$peak.rpm, na.rm = TRUE)

# Generate predictions across compression.ratio
plot_data <- data.frame(
  compression.ratio = seq(min(cars$compression.ratio), max(cars$compression.ratio), length.out = 100),
  peak.rpm = peak_fixed
)

plot_data$predicted_prob <- predict(cars2, newdata = plot_data, type = "response")

ggplot(plot_data, aes(x = compression.ratio, y = predicted_prob)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(
    title = paste("Predicted Probability of Fuel Type (Peak RPM =", round(peak_fixed), ")"),
    x = "Compression Ratio",
    y = "Probability of Gas"
  ) +
  theme_minimal(base_size = 14)

VIF(cars2)

library(car)
library(knitr)

# Calculate the VIF for the dataset
vif_values <- vif(cars2)

# Convert VIF values into a data frame for easy visualization
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

# Create a beautiful table using kable
kable(vif_df, caption = "Variance Inflation Factor (VIF) for car dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
