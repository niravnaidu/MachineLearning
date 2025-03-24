library(regclass)
library(ggplot2)
data("mtcars")

# Fit linear model
M <- lm(mpg ~ disp, data = mtcars)
# Create scatterplot with regression line
ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE, linetype = "dashed") + # Regression line with confidence interval
  labs(title = "Scatterplot of MPG vs. Displacement",
       subtitle = "With Linear Regression Line",
       x = "Displacement (cu. in.)",
       y = "Miles per Gallon (MPG)") +
  theme_minimal(base_size = 15) +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

# Visualize model using ggplot2
ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Styled scatter plot
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE, linetype = "dashed") + # Regression line with confidence interval
  labs(title = "Linear Regression of MPG vs. Displacement",
       x = "Displacement (cu. in.)",
       y = "Miles per Gallon (MPG)") +
  theme_minimal(base_size = 15) +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Center and bold title

M <- lm(mpg ~ disp, data = mtcars)

# Create a dataframe for residuals
residual_data <- data.frame(
  Fitted_Values = fitted(M),
  Residuals = resid(M)
)

# Create a pretty residuals plot
ggplot(residual_data, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Residual points
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +  # Reference line at 0
  labs(title = "Residuals Plot",
       subtitle = "Assessing Homoscedasticity and Model Fit",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal(base_size = 15) +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

# Summary of model
library(broom)
library(flextable)

summary(M)
model_summary <- tidy(M)  # Converts summary output into a clean dataframe

# Convert to a pretty table
kable(model_summary, digits = 3, caption = "Regression Model Summary: MPG vs. Displacement") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(2:5, color = "blue")  # Make numerical values blue

library(kableExtra)
library(knitr)

hist(mtcars$mpg)
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 3, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Miles Per Gallon (MPG)",
       x = "Miles Per Gallon (MPG)",
       y = "Count") +
  theme_minimal(base_size = 15) +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title


hist(mtcars$disp)
ggplot(mtcars, aes(x = disp)) +
  geom_histogram(binwidth = 50, fill = "cornflowerblue", color = "white", alpha = 0.9) +
  labs(title = "Distribution of Engine Displacement",
       x = "Displacement (cu. in.)",
       y = "Count") +
  theme_minimal(base_size = 15) +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

library(dplyr)
library(knitr)
library(kableExtra)
library(tibble)

summary(mtcars$mpg)
# Get summary statistics and convert to a dataframe
summary_stats <- summary(mtcars$mpg) %>%
  enframe(name = "Statistic", value = "Value")  # Convert named vector to a tibble

# Convert to a pretty table
kable(summary_stats, col.names = c("Statistic", "MPG"), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(2, color = "blue")  # Make values blue for readability


summary(mtcars$disp)
# Get summary statistics and convert to a tibble
summary_stats <- summary(mtcars$disp) %>%
  enframe(name = "Statistic", value = "Value")  # Convert named vector to a tibble

# Convert to a pretty table
kable(summary_stats, col.names = c("Statistic", "Displacement"), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(2, color = "blue")  # Make values blue for readability



