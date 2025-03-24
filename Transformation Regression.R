library(regclass)

nba_data <- read.csv("all_seasons_nba.csv")

hist(nba_data$x2)
# Load necessary libraries
library(ggplot2)

# Create a beautifully formatted histogram
ggplot(nba_data, aes(x = x2)) +
  geom_histogram(binwidth = 5, fill = "#FF5733", color = "white", alpha = 0.85) +  # Orange fill with white borders
  theme_minimal() +  # Clean and modern theme
  labs(title = "Distribution of Weight in NBA Data",
       x = "Weight (kilograms)",
       y = "Frequency") +  # Proper axis labels
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered bold title
    axis.title = element_text(face = "bold", size = 12),  # Bold axis labels
    axis.text = element_text(size = 10),  # Readable axis text
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soft grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for a clean look
  )

hist(nba_data$player_height)
# Create a beautifully formatted histogram
ggplot(nba_data, aes(x = player_height)) +
  geom_histogram(binwidth = 2, fill = "#3498DB", color = "white", alpha = 0.85) +  # Blue fill with white borders
  theme_minimal() +  # Clean and modern theme
  labs(title = "Distribution of NBA Player Heights",
       x = "Player Height (centimeters)",
       y = "Frequency") +  # Proper axis labels
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered bold title
    axis.title = element_text(face = "bold", size = 12),  # Bold axis labels
    axis.text = element_text(size = 10),  # Readable axis text
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soft grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for a cleaner look
  )

summary(nba_data$x2)
# Load necessary libraries
library(knitr)
library(kableExtra)
library(dplyr)

# Compute summary statistics
x2_summary <- summary(nba_data$x2)

# Convert to a dataframe
summary_df <- data.frame(
  Statistic = names(x2_summary),
  Value = as.numeric(x2_summary)
)

# Create a beautifully formatted kable table
summary_table <- summary_df %>%
  kable("html", caption = "Summary Statistics of X2 in NBA Data") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(1, bold = TRUE, color = "white", background = "#1E90FF") %>%  # Stylish header column
  column_spec(2, background = "#F8F9FA") %>%  # Light gray for the second column
  row_spec(0, bold = TRUE, background = "#4CAF50", color = "white")  # Header row styling

# Print the table
summary_table




# Compute summary statistics
weight_summary <- summary(nba_data$player_weight)

# Convert to a dataframe
summary_df <- data.frame(
  Statistic = names(weight_summary),
  Value = as.numeric(weight_summary)
)

# Create a beautifully formatted kable table
summary_table <- summary_df %>%
  kable("html", caption = "Summary Statistics of NBA Player Weights") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(1, bold = TRUE, color = "white", background = "#1E90FF") %>%  # Stylish header column
  column_spec(2, background = "#F8F9FA") %>%  # Light gray for the second column
  row_spec(0, bold = TRUE, background = "#4CAF50", color = "white")  # Header row styling

# Print the table
summary_table

summary(nba_data$player_height)
# Load necessary libraries
library(knitr)
library(kableExtra)
library(dplyr)

# Compute summary statistics
height_summary <- summary(nba_data$player_height)

# Convert to a dataframe
summary_df <- data.frame(
  Statistic = names(height_summary),
  Value = as.numeric(height_summary)
)

# Create a beautifully formatted kable table
summary_table <- summary_df %>%
  kable("html", caption = "Summary Statistics of NBA Player Heights") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(1, bold = TRUE, color = "white", background = "#1E90FF") %>%  # Stylish header column
  column_spec(2, background = "#F8F9FA") %>%  # Light gray for the second column
  row_spec(0, bold = TRUE, background = "#4CAF50", color = "white")  # Header row styling

# Print the table
summary_table
nba <- lm(player_weight~player_height, data = nba_data)
visualize_model(nba)
summary(nba)
# Load necessary library
library(ggplot2)

# Fit the linear model
nba <- lm(player_weight ~ player_height, data = nba_data)

# Extract R-squared value
r_squared <- summary(nba)$r.squared
r_squared_text <- paste0("R² = ", round(r_squared, 3))  # Format with 3 decimal places

# Create the plot
ggplot(nba_data, aes(x = player_height, y = player_weight)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) +  # Regression line with confidence interval
  labs(
    title = "NBA Player Height vs. Weight",
    x = "Player Height (cm)",
    y = "Player Weight (kg)",
    caption = "Data Source: nba_data"
  ) +
  annotate("text", x = max(nba_data$player_height) - 2, 
           y = min(nba_data$player_weight) + 10, 
           label = r_squared_text, size = 5, color = "black", fontface = "bold") +  # Add R² annotation
  theme_minimal(base_size = 14) +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )

# Load necessary libraries
library(knitr)
library(kableExtra)
library(broom)  # For tidying model outputs

# Fit the linear model
nba <- lm(player_weight ~ player_height, data = nba_data)

# Convert model summary into a tidy data frame
nba_table <- tidy(nba)  # Extracts coefficients, estimates, std error, t-value, and p-value

# Round numeric values for better readability
nba_table <- nba_table %>%
  mutate(across(where(is.numeric), round, digits = 3))

# Create a formatted table using kable
kable(nba_table, format = "html", caption = "Regression Model Summary: Player Height vs. Weight") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#0073C2", color = "white", bold = TRUE)



check_regression(nba)

install.packages("ggthemes")
library(ggfortify)
autoplot(nba, which = 1:1)
# Load necessary libraries
library(ggplot2)
library(ggthemes)  # Extra themes for styling

# Compute residuals
nba_data$residuals <- resid(nba)
nba_data$fitted_values <- fitted(nba)

# Create the residual plot
ggplot(nba_data, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Scatter plot for residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Reference line at y = 0
  labs(
    title = "Residual Plot: NBA Player Height vs. Weight",
    x = "Fitted Values (Predicted Player Weight)",
    y = "Residuals",
    caption = "Residuals should be randomly scattered around zero"
  ) +
  theme_minimal(base_size = 14) +  # Clean minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )


find_transformations(nba)

nba_data$ytran <- nba_data$player_height^(-2.25)
nba_data$xtran <- nba_data$player_weight^(-3)

nba2 <- lm(ytran~xtran, data = nba_data)
visualize_model(nba2)
autoplot(nba2, which = 1:2)

# Extract R-squared value
r_squared <- summary(nba2)$r.squared

# Create a styled scatter plot with regression line
ggplot(nba_data, aes(x = xtran, y = ytran)) +
  geom_point(color = "dodgerblue", alpha = 0.6, size = 2) +  # Scatter points
  geom_smooth(method = "lm", color = "firebrick", fill = "lightcoral", alpha = 0.3) +  # Regression line
  labs(title = "Linear Relationship Between xtran and ytran",
       x = "Transformed X",
       y = "Transformed Y",
       caption = paste("R² =", round(r_squared, 3))) +  # Display R-squared
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  annotate("text", x = max(nba_data$xtran) * 0.7, 
           y = max(nba_data$ytran) * 0.9, 
           label = paste("R² =", round(r_squared, 3)), 
           color = "firebrick", size = 5, fontface = "bold")



visualize_model(nba2)
summary(nba2)
check_regression(nba2)
autoplot(nba2, which = 1:1)


AIC(nba)
AIC(nba2)

summary(nba)
# Load necessary libraries
library(knitr)
library(kableExtra)
library(broom)  # For cleaning regression outputs
library(dplyr)  # For data manipulation

# Fit the linear model
nba <- lm(player_weight ~ player_height, data = nba_data)

# Extract model coefficients into a clean table
nba_coef <- tidy(nba) %>%
  mutate(across(where(is.numeric), round, digits = 3))  # Round numbers for clarity

# Extract model statistics (R², Adjusted R², p-value, etc.)
nba_stats <- glance(nba) %>%
  select(r.squared, adj.r.squared, p.value, sigma, df) %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  rename(
    `R-Squared` = r.squared,
    `Adj. R-Squared` = adj.r.squared,
    `p-Value` = p.value,
    `Residual Std. Error` = sigma,
    `Degrees of Freedom` = df
  )

# Create a formatted coefficients table
kable(nba_coef, format = "html", caption = "Regression Coefficients: Player Height vs. Weight") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#0073C2", color = "white", bold = TRUE)

# Create a second formatted table for model statistics
kable(nba_stats, format = "html", caption = "Model Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#0073C2", color = "white", bold = TRUE)

summary(nba2)
# Fit the linear model
nba2 <- lm(ytran ~ xtran, data = nba_data)

# Extract model coefficients into a clean table
nba2_coef <- tidy(nba2) %>%
  mutate(across(where(is.numeric), round, digits = 3))  # Round numbers for clarity

# Extract model summary statistics
nba2_stats <- glance(nba2) %>%
  select(r.squared, adj.r.squared, p.value, sigma, df) %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  rename(
    `R-Squared` = r.squared,
    `Adj. R-Squared` = adj.r.squared,
    `p-Value` = p.value,
    `Residual Std. Error` = sigma,
    `Degrees of Freedom` = df
  )

# Create a formatted table for regression coefficients
kable(nba2_coef, format = "html", caption = "Regression Coefficients: Player Height vs. Weight") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#0073C2", color = "white", bold = TRUE)

# Create a second formatted table for model summary statistics
kable(nba2_stats, format = "html", caption = "Model Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, background = "#0073C2", color = "white", bold = TRUE)
