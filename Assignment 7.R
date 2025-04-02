library(regclass)
library(ggplot2)
library(dplyr)
library(broom)
install.packages("gridExtra")
library(gridExtra)
F1 <- read.csv("F1Drivers_Dataset.csv")

hist(F1$Points)
# Create a beautiful histogram
ggplot(F1, aes(x = Points)) +
  geom_histogram(binwidth = 1000, fill = "#0073C2", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Points",
    x = "Points",
    y = "Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

hist(F1$Race_Wins)
# Create a beautiful histogram
ggplot(F1, aes(x = Race_Wins)) +
  geom_histogram(binwidth = 5, fill = "#69b3a2", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Race Wins",
    x = "Number of Race Wins",
    y = "Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


F1Drivers <- lm(F1$Points~F1$Race_Wins)

visualize_model(F1Drivers)
glimpse(F1)

find_transformations(F1Drivers)
F1RaceEntriesYearsActive <- F1$Race_Entries / F1$Years_Active

F1two <- lm(F1$Points~F1RaceEntriesYearsActive+F1$Race_Wins)
visualize_model(F1two)
# Fit the model
F1two <- lm(F1$Points ~ F1RaceEntriesYearsActive + F1$Race_Wins, data = F1)

# Get residuals after removing the effect of other predictors
partial_data <- augment(F1two)

# Plot the effect of Race Entries Years Active
p1 <- ggplot(partial_data, aes(x = F1RaceEntriesYearsActive, y = .fitted)) +
  geom_point(color = "#0073C2", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E69F00", se = FALSE) +
  labs(title = "Effect of Race Entries Years Active", 
       x = "Years Active", 
       y = "Predicted Points") +
  theme_minimal()

# Plot the effect of Race Wins
p2 <- ggplot(partial_data, aes(x = F1$Race_Wins, y = .fitted)) +
  geom_point(color = "#009E73", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#E69F00", se = FALSE) +
  labs(title = "Effect of Race Wins", 
       x = "Race Wins", 
       y = "Predicted Points") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)


range(F1RaceEntriesYearsActive)
range(F1$Race_Wins)
df <- data.frame(F1RaceEntriesYearsActive, Race_Wins = c(0,370))
extrapolation_check2(F1two, newdata = df)


library(car)
influencePlot(F1two)
# Extract influence measures
infl <- influence.measures(F1two)
infl_data <- data.frame(
  Leverage = hatvalues(F1two),
  Studentized_Residuals = rstudent(F1two),
  CookD = cooks.distance(F1two),
  ID = rownames(F1)  # Assuming F1 has row names
)

# Define threshold for Cook’s Distance
cook_threshold <- 4 / nrow(F1)

ggplot(infl_data, aes(x = Leverage, y = Studentized_Residuals, size = CookD, label = ID)) +
  geom_point(color = "#0073C2", alpha = 0.7) +
  geom_text(aes(label = ifelse(CookD > cook_threshold, ID, "")), hjust = 1.2, vjust = 1.2, size = 4) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2 * mean(infl_data$Leverage), linetype = "dashed", color = "red") +
  labs(
    title = "Influence Plot",
    x = "Leverage",
    y = "Studentized Residuals",
    size = "Cook's Distance"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )


OUTPUT <- influencePlot(F1two)
OUTPUT[c(3,4),]
library(knitr)
library(kableExtra)

# Create a beautiful table
OUTPUT[c(3,4),] %>%
  kable(format = "html", caption = "Formatted Table with Borders") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered"), full_width = FALSE)

summary(F1two)
library(broom)
library(knitr)
library(kableExtra)

# Convert summary output into a tidy dataframe
model_summary <- tidy(F1two)

# Create a well-formatted table using kable
model_summary %>%
  kable(format = "html", caption = "Regression Summary of F1two") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered"), full_width = FALSE, position = "center")

F1$Nationality <- as.factor(F1$Nationality)
F1three <- lm(F1$Race_Starts~F1$Nationality*F1$Race_Entries, data = F1)
visualize_model(F1three)

# Create an interaction plot with regression lines
ggplot(F1, aes(x = Race_Entries, y = Race_Starts, color = Nationality)) +
  geom_point(alpha = 0.6, size = 3) +   # Scatter points for context
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.2) +  # Regression lines
  labs(
    title = "Interaction Effect: Race Entries vs. Race Starts by Nationality",
    x = "Race Entries",
    y = "Race Starts",
    color = "Nationality"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

