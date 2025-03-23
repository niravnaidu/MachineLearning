library(ggplot2)
library(dplyr)
library(caret)
library(nflfastR)
library(tidyverse)
library(regclass)

PLAYERS <- load_player_stats(2019:2023)

# Summarize the data by player and season
df_totals <- PLAYERS %>%
  group_by(player_id, player_name, season) %>%
  summarise(
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(interceptions, na.rm = TRUE),
    sacks = sum(sacks, na.rm = TRUE),
    sack_yards = sum(sack_yards, na.rm = TRUE),
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    special_teams_tds = sum(special_teams_tds, na.rm = TRUE),
    fantasy_points = sum(fantasy_points, na.rm = TRUE),
    fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)
  ) %>%

arrange(player_name, season)

# Summarize the data by player and season
df_totals <- PLAYERS %>%
  group_by(player_id, player_name, season) %>%
  summarise(
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(interceptions, na.rm = TRUE),
    sacks = sum(sacks, na.rm = TRUE),
    sack_yards = sum(sack_yards, na.rm = TRUE),
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    special_teams_tds = sum(special_teams_tds, na.rm = TRUE),
    fantasy_points = sum(fantasy_points, na.rm = TRUE),
    fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)
  ) %>%
  
  
arrange(player_name, season)

# Scatterplot of Passing Yards vs. Passing Touchdowns
ggplot(df_totals, aes(x = attempts, y = passing_yards)) +
  geom_point(aes(color = as.factor(season), size = attempts), alpha = 0.7) +  # Points colored by season
  scale_color_viridis_d(option = "plasma", name = "Season") +  # Vibrant color palette
  scale_size_continuous(range = c(2, 6), name = "Pass Attempts") +  # Scale point size
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +  # Linear regression line
  labs(
    title = "Passing Attempts vs. Passing Yards (2019-2023)",
    subtitle = "Colored by Season, Sized by Pass Attempts",
    x = "Total Passing Attempts",
    y = "Total Passing Yards",
    caption = "Data: nflfastR"
  ) +
  theme_minimal(base_size = 14) +  # Clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

df_totals %>%
  filter(passing_yards >1) -> df2
# Scatterplot of Passing Yards vs. Rushing Yards
ggplot(df2, aes(x = passing_yards, y = rushing_yards)) +
  geom_point(aes(color = as.factor(season), size = attempts), alpha = 0.7) +  # Points colored by season
  scale_color_viridis_d(option = "plasma", name = "Season") +  # Vibrant color palette
  scale_size_continuous(range = c(2, 6), name = "Pass Attempts") +  # Scale point size
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +  # Linear regression line
  labs(
    title = "Passing Yards vs. Rushing Yards (2019-2023)",
    subtitle = "Colored by Season, Sized by Pass Attempts",
    x = "Total Passing Yards",
    y = "Total Rushing Yards",
    caption = "Data: nflfastR"
  ) +
  theme_minimal(base_size = 14) +  # Clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )
associate(attempts~passing_yards,data = df_totals)

associate(passing_yards~rushing_yards, data = df_totals)

axis.title = element_text(face = "bold")

hist(df_totals$attempts)
# Create a prettier histogram
ggplot(df_totals, aes(x = attempts)) +
  geom_histogram(
    bins = 30,                    # Adjust number of bins as needed
    fill = "#4CAF50",              # Nice green color
    color = "white",               # White borders for better separation
    alpha = 0.85                   # Slight transparency
  ) +
  labs(
    title = "Distribution of Attempts",
    x = "Number of Attempts",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()  # Cleaner look without minor grid lines
  )

hist(df_totals$passing_yards)

# Create a prettier histogram
ggplot(df_totals, aes(x = passing_yards)) +
  geom_histogram(
    bins = 30,                    # Adjust number of bins as needed
    fill = "#2196F3",              # Nice blue color
    color = "white",               # White borders for better separation
    alpha = 0.85                   # Slight transparency
  ) +
  labs(
    title = "Distribution of Passing Yards",
    x = "Passing Yards",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()  # Cleaner look without minor grid lines
  )

hist(df_totals$rushing_yards)
# Create a prettier histogram
ggplot(df_totals, aes(x = rushing_yards)) +
  geom_histogram(
    bins = 30,                    # Adjust number of bins as needed
    fill = "#FF9800",              # Nice orange color
    color = "white",               # White borders for better separation
    alpha = 0.85                   # Slight transparency
  ) +
  labs(
    title = "Distribution of Rushing Yards",
    x = "Rushing Yards",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()  # Cleaner look without minor grid lines
  )

summary_stats <- summary(df_totals$attempts)
summary_stats

install.packages("kableExtra")
library(kableExtra)
library(knitr)

# Generate summary statistics
summary_stats <- summary(df_totals$passing_yards)

# Convert summary to a data frame for better presentation
summary_df <- as.data.frame(t(summary_stats))
colnames(summary_df) <- c("Value")
summary_df$Statistic <- rownames(summary_df)
rownames(summary_df) <- NULL

# Display the summary statistics as a pretty table
kable(summary_df, 
      caption = "Summary Statistics for Passing Yards",
      align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                position = "center")
