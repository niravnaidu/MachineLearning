#1 Get data
#2 Get data into R
library(ggplot2)
library(dplyr)
library(flextable)
install.packages("qcc")
library(qcc)
NBA <- read.csv("PSinfo.csv", stringsAsFactors = TRUE)

#3 Make graphics
hist(NBA$age) 


nba_age <- NBA$age  # Extract the vector

pareto.chart(NBA$age)

# Compute summary statistics
nba_summary <- data.frame(
  Statistic = c("Median", "Mean", "Standard Deviation", "Minimum", "Maximum", "Count"),
  Value = c(
    round(median(nba_age, na.rm = TRUE), 1),
    round(mean(nba_age, na.rm = TRUE), 1),
    round(sd(nba_age, na.rm = TRUE), 1),
    round(min(nba_age, na.rm = TRUE), 1),
    round(max(nba_age, na.rm = TRUE), 1),
    length(nba_age)  # Count remains an integer
  )
)

# Create a beautifully formatted flextable
nba_summary %>%
  flextable() %>%

  theme_vanilla() %>%  # Apply a clean theme
  set_caption("📊 NBA Players Age Summary Statistics") %>%  # Add a title
  bold(part = "header") %>%  # Make headers bold
  color(i = 1, color = "blue") %>%  # Highlight the Median row
  bg(i = 2, bg = "lightblue") %>%  # Highlight the Mean row
  align(align = "center", part = "all") %>%  # Center align text
  autofit()  # Automatically adjust column widths



table(NBA$lg)
nba_lg_df <- as.data.frame(table(NBA$lg)) %>%
  arrange(desc(Freq))

# Create a styled flextable
nba_lg_df %>%
  flextable() %>%
  theme_vanilla() %>%  # Clean vanilla theme
  set_caption("🏀 NBA League Distribution") %>%  # Add a title
  color(j = "Freq", color = "blue") %>%  # Make frequency column blue
  bold(part = "header") %>%  # Bold headers
  bg(i = 1, bg = "white") %>%  # Highlight the highest frequency row
  align(align = "center", part = "all") %>%  # Center-align everything
  autofit()  # Adjust column widths automatically

plot(NBA$lg)
ggplot(NBA, aes(x = lg))
  geom_bar(fill = "steelblue", color = "black") +  # Add color
  theme_minimal() +  # Use a clean theme
  labs(title = "NBA League Distribution", x = "League", y = "Count") +  # Add labels
  theme(text = element_text(size = 14),  # Increase text size
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



#4 Interpret graphics
#5 Get summary statistics
#6 Interpret summary stats
#7 Make it look pretty


