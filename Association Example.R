library(regclass)
library(ggplot2)
library(vcd)
library(ggmosaic)
data("ATTRACTM")

# side-by-side boxplot, normal plot
associate(HappinessRating~Piercings,data = ATTRACTM,
          permutations = 500, seed = 123)
ggplot(ATTRACTM, aes(x = as.factor(Piercings), y = HappinessRating, fill = as.factor(Piercings))) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.color = "red") + 
  scale_fill_manual(values = c("#FF9999", "#66B2FF")) + # Custom colors
  labs(title = "Happiness Rating by Piercings",
       x = "Piercings (Yes/No)",
       y = "Happiness Rating") +
  theme_minimal() +  # Cleaner theme
  theme(legend.position = "none", text = element_text(size = 14))

# mosaic plot
associate(SkinClearScore~Smile,data = ATTRACTM,
          permutations = 500,seed = 123)
ggplot(data = ATTRACTM) +
  geom_mosaic(aes(x = product(Smile), fill = SkinClearScore)) +
  labs(title = "Mosaic Plot: Skin Clarity vs. Smile",
       x = "Smile Presence",
       y = "Skin Clarity",
       fill = "Skin Clarity Score") +
  scale_fill_viridis_d() +  # Use a nice color palette
  theme_minimal() +         # Clean background
  theme(text = element_text(size = 14))