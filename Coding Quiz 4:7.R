library(regclass)

data("SURVEY10")

M <- lm(Height~Weight+Gender, data = SURVEY10)
visualize_model(M)

summary(M)
