library(regclass)
library(tidyverse)
data("mtcars")

m <- lm(mpg~disp, data = mtcars)

visualize_model(m)

find_transformations(m)

mtcars$ytran <- mtcars$mpg^(3)
mtcars$xtran <- mtcars$disp^(-2.25)

m <- lm(ytran~xtran, data = mtcars)
visualize_model(m)
check_regression(m)
#change for git

install.packages("ggfortify")
library(ggfortify)
autoplot(m, which = 1:2)

choose_order(m)

m <- lm(mpg~poly(disp, 5), data = mtcars)

#VIF - variance inflation factor explains x's overlapping. Best if equal to 0.


VIF(m)
