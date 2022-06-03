library(tidyverse)
model <- lm(mtcars$mpg ~ mtcars$disp)

plot(y = mtcars$mpg, x = mtcars$disp, xlab = "Engine Size (cubic inches)",
     ylab = "Fuel Efficiency (Miles per Gallon)", main = "Fuel Efficiency From the `mtcars` Dataset")

abline(a = coef(model[1]), b = coef(model)[2], lty = 2)

