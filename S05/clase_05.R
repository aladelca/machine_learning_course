###### Problemas de regresión

library(tidyverse)

data = mtcars

###Modelo lineal

model = lm(data$mpg ~ data$disp)

plot(y = data$mpg, x = data$disp)
abline(a = coef(model[1]), b = coef(model)[2], lty = 2)

summary(model)


### Modelo lineal múltiple

model_mult = lm(mpg ~ disp + wt + hp, data = data)

summary(model_mult)



model_todas = lm(mpg ~ ., data = data)
summary(model_todas)


### Regularización Lasso

library(lasso2)
model_lasso = l1ce(mpg ~ ., data = data)
summary(model_lasso)$coefficients


pob = data.frame(uspop)
pob$uspop = as.numeric(pob$uspop)
pob$year = seq(from = 1790, to = 1970,by = 10)

plot(y = pob$uspop, x = pob$year)


model_lpob = lm(pob$uspop ~ pob$year)
summary(model_lpob)


plot(y = pob$uspop, x = pob$year)
abline(a = coef(model_lpob[1]), b = coef(model_lpob)[2], lty = 2, col = 'red')


#### Modelo cuadrático



model_ppob = lm(pob$uspop ~ poly(pob$year,2))
summary(model_ppob)


plot(y = pob$uspop, x = pob$year)
lines(sort(pob$year), fitted(model_ppob)[order(pob$year)], col = 'red', lty = 2)

pob_2022 = data.frame(year = c(1980,1990,2000,2010), uspop = c(227.8,253.7,290.8,315.4))


x_test = data.frame(pob_2022$year)



y_pred = predict(model_ppob,data = pob_2022)

###mse_manual

predict(lm(uspop ~ poly(year,2)), data = pob_2022)


pred = c(229.1,267.8,287.2,316.4)
pob_2022$uspop


###MSE

sum((pob_2022$uspop - pred)^2)/4
