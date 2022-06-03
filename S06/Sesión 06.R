library(e1071)
library(caret)

#### Split train - test

data(mtcars)
mtcars
split = sample(c(rep(0,0.7*nrow(mtcars)),rep(1,0.3*nrow(mtcars))))
split[32] = 1

mtcars$split = split

train = mtcars[split == 0,]
test = mtcars[split == 1,]

#### Training linear regression model

modelo_rl = lm(mpg ~ cyl + disp + hp, data = train)
summary(modelo_rl)

####### Predictions and results

y_pred_rl = predict(modelo_rl,test)
mse_rl = mean((test$mpg - y_pred_rl)^2)

#### RandomForest

library(randomForest)

modelo_rf = randomForest(mpg ~ cyl + disp + hp,
                         data = train,
                         ntree = 100)

plot(modelo_rf)

y_pred_rf = predict(modelo_rf, test)
mse_rf = mean((test$mpg - y_pred_rf)^2)

#### Support Vector Regressor

modelo_svm = svm(mpg ~ cyl + disp + hp,
                 data = train,
                 kernel = 'linear')

y_pred_svm = predict(modelo_svm, test)
mse_svm = mean((test$mpg - y_pred_svm)^2)

mae_svm = mean(abs(test$mpg - y_pred_svm))
mae_rf = mean(abs(test$mpg - y_pred_rf))

##### Function for calculate MSE

mse_calculado = function(x,y){mean((x-y)^2)}

mse_calculado(test$mpg,y_pred_rf)
mse_calculado(test$mpg,y_pred_svm)
mse_calculado(test$mpg,y_pred_rl)


######## Competencia


View(train)


library(tidyverse)
library(tidyr)


train_select = train %>%  select(LotFrontage,LotArea,OverallQual,OverallCond, MasVnrArea,BsmtFinSF1,TotalBsmtSF, BsmtFullBath, BsmtHalfBath,
                                 FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd,SalePrice)


train_select$TimeRem = train$YearRemodAdd - train$YearBuilt

drop_na(train_select$SalePrice)

train_select_df = train_select %>% filter(!is.na(SalePrice))

train_select_df = train_select_df %>% filter(!is.na(LotFrontage))
train_select_df[is.na(train_select_df$OverallCond),]

train_final = train_select_df %>% select(LotFrontage, LotArea, BedroomAbvGr, SalePrice)

train_final

modelo_uno = randomForest(SalePrice ~., data = train_final, ntree = 40)

plot(modelo_uno)




test_final = test %>% select(LotFrontage,LotArea, BedroomAbvGr)
y_pred = data.frame(predict(modelo_uno,test_final))

media = mean(y_pred$predict.modelo_uno..test_final., na.rm = TRUE)
media
y_pred_nuevo = y_pred %>%  filter (is.na(y_pred$predict.modelo_uno..test_final.) %>%  mutate(predict.modelo_uno..test_final.,media)

y_pred_nuevo
                                   
y_pred$Id = test$Id
y_pred

write.csv(y_pred,file = 'C:/Users/Adrián Alarcón/Downloads/resultados_competencia.csv')


