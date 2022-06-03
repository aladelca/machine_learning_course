library(tidyverse)
library(caret)
library(ROCR)
library(pROC)
library(fastAdaboost)
library(xgboost)

data = read_csv('https://raw.githubusercontent.com/calarcond/machine_learning_model/main/archivos_trabajo/gre_class.csv')

ind = sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
train = data[ind == 1,]
test = data[ind == 2,]

train_scale = data.frame(scale(train[,c(2,3,4)]))
train_scale$admit = train$admit

#### Adaboost


modelo_adaboost = fastAdaboost::adaboost(admit~., data = train_scale, 1000)

y_test = test$admit
x_test = test[,c(2,3,4)]
x_test_scale = data.frame(scale(x_test))


y_pred_adaboost = predict(modelo_adaboost, newdata = x_test_scale)

y_pred_adaboost$class
y_pred_adaboost$prob

cm_adaboost = table(y_test,y_pred_adaboost$class)
confusionMatrix(cm_adaboost, positive = '1')
curva_roc_adaboost = roc(y_test, y_pred_adaboost$prob[,2])
ggroc(curva_roc_adaboost)
auc(curva_roc_adaboost)

#### XGBoost

library(xgboost)

y_train = data.matrix(train$admit)
x_train = data.matrix(train[,c(2,3,4)])
y_test = data.matrix(test$admit)
x_test = data.matrix(test[,c(2,3,4)])



xgb_train = xgb.DMatrix(data = x_train, label =  y_train)
xgb_test = xgb.DMatrix(data = x_test, label = y_test)

modelo_xgboost = xgboost(data = xgb_train, nrounds = 1000, objective = 'binary:logistic')

y_pred_xgboost = predict(modelo_xgboost, xgb_test)
y_pred_xgboost = as.numeric(y_pred_xgboost>0.5)
cm_xgboost = table(y_test,y_pred_xgboost)
confusionMatrix(cm_xgboost,positive = '1')
roc_xgboost = roc(y_test,y_pred_xgboost)
ggroc(roc_xgboost)
auc(roc_xgboost)
auc(curva_roc_adaboost)


#### RandomForest

library(randomForest)

train$admit = as.factor(train$admit)
test$admit = as.factor(test$admit)

x_train = train[,2:4]
y_train = train$admit
x_test = test[,2:4]
y_test = test$admit

modelo_rf = randomForest(admit~., data =train,
                         ntree = 100)

y_pred_rf = predict(modelo_rf,x_test)
y_pred_prob_rf = predict(modelo_rf, x_test, type = 'prob')

cm_rf = table(y_test,y_pred_rf)
confusionMatrix(cm_rf,positive = '1')
curva_roc_rf = roc(y_test,y_pred_prob_rf[,2])
ggroc(curva_roc_rf)


auc(curva_roc_rf)
auc(roc_xgboost)
auc(curva_roc_adaboost)





