library(tidyverse)
library(caret)
library(randomForest)
library(ROCR)
library(pROC)
library(adabag)

data = read_csv('https://raw.githubusercontent.com/calarcond/machine_learning_model/main/archivos_trabajo/gre_class.csv')
data$admit = as.factor(data$admit)


table(data$admit)

ind = sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
train = data[ind==1,]
test = data[ind==2,]

x_train = train[,2:4]
y_train = train$admit

x_test = test[,2:4]
y_test = test$admit

modelo_uno = randomForest(admit ~.,
                          data = train,
                          ntree = 100)

plot(modelo_uno)
importance(modelo_uno)

modelo_uno$forest

y_pred_uno = predict(modelo_uno,x_test)
y_pred_prob_uno = predict(modelo_uno,x_test, type = 'prob')


#####

cm_uno = table(y_test,y_pred_uno)
confusionMatrix(cm_uno, positive = '1')

curva_roc_uno = roc(y_test, y_pred_prob_uno[,2])
ggroc(curva_roc_uno)
auc(curva_roc_uno)

#### Cambio en la matriz de confusion

cm_dos = table(y_test,y_pred_prob_uno[,1])
confusionMatrix(cm_dos, positive = '1')



#AUC = 0.6575
#Accuracy = 0.6792


#### Adaboost


x_train_scale = data.frame(scale(train[,c(2,3,4)]))
x_train_scale$admit = train$admit
x_train_scale
train$admit = as.numeric(train$admit)


library(fastAdaboost)
modelo_tres = fastAdaboost::adaboost(admit~., data = x_train_scale,1000)

y_pred_tres = predict(modelo_tres, newdata = x_test)
cm_tres = table(y_test, y_pred_tres$class)
confusionMatrix(cm_tres, positive = '1')
curva_roc_tres = roc(y_test, y_pred_tres$prob[,2])
ggroc(curva_roc_tres)
auc(curva_roc_tres)


mean(y_pred_tres$class == y_test)


library(xgboost)

train = train %>% mutate(admit = case_when(admit == '1'~1, TRUE~0))
x_train = data.matrix(subset(train, select = -c(1)))
y_train = train$admit

test = test %>% mutate(admit = case_when(admit == '1'~1, TRUE~0))
x_test = data.matrix(subset(test, select = -c(1)))
y_test = test$admit


xgb_train = xgb.DMatrix(data = x_train, label = y_train)
xgb_test = xgb.DMatrix(data = x_test, label = y_test)


modelo_cuatro = xgboost(data = xgb_train, nrounds = 100, objective = 'binary:logistic')

summary(modelo_cuatro)

y_pred_cuatro = predict(modelo_cuatro,xgb_test)
y_pred_cuatro = as.numeric(y_pred_cuatro>0.1)
cm_cuatro = table(y_test,y_pred_cuatro)
confusionMatrix(cm_cuatro,positive = '1')
xgb.importance(model = modelo_cuatro)
roc_cuatro = roc(y_test,y_pred_cuatro)
auc(roc_cuatro)
ggroc(roc_cuatro)
