library(tidyverse)
library(caret)
library(randomForest)
library(ROCR)
library(pROC)

data = read_csv('https://raw.githubusercontent.com/calarcond/machine_learning_model/main/archivos_trabajo/gre_class.csv')
data$admit = as.factor(data$admit)


table(data$admit)

ind = sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
train = data[ind==1,]
test = data[ind==2,]

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

### Threshold = 0.6

y_pred_th06 = y_pred_prob_uno[y_pred_prob_uno<0.6]=0
y_pred_prob_uno[y_pred_prob_uno>=0.6]=1
y_pred_prob_uno[,1]

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