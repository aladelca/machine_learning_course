## Clase 03
library(tidyverse)
library(RCurl)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(ROCR)
library(pROC)


data = read_csv('https://raw.githubusercontent.com/calarcond/machine_learning_model/main/archivos_trabajo/gre_class.csv')

data$admit = as.factor(data$admit)

dist = data.frame(table(data$admit))
colnames(dist)[1] = 'class'
colnames(dist)[2] = 'freq'

ggplot(data = dist, aes(class, freq))+
  geom_col(position = 'dodge') + 
  labs(x = 'Clase', y = 'Casos', title = 'Distribución de variable objetivo') + 
  geom_text(aes(label = freq))

### Separación

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

x_test = test[,2:4]
y_test = test$admit

table(train$admit)
table(test$admit)

modelo_uno = randomForest(admit ~.,data = train,
                          nclass = 10
                          )


y_pred_uno = predict(modelo_uno, x_test)
y_pred_prob_uno = predict(modelo_uno, x_test, type = 'prob')
View(y_pred_prob_uno)

cm_uno = table(y_test,y_pred_uno)
confusionMatrix(cm_uno, positive = '1')
curva_roc_uno = roc(y_test,y_pred_prob_uno[,2])
ggroc(curva_roc_uno)
auc(curva_roc_uno)


### Oversampling

over = ovun.sample(admit~.,data = train, method = 'over')$data
table(over$admit)

modelo_dos = randomForest(admit~., data = over,
                          nclass = 10)

y_pred_dos = predict(modelo_dos, x_test)
y_pred_prob_dos = predict(modelo_dos, x_test, type = 'prob')
cm_dos = table(y_test, y_pred_dos)
confusionMatrix(cm_dos,positive = '1')
curva_roc_dos = roc(y_test,y_pred_prob_dos[,2])
ggroc(curva_roc_dos)
auc(curva_roc_dos)

library(knitr)

smote_data = ubBalance(train[,c(2:4)],
                       train[,c(1)],
                       type = 'ubOver')


### Undersampling
