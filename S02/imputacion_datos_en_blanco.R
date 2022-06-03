library(tidyverse)


library(ggplot2)

ggplot(data = dataframe, aes(x = X20)) + geom_histogram(bins = 20)

link = 'https://raw.githubusercontent.com/calarcond/machine_learning_model/main/archivos_trabajo/titanic.csv'
titanic = read_csv(link)


titanic[titanic == 'NA'] = NA

summary(titanic)


titanic_imputado_mean = titanic
titanic_imputado_dos = titanic
table(is.na(titanic_imputado_mean$Age))
table(is.na(titanic_imputado_mean$Cabin))


ggplot(data = titanic_imputado_mean, aes(x = Age)) + geom_histogram()

promedio_edad = mean(titanic_imputado_mean$Age, na.rm = TRUE)

titanic_imputado_mean = titanic_imputado_mean %>% mutate(Age = case_when(is.na(Age) ~ promedio_edad,
                                    TRUE ~ Age))

table(is.na(titanic_imputado_mean$Age))


ggplot(data = titanic_imputado_mean, aes(x = Age)) + geom_histogram()
mean(titanic_imputado_mean$Age)


library(imputeTS)
#https://www.rdocumentation.org/packages/imputeTS/versions/3.2

na_mean(titanic_imputado_dos)

na_locf(titanic_imputado_dos)

na_ma(titanic_imputado_dos)



install.packages(DMwR2)
library(DMwR2)

numericas = titanic_imputado_dos[,c(2,3,6,7,8,10)]
numericas
imputado = knnImputation(numericas,k=3)
titanic_imputado_dos$Age = imputado$Age


ggplot(data = titanic_imputado_dos, aes(x = Age)) + geom_histogram()
mean(titanic_imputado_dos$Age)
