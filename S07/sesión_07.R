data("iris")
data = iris[,-5]
data
install.packages('ClusterR')
install.packages('cluster')

#### Modelo KMeans


modelo_kmeans = kmeans(data, centers = 5)
modelo_kmeans$size

library(ggplot2)

par(mfrow = c(1,2))
plot(data[c('Sepal.Length','Sepal.Width')], col = modelo_kmeans$cluster, main = 'Modelo KMeans')
plot(iris[c('Sepal.Length','Sepal.Width')], col = iris$Species, main = 'Real')

library('tidyverse')

modelo_kmeans$tot.withinss

iteracion = function(k){
  cluster = kmeans(data,k)
  return(cluster$tot.withinss)
}

max_k = 20
wss = sapply(2:max_k,iteracion)
codo = data.frame(2:max_k,wss)

codo
ggplot(codo, aes(x = c(2:max_k), y = wss)) + geom_point() + geom_line()


#### Hierarchical Clustering

data(mtcars)
mtcars
mtcars_esc = data.frame(scale(mtcars))


distancias = dist(mtcars_esc,method = 'euclidean')
modelo_clustering = hclust(distancias, method = 'average')
plot(modelo_clustering)

countries_filtrado = countries %>% filter(Region == 'LATIN AMER. & CARIB')
countries_filtrado

countries_esc = data.frame(scale(countries_filtrado[,c(7,3,5,9)]))


rownames(countries_esc) = countries_filtrado$Country
countries_esc$country = countries_filtrado$Country
countries_esc

distancias_c = dist(countries_esc, method = 'euclidean')
modelo = hclust(distancias_c, method = 'average')
plot(modelo)
View(countries_esc)

rownames(countries_esc) = countries_esc$country
countries_esc$country = rownames(countries_esc)
countries_esc

#### DBScan

install.packages('fpc')
library(fpc)

modelo_dbscan = dbscan(countries_esc, eps = 0.8, MinPts = 1)
pred = modelo_dbscan$cluster
table(modelo_dbscan$cluster, iris$Species)

plot(modelo_dbscan, countries_esc)
text(modelo_dbscan, countries_esc, labels = countries_esc$country)

modelo_dbscan$cluster

