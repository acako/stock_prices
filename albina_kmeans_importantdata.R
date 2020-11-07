#load dataset
data <- read.csv("important.csv")
#data summary
summary(data)
#kmeans(x, centers, iter.max = 10, nstart = 1)
#install.packages("factoextra")
library(factoextra)
library(cluster)
library(dplyr)
df_imputed <- read.csv('full_set.csv')
data$year <- df_imputed$year
data$uniqueticker <- paste(data$X, data$year)
rownames(data) <- data$uniqueticker
data_clean <- select(data, -c(X.1, X, Market.Cap, year, uniqueticker))


#Silhouette Method for finding the optimal number of clusters
#install.packages("NbClust")
library(NbClust)
fviz_nbclust(data_clean, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
#data_scaled <- scale(data_clean)
#fviz_nbclust(data_scaled, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
#selected top 2: 2, 3 and 4 clusters

#2 clusters
set.seed(123)
km.res <- kmeans(data_clean, 2, nstart = 25)
print(km.res)
km.res$cluster
head(km.res$cluster, 2)
fviz_cluster(km.res, data_clean, ellipse.type = "norm")

#4 clusters
set.seed(123)
km.res <- kmeans(data_clean, 4, nstart = 50)
print(km.res)
km.res$cluster
head(km.res$cluster, 4)
fviz_cluster(km.res, data_clean, ellipse.type = "norm")

#3 clusters
set.seed(123)
km.res <- kmeans(data_clean, 3, nstart = 50)
print(km.res)
km.res$cluster
head(km.res$cluster, 3)
fviz_cluster(km.res, data_clean, ellipse.type = "norm")