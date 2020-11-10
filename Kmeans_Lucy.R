library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)
library(ggpubr)
library(heatmaply)
library(factoextra)
library(gvlma)
library(rpart.plot)
library(FactoMineR)
install.packages("FactoMineR")


df <- read.csv('full_set.csv')
df_train <- df[c(3:60)]
summary(df_train)
df_important <- read.csv('important.csv')
df_important_num <- df_important[c(3:13)]
df_kmeans <- scale(df_important_num)
df_kmeans2 <- scale(df_train)
#fviz_nbclust(df_kmeans, kmeans, method = "wss") +
  #geom_vline(xintercept = 3, linetype = 2)
#Error: cannot allocate vector of size 3.1 Gb

set.seed(123)
km.res <- kmeans(df_kmeans, 4, nstart = 25)
km.res2 <- kmeans(df_kmeans, 3, nstart = 50)
# Print the results
print(km.res)
print(km.res2)

km.res3 <- kmeans(df_kmeans2, 3, nstart = 50)
print(km.res2)

res.pca <- PCA(df_important_num, graph = FALSE)
print(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# trctrl <- trainControl(method = "cv", number = 5)
# set.seed(333)
# dtree_fit <- train(Market.Cap ~., data = df, method = "rpart",
#                    parms=list(split='information'),
#                    tuneLength = 10,
#                    trControl=trctrl)
# 
# summary(dtree_fit$finalModel)
