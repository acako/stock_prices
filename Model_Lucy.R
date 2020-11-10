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

df <- read.csv("important_with_clusters_updated.csv")

#set sector and cluster as factors. 
df$cluster <- as.factor(df$cluster)
df$Sector <- as.factor(df$Sector)

df_trained <- df[c(5:17)]

#Stochastic Gradient Boosting
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
nrow(hyper_grid)
gbmFit1 <- train(Market.Cap ~ ., data = df_trained, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit2 <- train(Market.Cap ~ ., data = df_trained, 
                 method = "gbm", 
                 trControl = fitControl,
                 tuneGrid = gbmGrid,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

print(gbmFit1)
summary(gbmFit1)
print(gbmFit2)#The final values used for the model were n.trees = 600, interaction.depth =
#9, shrinkage = 0.1 and n.minobsinnode = 20.


lassoFit <- train(Market.Cap ~ .,
                  data = df_trained,
                  method = "lasso",  # now we're using the lasso method
                  trControl = fitControl,
                  preProcess = c('scale', 'center'))  
print(lassoFit)


