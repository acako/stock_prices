#load dataset
data <- read.csv("important_with_clusters.csv")
head(data)
#remove columns that are not features
library(dplyr)
data_clean <- select(data, -c(X.2, X.1, X))
head(data_clean)
#set sector and cluster as factors. cluster data is not meant to be numeric
data_clean$cluster <- as.factor(data_clean$cluster)
data_clean$Sector <- as.factor(data_clean$Sector)
#let's take a look at classes
str(data_clean)

#load libraries
library(caret)
library(rpart)
library(xgboost)
library(tidyverse)
set.seed(123)
#remove NA values 
#check for NA values
#which(is.na(data_clean))
#remove all NA values and check final data
final_data <- na.omit(data_clean)
summary(final_data)
grid <- expand.grid(nrounds = c(100, 200), eta = c(0.1, 0.3), max_depth = c(3, 6), gamma = 0, colsample_bytree = c(0.5, 0.8), min_child_weight = c(1,5), subsample = 0.8)
XGB_model <- train(Market.Cap ~.,data=final_data,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                             method='xgbTree', tuneGrid = grid)

#model statistics
summary(XGB_model)
print(XGB_model)
plot(XGB_model)
attributes(XGB_model)

#RMSE:11099749916
#MAE:-Inf
#Rsquared: 0.8939831

#model accuracy
min(XGB_model$results$RMSE)
max(XGB_model$results$Rsquared)
min(XGB_model$resuls$MAE)

saveRDS(XGB_model, 'XGB_model_albina_updated.rds')
