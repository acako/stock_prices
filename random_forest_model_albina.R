data <- read.csv("important_with_clusters_updated.csv")
head(data)
data_clean <- select(data, -c(X.3, X.2, X.1, X))
data_clean <- na.omit(data_clean)
#load libraries
library(caret)
library(rpart)
library(randomForest)
library(tidyverse)
summary(data_clean)
str(data_clean)
#set sector and cluster as factors. cluster data is not meant to be numeric
data_clean$cluster <- as.factor(data_clean$cluster)
data_clean$Sector <- as.factor(data_clean$Sector)
summary(data_clean)
str(data_clean)
#run model untuned
set.seed(123)

grid <- expand.grid(mtry = c(2,5), max_depth = c(10, 40, 100), min_samples_split = c(2, 5, 10), n_estimators = c(200, 800, 2000))
random_forest_model <- train(Market.Cap ~.,data=data_clean,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                   method='rf', tuneGrid = grid)
summary(random_forest_model)
print(random_forest_model)
plot(random_forest_model)
attributes(random_forest_model)
min(random_forest_model$results$RMSE)
max(random_forest_model$results$Rsquared)
min(random_forest_model$resuls$MAE)