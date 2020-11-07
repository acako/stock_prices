data <- read.csv("exploration_data.csv")
#data summary
summary(data)

#multiple histogram plot for missing data
library(ggplot2)
library(reshape2)
ggplot(melt(data),aes(x=value)) + geom_histogram() + facet_wrap(~variable)
library(dplyr)
#load imputed dataset
data_imp <- read.csv("full_set.csv")

summary(data_imp)

#remove data with missing values
data_clean <- select(data_imp, -c(X.1, X))
#load libraries
library(caret)
library(rpart)
library(xgboost)
library(tidyverse)
#set seed
summary(data_clean)
set.seed(100)
#run decision tree
dec_tree <- train(Market.Cap ~.,data=data_clean,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                             method='rpart')

summary(dec_tree)
print(dec_tree)
plot(dec_tree)
attributes(dec_tree)

#model accuracy
min(dec_tree$results$RMSE)
max(dec_tree$results$Rsquared)
max(dec_tree$resuls$MAE)
library(rpart.plot)
#rpart.plot(dec_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#rpart.plot(fit, extra= 106)

saveRDS(dec_tree, 'decisiontree_model_updated_albina.rds')
tmp <- rownames(object$splits)

summary.rpart(dec_tree)