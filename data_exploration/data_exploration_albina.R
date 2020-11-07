#load dataset
data <- read.csv("exploration_data.csv")
#data summary
summary(data)

#multiple histogram plot for missing data
#load libraries
library(ggplot2)
library(reshape2)
#make plot
ggplot(melt(data),aes(x=value)) + geom_histogram() + facet_wrap(~variable)
#load dplyr
library(dplyr)
#load imputed dataset
data_imp <- read.csv("full_set.csv")
#data summary
summary(data_imp)
#remove data that we do not need for the model
data_clean <- select(data_imp, -c(X.1, X))
#load libraries to run decision tree
library(caret)
library(rpart)
library(xgboost)
library(tidyverse)
#new data summary
summary(data_clean)
#set seed
set.seed(100)
#run decision tree
dec_tree <- train(Market.Cap ~.,data=data_clean,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                             method='rpart')
#look at decision tree
summary(dec_tree)
print(dec_tree)
plot(dec_tree)
attributes(dec_tree)
#model accuracy
min(dec_tree$results$RMSE)
max(dec_tree$results$Rsquared)
max(dec_tree$resuls$MAE)
#save RDS
saveRDS(dec_tree, 'decisiontree_model_updated_albina.rds')
