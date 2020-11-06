data <- read.csv("exploration_data.csv")
#data summary
summary(data)

#multiple histogram plot for missing data
library(ggplot2)
library(reshape2)
ggplot(melt(data),aes(x=value)) + geom_histogram() + facet_wrap(~variable)
library(dplyr)
#load imputed dataset
data_imp <- read.csv("cart_imputation.csv")

summary(data_imp)
library(cart)

#remove data with missing values
data_clean <- select(data_imp, -c(Total.non.current.assets, Deferred.revenue, Tax.Liabilities, Total.non.current.liabilities))
dec_tree <- train(Market.Cap ~.,data=data_clean,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                             method='rPart')

  

