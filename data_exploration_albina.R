data <- read.csv("exploration_data.csv")
#data summary
summary(data)

#multiple histogram plot for missing data
library(ggplot2)
library(reshape2)
ggplot(melt(data),aes(x=value)) + geom_histogram() + facet_wrap(~variable)

#load imputed dataset
data_imp <- read.csv("cart_imputation.csv")

summary(data_imp)

#remove data with missing values
data_clean <-data_imp - c[24, 30, 31, 33])
model_final_price_1 <- train(final_price ~.,data=complete_d_1,trControl = trainControl("cv",number=10, savePredictions = 'all'),
                             method='xgbTree', tuneGrid = grid)

  

