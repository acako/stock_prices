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

  

