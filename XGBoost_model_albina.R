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

