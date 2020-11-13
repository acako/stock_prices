data <- read.csv("important_with_clusters.csv")
library(dplyr)
data$cluster <- as.factor(data$cluster)
data$Sector <- as.factor(data$Sector)

str(data)

final_data <- na.omit(data)

write.csv(final_data, "important_with_clusters_updated.csv")