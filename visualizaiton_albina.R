data <- read.csv("exploration_data.csv")
summary(data)

#multiple histogram plot
library(ggplot2)
library(reshape2)
ggplot(melt(data),aes(x=value)) + geom_histogram() + facet_wrap(~variable)


