library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(stats)

top_ten <- read.csv('important.csv')

ten_norm <- scale(subset(top_ten[,3:13]))

set.seed(123)

cluster_diana <- diana(ten_norm)

cluster_diana$dc

cut_diana <- cutree(cluster_diana, k=5)

table(cut_diana)
