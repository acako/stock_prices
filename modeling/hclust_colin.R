library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(stats)

top_ten <- read.csv('important.csv')

ten_norm <- scale(subset(top_ten[,3:13]))

set.seed(123)

dist_mat <- dist(ten_norm, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k=50)
ten_norm_avg_clust <- top_ten %>% mutate(cluster=cut_avg)
count(ten_norm_avg_clust, cluster)

#not good, try a different method...complete

hclust_comp <- hclust(dist_mat, method = 'complete')
plot(hclust_comp)
cut_comp <- cutree(hclust_comp, k=50)
ten_norm_comp_clust <- top_ten %>% mutate(cluster=cut_comp)
count(ten_norm_comp_clust, cluster)

#median

hclust_med <- hclust(dist_mat, method = 'median')
plot(hclust_med)
cut_med <- cutree(hclust_med, k=50)
ten_norm_med_clust <- top_ten %>% mutate(cluster=cut_med)
count(ten_norm_med_clust, cluster)

#single

hclust_single <- hclust(dist_mat, method = 'single')
plot(hclust_single)
cut_single <- cutree(hclust_single, k=50)
ten_norm_single_clust <- top_ten %>% mutate(cluster=cut_single)
count(ten_norm_single_clust, cluster)

#centroid

hclust_cent <- hclust(dist_mat, method = 'centroid')
plot(hclust_cent)
cut_cent <- cutree(hclust_cent, k=50)
ten_norm_cent_clust <- top_ten %>% mutate(cluster=cut_cent)
count(ten_norm_cent_clust, cluster)


#try it on the full set

full_set <- read.csv('full_set.csv')

full_norm <- scale(subset(full_set[,3:60]))
dist_mat_full <- dist(full_norm, method = 'euclidean')

hclust_avg_full <- hclust(dist_mat_full, method = 'average')
plot(hclust_avg_full)
cut_avg_full <- cutree(hclust_avg_full, k=50)
full_norm_avg_clust <- top_ten %>% mutate(cluster=cut_avg_full)
count(full_norm_avg_clust, cluster)
