library(dplyr)
library(cluster)
library(factoextra)
library(dendextend)
library(ggplot2)
library(FactoMineR)
library(NbClust)

#loading dataframes and stock names
df <- read.csv('important.csv')
df_imputed <- read.csv('full_set.csv')
nasdaq <- read.csv('nasdaq.csv')
nasdaq$Ticker <- toupper(nasdaq$Ticker)
names(nasdaq) <- c('ticker','name')
NYSE <- read.delim("NYSE.txt")
NYSE <- data.frame(NYSE)
names(NYSE) <- c('ticker','name')
stocks_lookup_table <- rbind(NYSE,nasdaq)

##let's start with PCA
df_PCA <- df
df_PCA$year <- df_imputed$year
df_PCA$uniqueticker <- paste(df_PCA$X, df_PCA$year)
rownames(df_PCA) <- df_PCA$uniqueticker
df_PCA <- df_PCA[, !names(df_PCA) %in% c('X','X.1', 'Market.Cap', 'year','uniqueticker','Sector')]
res.pca <- PCA(df_PCA)

#visualize percentage of explained variance from each dimension. first 5 explain ~85%
fviz_eig(res.pca)

#color visualization for individual companies
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#color visualization for variables
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             # Avoid text overlapping
)
#biplot of both variables and individual points
fviz_pca_biplot(res.pca,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

##cluster using top 5 PCA columns which explain 85% var
clustering_PCA <- data.frame(res.pca$ind$coord)
write.csv(clustering_PCA, 'PCA_df.csv')

# Now K means clustering
#fviz_nbclust(clustering_PCA, kmeans, method = "wss") + labs(subtitle = "Elbow method")

#2 clusters
# set.seed(123)
# clustering_PCA <- data.frame(res.pca$ind$coord)
# km.res <- kmeans(clustering_PCA, 2, nstart = 25)
# fviz_cluster(km.res, clustering_PCA, ellipse.type = "norm")
# 
# #3 cluster
# clustering_PCA <- data.frame(res.pca$ind$coord)
# km.res <- kmeans(clustering_PCA, 3, nstart = 25)
# fviz_cluster(km.res, clustering_PCA, ellipse.type = "norm")

#4 cluster
clustering_PCA <- data.frame(res.pca$ind$coord)
km.res <- kmeans(clustering_PCA, 4, nstart = 25)
fviz_cluster(km.res, clustering_PCA, ellipse.type = "norm")

#analysis of clusters
clustering_PCA$cluster <- as.factor(km.res$cluster)
clustering_PCA$Market.Cap <- df_imputed$Market.Cap
clustering_PCA$ticker <- df_imputed$X
df_imputed$cluster <- as.factor(km.res$cluster)
table(clustering_PCA$cluster)


cluster1 <- rownames(clustering_PCA[clustering_PCA$cluster==1,])
cluster2 <- rownames(clustering_PCA[clustering_PCA$cluster==2,])
cluster3 <- rownames(clustering_PCA[clustering_PCA$cluster==3,])
cluster4 <- rownames(clustering_PCA[clustering_PCA$cluster==4,])
cluster1 <- unique(substr(cluster1, 1, nchar(cluster1)-5))
cluster2 <- unique(substr(cluster2, 1, nchar(cluster2)-5))
cluster3 <- unique(substr(cluster3, 1, nchar(cluster3)-5))
cluster4 <- unique(substr(cluster4, 1, nchar(cluster4)-5))


intersect(cluster1, cluster2)
intersect(cluster1, cluster3)
intersect(cluster1, cluster4)
intersect(cluster2, cluster3)
intersect(cluster2, cluster4)
intersect(cluster3, cluster4)

c1df <- data.frame(cluster1)
names(c1df) = c('ticker')
c2df <- data.frame(cluster2)
names(c2df) = c('ticker')
c3df <- data.frame(cluster3)
names(c3df) = c('ticker')
c4df <- data.frame(cluster4)
names(c4df) = c('ticker')

c1df <- left_join(c1df, stocks_lookup_table, by = 'ticker')
c2df <- left_join(c2df, stocks_lookup_table, by = 'ticker') # this cluster contains the big tech stocks
c3df <- left_join(c3df, stocks_lookup_table, by = 'ticker') # this cluster contains big banks + oil
c4df <- left_join(c4df, stocks_lookup_table, by = 'ticker') # this cluster contains big pharma and consumer goods

df$cluster <- km.res$cluster
rownames(df) <- rownames(df_PCA)
df$Sector <- df_imputed$Sector
df <- df[df$X.1!=373,]
df <- df[df$X.1!=4027,]
df <- df[df$X.1!=12158,]

write.csv(df, 'important_with_clusters.csv')



# JUNK CODE BELOW

# #cluster1: Walmart, Toyota, General Electric, AT&T, Verizon, China Mobile, Exxon Mobil, Chevron, 
# 
# cluster1$company <- data.frame(merge(cluster1, NYSE, by.x = ))
# 
# 
# # K means clustering with only one year
# df_2018 <- df_imputed[df_imputed$year == 2018, ]
# df_2018 <- df_2018[, names(df_2018) %in% names(df)]
# rownames(df_2018) <- df_2018$X
# df_2018_clean <- df_2018[, !names(df_2018) %in% c('X.1','X', 'Market.Cap')]
# fviz_nbclust(df_2018_clean, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
# 
# km.res <- kmeans(df_2018_clean, 4, nstart = 25)
# fviz_cluster(km.res, df_2018_clean, ellipse.type = "norm")
# df_2018_clean$cluster <- km.res$cluster
# setdiff(rownames(df_2018_clean[df_2018_clean$cluster == 2, ]), toupper(nasdaq$Ticker))
# length(setdiff(toupper(nasdaq$Ticker), rownames(df_2018_clean[df_2018_clean$cluster == 3, ])))
# setdiff(toupper(nasdaq$Ticker), rownames(df_2018_clean[df_2018_clean$cluster == 3, ]))

# ####################
# df_random <- df[sample(nrow(df), 100),]
# df_random <- df_random[, !names(df_random) %in% c('X','X.1', 'Market.Cap', 'year','uniqueticker')]
# df_random <- data.frame(scale(df_random)) 
# d <- dist(df_random, method = 'euclidean')
# 
# 
# hc2 <- hclust(d, method = 'complete')
# plot(hc2, hang = -1)
# hc2$order
# df_random$cluster <- cutree (hc2, k = 4)
# rect.hclust(hc2, k = 4, border = 2:5)
# 
# df$year <- df_imputed$year
# df$uniqueticker <- paste(df$X, df$year)
# rownames(df) <- df$uniqueticker
# df <- df[, !names(df) %in% c('X','X.1', 'Market.Cap', 'year','uniqueticker')]
# df <- data.frame(scale(df)) 
# d <- dist(df, method = 'euclidean')
# df$Market.Cap <- df_imputed$Market.Cap
# 
# hc1 <- hclust(d, method = 'complete')
# plot(hc1, hang = -1)
# hc1$order
# df$cluster <- as.factor(cutree (hc1, k = 12))
# table(df$cluster)
# 
# ggplot(df, aes(x=log(Market.Cap, color = cluster))) + geom_density()
# 
# d2 <- dist(clustering_PCA, method = 'euclidean')
# 
# hc2 <- hclust(d2, method = 'complete')
# plot(hc2, hang = -1)
# hc2$order
# clustering_PCA$cluster <- as.factor(cutree (hc2, k = 6))
# table(clustering_PCA$cluster)
