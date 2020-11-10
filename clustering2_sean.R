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

# df_imputed <- df_imputed[df_imputed$X.1!=373,]
# df_imputed <- df_imputed[df_imputed$X.1!=4027,]
# df_imputed <- df_imputed[df_imputed$X.1!=12158,]

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
#write.csv(clustering_PCA, 'PCA_df.csv')

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
set.seed(123)
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
df$Market.Cap <- df_imputed$Market.Cap
df <- df[df$X.1!=373,]
df <- df[df$X.1!=4027,]
df <- df[df$X.1!=12158,]
df$cluster <- as.factor(df$cluster)

#write.csv(df, 'important_with_clusters.csv')


#analyzing clusters

df_updated <- read.csv('important_with_clusters_updated.csv')
#df_updated$Short.term.debt <- df_imputed$Short.term.debt
df_updated$cluster <- as.factor(df_updated$cluster)
rownames(df_updated) <- df_updated$X.2
df_updated <- df_updated[df_updated$X.1!=373,]
df_updated <- df_updated[df_updated$X.1!=4027,]
df_updated <- df_updated[df_updated$X.1!=12158,]
df_updated <- df_updated[df_updated$Market.Cap!= 0,]

km.res$cluster <- df_updated$cluster
fviz_cluster(km.res, clustering_PCA, ellipse.type = "norm")

df_updated$PPR <- (df_updated$R.D.Expenses/df_updated$Market.Cap)
df_updated$DPR <- abs(df_updated$Dividend.payments/df_updated$Market.Cap)
df_updated$DTI <- df_updated$Total.debt/df_updated$Consolidated.Income


df_c1 <- subset(df_updated, df_updated$cluster==1) #everything else
df_c2 <- subset(df_updated, df_updated$cluster==2) #Big tech, 'FANG'
df_c3 <- subset(df_updated, df_updated$cluster==3) #big banks + oil
df_c4 <- subset(df_updated, df_updated$cluster==4) #big pharma, consumer goods?

#Clusters by Market Cap (value of the company) - note cluster 1 is clearly lower market cap compared to others
ggplot(df_updated, aes(x = log(Market.Cap), color=cluster)) + geom_density() + labs(title = 'Clusters by log(Market cap)')
#Clusters by sector
ggplot(df_updated, aes(x=cluster, fill = Sector)) + geom_bar(position = 'fill')
# look into clusters by market cap AND

#Clusters by dividend payout: dividend payout higher in cluster 3 compared to others. cluster 1 mostly probably smaller companies with lower dividend
ggplot(df_updated[df_updated$DPR<0.1,], aes(x = DPR, color=cluster)) + geom_density() + labs(title = 'Clusters by Dividend payout, normalized by Market Cap')
ggplot(df_updated, aes(x=Market.Cap, y=abs(Dividend.payments), color = cluster)) + geom_point() + labs(title='Market Cap vs Dividend by Cluster') 
#Cluster 3
ggplot(df_c3, aes(x=Market.Cap, y=abs(Dividend.payments), color = Sector)) + geom_point()  + labs(title = 'Cluster 3 Dividend Payout by Sector')
ggplot(df_c3[df_c3$DPR<0.2,], aes(x = DPR, color=Sector)) + geom_density() + labs(title = 'Clusters by Dividend payout, normalized by Market Cap')


#Clusters by Research and Development
ggplot(df_updated[df_updated$PPR != 0 & df_updated$PPR<0.4,], aes(x = PPR, color=cluster)) + geom_density() + labs(title = 'Research to Price Ratio by cluster')

#Cluster 2 and cluster 4 have higher R and D compared to average
ggplot(df_updated, aes(x=Market.Cap, y=R.D.Expenses, color = cluster)) + geom_point() + labs(title = 'Higher market cap correlates with more R and D')

#tech and big pharma on average have higher R and D
ggplot(df_updated, aes(x=Market.Cap, y=R.D.Expenses, color = Sector)) + geom_point() + labs(title = 'R and D expenses by Sector')
#Cluster 2
ggplot(df_c2, aes(x=Market.Cap, y=R.D.Expenses, color = Sector)) + geom_point() + geom_text(label=df_c2$X.2) 
#Cluster 3
ggplot(df_c4, aes(x = PPR, color=Sector)) + geom_density() + labs(title = 'Research to Price Ratio in Cluster 3 by Sector')
ggplot(df_c4[df_c4$PPR != 0 & df_updated$PPR< 0.4,], aes(x = PPR, color=Sector)) + geom_density() + labs(title = 'Research to Price Ratio in Cluster 3 by Sector')
#Cluster 4
ggplot(df_c4[df_c4$PPR != 0 & df_updated$PPR< 0.4,], aes(x = PPR, color=Sector)) + geom_density() + labs(title = 'Research to Price Ratio in Cluster 4 by Sector')

# big pharma companies 

#debt to income ratio
ggplot(df_updated[df_updated$DTI > -25 & df_updated$DTI < 25,], aes(x = abs(DTI), color=cluster)) + geom_density() + labs(title = 'Clusters by Debt to Income Ratio')



