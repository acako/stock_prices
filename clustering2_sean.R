library(dplyr)
library(cluster)
library(factoextra)
library(dendextend)
library(ggplot2)
library(FactoMineR)

df <- read.csv('important.csv')
df_imputed <- read.csv('full_set.csv')

df$year <- df_imputed$year
df$uniqueticker <- paste(df$X, df$year)
rownames(df) <- df$uniqueticker
df <- df[, !names(df) %in% c('X','X.1', 'Market.Cap', 'year','uniqueticker')]
df <- data.frame(scale(df)) 
d <- dist(df, method = 'euclidean')
df$Market.Cap <- df_imputed$Market.Cap

hc1 <- hclust(d, method = 'complete')
plot(hc1, hang = -1)
hc1$order
df$cluster <- as.factor(cutree (hc1, k = 12))
table(df$cluster)

ggplot(df, aes(x=log(Market.Cap, color = cluster))) + geom_density()


##let's try with PCA
df_PCA <- read.csv('important.csv')
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

##cluster using top 5 PCA columns
clustering_PCA <- data.frame(res.pca$ind$coord)
write.csv(clustering_PCA, 'PCA_df.csv')
d2 <- dist(clustering_PCA, method = 'euclidean')
clustering_PCA$Market.Cap <- df_imputed$Market.Cap

hc2 <- hclust(d2, method = 'complete')
plot(hc2, hang = -1)
hc2$order
clustering_PCA$cluster <- as.factor(cutree (hc2, k = 200))
table(clustering_PCA$cluster)

####################
df_random <- df[sample(nrow(df), 100),]
df_random <- df_random[, !names(df_random) %in% c('X','X.1', 'Market.Cap', 'year','uniqueticker')]
df_random <- data.frame(scale(df_random)) 
d <- dist(df_random, method = 'euclidean')


hc2 <- hclust(d, method = 'complete')
plot(hc2, hang = -1)
hc2$order
df_random$cluster <- cutree (hc2, k = 4)
rect.hclust(hc2, k = 4, border = 2:5)