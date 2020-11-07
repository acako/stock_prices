library(Hmisc)
library(ggcorrplot)
library(tidyverse)
library(VIF)
library(car)
library(caret)


df <- read.csv('fundamental_data.csv')
df_imputed <- read.csv('full_set.csv')

#df_complete <- read.csv('complete_data.csv')
#df_tickernames <- df_complete[, names(df_complete) %in% c('X', 'X.1')]
#colnames(df_tickernames) <- c('X', 'ticker')
#df <- merge(df_tickernames, df, by = 'X')

#looking at currency conversions
#attach(df_complete)
#df_sorted_rev <- df_complete[order(-Revenue),]
#df_sorted_rev <- df_sorted_rev[, names(df_sorted_rev) %in% c('X', 'Revenue', 'year')]
#df_sorted_mc <- df_complete[order(-Market.Cap),]
#df_sorted_mc <- df_sorted_mc[, names(df_sorted_mc) %in% c('X', 'Market.Cap', 'year')]

#looked at the top 200 or so rows:
#questionable companies and conversion rates: 
 #IGLD (Tel Aviv) - 2014 (3.9101 ILS), 2015 (3.8974 ILS), 2016 (3.8631 ILS), 2017 (3.8519 ), 2018 (3.7391 )
 #SBT (should technically be trading on NASDAQ...) - don't know, but should probably remove
 #KST (0.0615338 )
 #AMX
#Suggest removal instead of trying to convert currency

 

#predictor independence
predictor_cols <- names(df)[!names(df) %in% c('Class', 'X.1', 'X', 'Sector', 'ticker')]

#preliminary ggplot, very messy
cor <- rcorr(as.matrix(df[, names(df) %in% predictor_cols]))
p.mat <- cor_pmat(as.matrix(df[, names(df) %in% predictor_cols]))
ggcorrplot(cor$r, type = 'upper', p.mat = p.mat, sig.level = 0.05)

#filtering
cor_mat <- cor$r
cor_mat[!lower.tri(cor_mat)] <- NA # remove diagonal and redundant values

cor2 <- data.frame( cor_mat) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.8 & abs(correlation) < 1)
cor2

setdiff(cor2$rowname, cor2$variable)

mtx_cor2 <- spread(cor2, rowname, correlation)
rownames(mtx_cor2) <- mtx_cor2$variable
mtx_cor2$variable <- NULL
mtx_cor2 <- mtx_cor2 %>%
  mutate_all(~replace(., is.na(.), 0))

ggcorrplot(as.matrix(mtx_cor2), type = 'upper', sig.level = 0.05, lab = TRUE)

#TESTING
corrtable <- as.data.frame(as.table(cor_mat))
corrtable <- na.omit(corrtable) 
corrtable <- corrtable[corrtable$Freq > 0.8, ]
mtx_corrtable <- spread(corrtable, Var1, Freq)
mtx_corrtable[is.na(mtx_corrtable)] <- 0
rownames(mtx_corrtable) <- mtx_corrtable$Var2
mtx_corrtable$Var2 <- NULL
ggcorrplot(mtx_corrtable, type = 'upper', lab = TRUE)


#linear regression

library(caret)
set.seed(123)

sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ][, names(df) %in% predictor_cols]
test  <- df[-sample, ][, names(df) %in% predictor_cols]
na.rm=TRUE
model1 <- train(Market.Cap~., train, method = 'glm', na.action = na.pass)

independence <- data.frame(vif(model1$finalModel))
independence$rownames <- rownames(independence)
colinear <- independence[independence$vif.model1.>5,]
colinear
nrow(colinear)
#41 variables can be removed from the model


#Outliers
df_predictors <- df[, names(df) %in% predictor_cols]

par(mfrow=c(3,3))
for (i in predictor_cols){
  boxplot(df_predictors[[i]], main = paste(i), xlab = '', col= 4)
}

#outlier-log
df_log <- data.frame(lapply(df_predictors, log))
par(mfrow=c(3,3))
for (i in predictor_cols){
  boxplot(df_log[[i]], main = paste(i), xlab = '', col= 4)
}

library(rpart)
set.seed(123)
df_imputed <- df_imputed[, !names(df_imputed) %in% c('X', 'X.1')]
df_imputed$year <- as.factor(df_imputed$year)

#decision tree
cv = trainControl(method = 'cv', n = 5)
tree_mod <- train(Market.Cap~., df_imputed, method = 'rpart', na.action = na.pass, trControl = cv)

importance <- sort(tree_mod$finalModel$variable.importance)
barplot(importance, col = 4, horiz=TRUE, las=2, cex.names = 0.7)

tree_mod
importance

#linear regression
lm <- train(Market.Cap~., df_imputed, method = 'glm', na.action = na.pass, trControl = cv)
coeff <- sort(lm$finalModel$coefficients)
barplot(coeff, col = 4, horiz=TRUE, las=2, cex.names = 0.7)

lm_independence <- data.frame(vif(lm$finalModel))
lm_independence$rownames <- rownames(lm_independence)
lm_colinear <- lm_independence[lm_independence$vif.lm.finalModel.>5,]
lm_colinear
length(lm_colinear)

#random forest
rf <- train(Market.Cap~., df_imputed, method = 'rf', na.action = na.omit, trControl = cv)

rf_importance <- sort(rf$finalModel$importance)
barplot(rf_importance, col = 4, horiz=TRUE, las=2, cex.names = 0.7)

tree_mod
importance
