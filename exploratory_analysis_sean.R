library(Hmisc)
library(ggcorrplot)
library(tidyverse)
library(VIF)

df <- read.csv('exploration_data.csv')


#predictor independence
predictor_cols <- names(df)[!names(df) %in% c('PRICE.VARR', 'Class', 'X.1', 'X', 'Sector')]

#preliminary ggplot, very messy
cor <- rcorr(as.matrix(df[, names(df) %in% predictor_cols]))
p.mat <- cor_pmat(as.matrix(df[, names(df) %in% predictor_cols]))
ggcorrplot(cor$r, type = 'upper', p.mat = p.mat, sig.level = 0.05)


ggcorrplot(cor, type='upper', p.mat = p.mat, sig.level = 0.05)


#linear regression
library(caret)

model_cols <- names(df)[!names(df) %in% c('Class', 'X.1', 'X', 'Sector')]
train_df <- df[, names(df) %in% model_cols]
model1 <- lm(PRICE.VARR~., train_df)

library(car)
independence <- data.frame(vif(model1))
independence$rownames <- rownames(independence)
colinear <- independence[independence$vif.model1.>5,]
colinear
