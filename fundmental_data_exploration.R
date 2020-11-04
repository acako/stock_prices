library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)
library(factoextra)
#load in the first file
data_2014 <- read.csv('2014_Financial_Data.csv')
#summary(data_2014)
#str(data_2014)

#plot missing values
#missing_plot(data_2014)

#add a column for year
data_2014 <- data_2014 %>% mutate(year=2014)

#do the same for the other 4 data sets

#load in the first file
data_2015 <- read.csv('2015_Financial_Data.csv')
#summary(data_2015)
#str(data_2015)

#plot missing values
#missing_plot(data_2015)

#add a column for year
data_2015 <- data_2015 %>% mutate(year=2015)

#load in the first file
data_2016 <- read.csv('2016_Financial_Data.csv')
#summary(data_2016)
#str(data_2016)

#plot missing values
#missing_plot(data_2016)

#add a column for year
data_2016 <- data_2016 %>% mutate(year=2016)

#load in the first file
data_2017 <- read.csv('2017_Financial_Data.csv')
#summary(data_2017)
#str(data_2017)

#plot missing values
#missing_plot(data_2017)

#add a column for year
data_2017 <- data_2017 %>% mutate(year=2017)

#load in the first file
data_2018 <- read.csv('2018_Financial_Data.csv')
#summary(data_2018)
#str(data_2018)

#plot missing values
#missing_plot(data_2018)

#add a column for year
data_2018 <- data_2018 %>% mutate(year=2018)

# combine all 5 sets
#complete_data <- rbind(data_2014, data_2015, data_2016, data_2017, data_2018)
# names do not match, need to fix this...it is because of xyear.price.var

colnames(data_2014)[224] <- 'PRICE.VARR'
colnames(data_2015)[224] <- 'PRICE.VARR'
colnames(data_2016)[224] <- 'PRICE.VARR'
colnames(data_2017)[224] <- 'PRICE.VARR'
colnames(data_2018)[224] <- 'PRICE.VARR'

complete_data <- rbind(data_2014, data_2015, data_2016, data_2017, data_2018)

#only include fundamental columns
complete_data <- subset(complete_data[,
                      c(1:4,6:8,10,12:14,16,20,22,30,33,34,36,38,40:43,45:53,55,56,60:74,176,179:190,223:226)])
missing_plot(complete_data)
sort((sapply(complete_data, function(x) sum(is.na(x)))), decreasing=TRUE)
#looks like a lot of observations make up the majority of the missing data 
#what if we remove observations that have more than a third of the columns NA
complete_data_remove <- complete_data[which(rowMeans(!is.na(complete_data))>(1/3)),]
missing_plot(complete_data_remove)
sort((sapply(complete_data_remove, function(x) sum(is.na(x)))), decreasing=TRUE)

#that is a smaller number of NA's
#lets make the sector, class, and year columns factors
complete_data_remove$Sector <- as.factor(complete_data_remove$Sector)
complete_data_remove$Class <- as.factor(complete_data_remove$Class)
complete_data_remove$year <- as.factor(complete_data_remove$year)

#save the new data set as a csv
write.csv(complete_data_remove,"fundamental_data.csv")
pvq <- quantile(complete_data_remove$PRICE.VARR, probs = c(0.01,0.99), names=FALSE, na.rm=TRUE)
plot_data <- complete_data_remove
plot_data[plot_data==0] <- NA

# plot relationship between target variable and predictors
for (i in c(2:62)){
  q <- quantile(plot_data[[i]], probs = c(0.05, 0.95), names=FALSE, na.rm = TRUE)
  if (min(plot_data[[i]], na.rm=TRUE) >= 0) {
    lin_plot <- plot_data %>%
      ggplot(aes_string(x=plot_data[[i]], y=plot_data$PRICE.VARR)) +
      geom_point() + scale_x_continuous(trans='log2') + 
      ylim(pvq[1],pvq[2]) + xlim(q[1],q[2]) + 
      labs(title=as.list(names(plot_data[i])), x='', y='Price Change')
  } else {
    lin_plot <- plot_data %>%
      ggplot(aes_string(x=plot_data[[i]], y=plot_data$PRICE.VARR)) +
      geom_point() + ylim(pvq[1],pvq[2]) +  xlim(q[1],q[2]) + 
      labs(title=as.list(names(plot_data[i])), x='', y='Price Change') 
  }
  ggsave(paste(as.list(names(plot_data[i])),'plot.png',sep=''),plot = lin_plot)
}

#run linear regression for each predictor
ctrl <- trainControl(method='cv', number = 5)
linear_regressions <- c()
for (i in c(2:62)){
  data <- na.omit(subset(complete_data_remove[,c(i,64)]))
  linear_regressions[[i]] <- train(PRICE.VARR ~., data=data, method='lm', trControl=ctrl)
}

linear_R2 <- c()
for (i in c(2:62)){
  linear_R2[[i]] <- linear_regressions[[i]]$results$Rsquared
}

logistic_regressions <- c()
for (i in c(2:62)){
  data <- na.omit(subset(complete_data_remove[,c(i,64)]))
  logistic_regressions[[i]] <- train(PRICE.VARR ~., data=data, method='glm', trControl=ctrl)
}

log_R2 <- c()
for (i in c(2:62)){
  log_R2[[i]] <- logistic_regressions[[i]]$results$Rsquared
}


lin_log <- data.frame(lin=unlist(linear_R2), log=unlist(log_R2))

lin_log %>% ggplot(aes(x=lin, y=log)) + geom_point()

#imputed_data_cart <- mice(complete_data_remove, m=5, method='cart',maxit = 5)

#saveRDS(imputed_data_cart, 'cart_imputation.rds')
#complete_cart <- complete(imputed_data_cart, 1)
#missing_plot(complete_cart)
#sort((sapply(complete_cart, function(x) sum(is.na(x)))), decreasing=TRUE)

#write.csv(complete_cart, 'cart_imputation.csv')
#still 4 columns with missing values
#try rf
#imputed_data_rf <- mice(complete_data_remove, m=5, method='rf',maxit = 5)
