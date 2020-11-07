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
                      c(1:4,6:8,10,12:14,16,20,22,30,33,34,36,38,40:43,45:53,55,56,60:74,142,176,179:190,223,226)])
complete_data <- complete_data[complete_data$X != 'IGLD', ]
complete_data <- complete_data[complete_data$X != 'SBT', ]
complete_data <- complete_data[complete_data$X != 'KST', ]
complete_data <- complete_data[complete_data$X != 'AMX', ]


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
complete_data_remove$year <- as.factor(complete_data_remove$year)

#save the new data set as a csv
#write.csv(complete_data_remove,"fundamental_data.csv")
pvq <- quantile(complete_data_remove$Market.Cap, probs = c(0.01,0.99), names=FALSE, na.rm=TRUE)
plot_data <- complete_data_remove
plot_data[plot_data==0] <- NA

# plot relationship between target variable and predictors
#for (i in c(2:62)){
  #q <- quantile(plot_data[[i]], probs = c(0.05, 0.95), names=FALSE, na.rm = TRUE)
  #if (min(plot_data[[i]], na.rm=TRUE) >= 0) {
  #  lin_plot <- plot_data %>%
  #    ggplot(aes_string(x=plot_data[[i]], y=plot_data$Market.Cap)) +
  #    geom_point() + scale_x_continuous(trans='log2') + 
  #   ylim(pvq[1],pvq[2]) + xlim(q[1],q[2]) + 
  #    labs(title=as.list(names(plot_data[i])), x='', y='Price Change')
  #} else {
  #lin_plot <- plot_data %>%
  #    ggplot(aes_string(x=plot_data[[i]], y=plot_data$Market.Cap)) +
  #    geom_point() + ylim(pvq[1],pvq[2]) +  xlim(q[1],q[2]) + 
  #    labs(title=as.list(names(plot_data[i])), x='', y='Market Cap')
  #}
  #ggsave(paste(as.list(names(plot_data[i])),'plot.png',sep=''),plot = lin_plot)
#}

#run linear regression for each predictor
ctrl <- trainControl(method='cv', number = 5)
linear_regressions <- c()
for (i in c(2:62)){
  data <- na.omit(subset(complete_data_remove[,c(i,50)]))
  linear_regressions[[i]] <- train(Market.Cap ~., data=data, method='lm', trControl=ctrl)
}

linear_R2 <- c()
for (i in c(2:62)){
  linear_R2[[i]] <- linear_regressions[[i]]$results$Rsquared
}

logistic_regressions <- c()
for (i in c(2:62)){
  data <- na.omit(subset(complete_data_remove[,c(i,50)]))
  logistic_regressions[[i]] <- train(Market.Cap ~., data=data, method='glm', trControl=ctrl)
}

log_R2 <- c()
for (i in c(2:62)){
  log_R2[[i]] <- logistic_regressions[[i]]$results$Rsquared
}

columns <- names(subset(complete_data_remove[,c(2:62)]))

lin_log <- data.frame(predictors=unlist(columns), lin=unlist(linear_R2), log=unlist(log_R2))

lin_log %>% ggplot(aes(x=lin, y=log)) + geom_point()

###Time to look at missingness fully

data <- complete_data
sort((sapply(data, function(x) sum(is.na(x)))), decreasing=TRUE)
missingness <- data.frame(name = character(),
                          percentage = numeric(),
                          min_diff = numeric(),
                          q1_diff = numeric(),
                          median_diff = numeric(),
                          mean_diff = numeric(),
                          q3_diff = numeric(),
                          max_diff = numeric())
for (i in c(2:62)) {
  new_data <- subset(data[,c(i,64)])
  complete_data <- na.omit(new_data)
  missing_data <- new_data[rowSums(is.na(new_data)) > 0,]
  miss_pctg <- length(missing_data$PRICE.VARR)/(length(complete_data$PRICE.VARR) + length(missing_data$PRICE.VARR))
  sum_comp <- unname(summary(complete_data$PRICE.VARR))
  sum_miss <- unname(summary(missing_data$PRICE.VARR))
  min_diff <- (sum_miss[1]-sum_comp[1])/sum_comp[1]
  q1_diff <- (sum_miss[2]-sum_comp[2])/sum_comp[2]
  median_diff <- (sum_miss[3]-sum_comp[3])/sum_comp[3]
  mean_diff <- (sum_miss[4]-sum_comp[4])/sum_comp[4]
  q3_diff <- (sum_miss[5]-sum_comp[5])/sum_comp[5]
  max_diff <- (sum_miss[6]-sum_comp[6])/sum_comp[6]
  missingness[i-1,] = c(names(complete_data)[1],
                             miss_pctg,
                             min_diff,
                             q1_diff,
                             median_diff,
                             mean_diff,
                             q3_diff,
                             max_diff)
}

missingness[2:8] <- lapply(missingness[2:8], as.numeric)

for (i in c(3:8)){
  print(missingness %>%
          ggplot(aes(y=percentage, x=missingness[[i]])) +
          geom_point() +
          labs(x=as.list(names(missingness)[i])))
}

missingness_2 <- data.frame(name = character(),
                          percentage = numeric(),
                          min_comp = numeric(),
                          min_miss = numeric(),
                          q1_comp = numeric(),
                          q1_miss = numeric(),
                          median_comp = numeric(),
                          median_miss = numeric(),
                          mean_comp = numeric(),
                          mean_miss = numeric(),
                          q3_comp = numeric(),
                          q3_miss = numeric(),
                          max_comp = numeric(),
                          max_miss = numeric())

for (i in c(2:62)) {
  new_data <- subset(data[,c(i,64)])
  complete_data <- na.omit(new_data)
  missing_data <- new_data[rowSums(is.na(new_data)) > 0,]
  miss_pctg <- length(missing_data$PRICE.VARR)/(length(complete_data$PRICE.VARR) + length(missing_data$PRICE.VARR))
  sum_comp <- unname(summary(complete_data$PRICE.VARR))
  sum_miss <- unname(summary(missing_data$PRICE.VARR))
  min_comp <- sum_comp[1]
  min_miss <- sum_miss[1]
  q1_comp <- sum_comp[2]
  q1_miss <- sum_miss[2]
  median_comp <- sum_comp[3]
  median_miss <- sum_miss[3]
  mean_comp <- sum_comp[4]
  mean_miss <- sum_miss[4]
  q3_comp <- sum_comp[5]
  q3_miss <- sum_miss[5]
  max_comp <- sum_comp[6]
  max_miss <- sum_miss[6]
  missingness_2[i-1,] = c(names(complete_data)[1],
                        miss_pctg,
                        min_comp,
                        min_miss,
                        q1_comp,
                        q1_miss,
                        median_comp,
                        median_miss,
                        mean_comp,
                        mean_miss,
                        q3_comp,
                        q3_miss,
                        max_comp,
                        max_miss)
}

missingness_2[2:14] <- lapply(missingness_2[2:14], as.numeric)

missingness_2 %>% ggplot(aes(x=min_comp, y=min_miss)) + geom_point()
missingness_2 %>% ggplot(aes(x=q1_comp, y=q1_miss)) + geom_point()
missingness_2 %>% ggplot(aes(x=median_comp, y=median_miss)) + geom_point()
missingness_2 %>% ggplot(aes(x=mean_comp, y=mean_miss)) + geom_point()
missingness_2 %>% ggplot(aes(x=q3_comp, y=q3_miss)) + geom_point()
missingness_2 %>% ggplot(aes(x=max_comp, y=max_miss)) + geom_point()


imputed_data_cart <- mice(complete_data_remove, m=5, method='cart',maxit = 5)

saveRDS(imputed_data_cart, 'cart_imputation.rds')
complete_cart <- complete(imputed_data_cart, 1)
#missing_plot(complete_cart)
sort((sapply(complete_cart, function(x) sum(is.na(x)))), decreasing=TRUE)
#remove total.non.current.liabilities. and total.non.current.assets

complete_cart <- subset(complete_cart[,c(1:22,24:28,31,33:65)])

complete_cart$year <- as.factor(complete_cart$year)

sort((sapply(final_set, function(x) sum(is.na(x)))), decreasing=TRUE)
write.csv(complete_cart, 'full_set.csv')
#still 4 columns with missing values


### csv for clustering
full_set <- read.csv("full_set.csv")

full_set <- subset(full_set[,c(2,16,43,35,10,31,36,8,6,28,27,47)])

write.csv(full_set,'important.csv')
