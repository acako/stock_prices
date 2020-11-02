library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)

#load in the first file
data_2014 <- read.csv('2014_Financial_Data.csv')
summary(data_2014)
str(data_2014)

#plot missing values
missing_plot(data_2014)

#add a column for year
data_2014 <- data_2014 %>% mutate(year=2014)

#do the same for the other 4 data sets

#load in the first file
data_2015 <- read.csv('2015_Financial_Data.csv')
summary(data_2015)
str(data_2015)

#plot missing values
missing_plot(data_2015)

#add a column for year
data_2015 <- data_2015 %>% mutate(year=2015)

#load in the first file
data_2016 <- read.csv('2016_Financial_Data.csv')
summary(data_2016)
str(data_2016)

#plot missing values
missing_plot(data_2016)

#add a column for year
data_2016 <- data_2016 %>% mutate(year=2016)

#load in the first file
data_2017 <- read.csv('2017_Financial_Data.csv')
summary(data_2017)
str(data_2017)

#plot missing values
missing_plot(data_2017)

#add a column for year
data_2017 <- data_2017 %>% mutate(year=2017)

#load in the first file
data_2018 <- read.csv('2018_Financial_Data.csv')
summary(data_2018)
str(data_2018)

#plot missing values
missing_plot(data_2018)

#add a column for year
data_2018 <- data_2018 %>% mutate(year=2018)

# combine all 5 sets
complete_data <- rbind(data_2014, data_2015, data_2016, data_2017, data_2018)
# names do not match, need to fix this...it is because of xyear.price.var

colnames(data_2014)[224] <- 'PRICE.VARR'
colnames(data_2015)[224] <- 'PRICE.VARR'
colnames(data_2016)[224] <- 'PRICE.VARR'
colnames(data_2017)[224] <- 'PRICE.VARR'
colnames(data_2018)[224] <- 'PRICE.VARR'

complete_data <- rbind(data_2014, data_2015, data_2016, data_2017, data_2018)
missing_plot(complete_data)
#difficult to see pattern
missing_pattern(complete_data)
missing_count <- sort((sapply(complete_data, function(x) sum(is.na(x)))), decreasing=TRUE)


#attempt mice

set.seed(123)

imputed_data_cart <- mice(complete_data, m=1, method='cart',maxit = 1)
imputed_data_rf <- mice(complete_data, m=1, method='rf',maxit = 1)
imputed_data_norm <- mice(complete_data, m=1, method='norm.predict',maxit = 1)

complete_cart <- complete(imputed_data_cart,1)
complete_rf <- complete(imputed_data_rf, 1)

write.csv(complete_data, 'complete_data.csv')

write.csv(complete_cart, 'cart_data.csv')

write.csv(complete_rf, 'rf_data.csv')

#look at differences between datasets

col_names <- as.list(names(complete_data))
no_na <- list('X', 'Sector', 'PRICE.VARR', 'Class', 'Year')
col_names <- setdiff(col_names, no_na)
for (i in c(2:224)){
  plot <- print(ggplot() + 
          geom_density(aes(x=complete_data[[i]], colour='red')) + 
          geom_density(aes(x=complete_cart[[i]], colour='green')) + 
          geom_density(aes(x=complete_rf[[i]], colour='blue')) + 
          scale_x_continuous(trans='log2') + 
          theme(legend.position = 'none') + labs(title=col_names[i], x=''))
  ggsave(paste(col_names[i],'plot.png',sep=''),plot = plot)
}
