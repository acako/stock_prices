library(dplyr)
library(readr)
library(finalfit)
library(ggplot2)
library(grid)
library(gridExtra)
library(mice)
library(caret)
library(rpart)
library(ggpubr)
library(heatmaply)
library(factoextra)
library(gvlma)


#Load the data
df <- read.csv('fundamental_data.csv')
summary(df)

#normalized the dataset
df_normalize <- normalize(df)
summary(df_normalize)

plot_index <- list()
for(i in c(1:10)){
  plot_index[[i]] <- paste("plot_",i,sep='')
  print(plot_index[[i]])
  
}
#plot the normal distruibution plots
for (i in c(2:62)){
  
  plot_index[[names(df[i])]] <- ggplot(df_normalize, aes(x = df_normalize[[i]])) +
    stat_function(
      fun = dnorm,
      args = with(df_normalize, c(mean = mean(df_normalize[[i]], na.rm=TRUE), 
                            sd = sd(df_normalize[[i]], na.rm=TRUE))))+
    labs(title=as.list(names(df_normalize[i])), x='',y='Price Change')
  #ggsave(paste(as.list(names(df[i])),'plot.png',sep=''))
  print(plot_index[[names(df[i])]])
  
}

nCol <- 3
plot1 <- do.call("grid.arrange", c(plot_index[1:9], ncol=nCol))
plot2 <- do.call("grid.arrange", c(plot_index[10:20], ncol=nCol))
plot3 <- do.call("grid.arrange", c(plot_index[20:30], ncol=nCol))
plot4 <- do.call("grid.arrange", c(plot_index[30:40], ncol=nCol))
plot5 <- do.call("grid.arrange", c(plot_index[40:50], ncol=nCol))
plot6 <- do.call("grid.arrange", c(plot_index[50:62], ncol=nCol))

#It is customary to check for heteroscedasticity of residuals once you build the linear regression model. 
#The reason is, we want to check if the model thus built is unable to explain some pattern in the response variable (Y), 
#that eventually shows up in the residuals. 
#This would result in an inefficient and unstable regression model that could yield bizarre predictions later on.

#The plots we are interested in are at the top-left and bottom-left. 
#The top-left is the chart of residuals vs fitted values, 
#while in the bottom-left one, it is standardised residuals on Y axis. If there is absolutely no heteroscedastity, 
#you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.
for (i in c(2:3)){
  lmMod <- lm(PRICE.VARR ~ df[[i]], data=df) # initial model
  #This tests for the linear model assumptions and helpfully provides information on other assumptions. 
  #In this case we are going to look at the heteroskedasticity decisions, 
  #which has been identified as not being satisfied. 
  #We therefore reject the null hypothesis and state that there is heteroskedasticity 
  #in this model at the 5% significance level.
  print(gvlma(lmMod))
  #jpeg(file=paste(as.list(names(df[i])),'plot.jpeg',sep=''))
  par(mfrow=c(2,2)) # init 4 charts in 1 panel
  plot(lmMod,main=names(df[i]))
  #dev.off()
}
#some other ways to detect heteroscedasticity
#establish the presence or absence of heteroscedasticity – 
#The Breush-Pagan test and the NCV test
lmtest::bptest(lmMod)  # Breusch-Pagan test
car::ncvTest(lmMod)  # Breusch-Pagan test
