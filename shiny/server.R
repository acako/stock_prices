#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(xgboost)
#remove sci notation
options(scipen = 999)
#load in tickers
tickers <- read.csv('tickers.csv')
#load in model
model <- readRDS('XGB_model_albina_updated.rds')
#load in cluster info
cluster_info <- read.csv('clusters.csv')
#function to retrieve financial info if ticker is selected
fill_ticker_df <- function(tick){
    params <- list('datatype'='json')
    apikey <- 'apikey=81419012a8f4bf777c342c25e2ddaf77'
    url <-  'https://financialmodelingprep.com/api/v3/'
    mc_url <- paste(url,'market-capitalization/',tick,'?',apikey,sep = '')
    cash_url <- paste(url,'cash-flow-statement/', tick,'?', apikey, sep='')
    balance_url <- paste(url, 'balance-sheet-statement/', tick, '?',apikey, sep = '')
    income_url <- paste(url, 'income-statement/', tick, '?',apikey, sep = '')
    ticker_data <- data.frame(
        'ticker'=as.character(''),
        'Consolidated.income'=as.integer(0),
        'Dividend.payments'=as.integer(0),
        'Stock.based.compensation'=as.integer(0),
        'Income.Tax.Expense'=as.integer(0),
        'Retained.earnings..deficit.'=as.integer(0),
        'Operating.Cash.Flow'=as.integer(0),
        'Operating.Expenses'=as.integer(0),
        'R.D.Expenses'=as.integer(0),
        'Total.debt'=as.integer(0),
        'Long.term.debt'=as.integer(0),
        'Sector'=as.character(''),
        'cluster'=as.numeric(1),
        'Current.Market.Cap'=as.integer(0)
        )

    if (!is.na(tick)){
        mc_res <- GET(url = mc_url, query=params)
        cash_res <- GET(url = cash_url, query=params)
        balance_res <- GET(url=balance_url, query=params)
        income_res <- GET(url=income_url, query=params)
        ticker_data$ticker[1] <- as.character(tick)
        ticker_data$Consolidated.Income[1] <- content(income_res)[[1]]$netIncome
        ticker_data$Dividend.payments[1] <- content(cash_res)[[1]]$dividendsPaid
        ticker_data$Stock.based.compensation[1] <- content(cash_res)[[1]]$stockBasedCompensation
        ticker_data$Income.Tax.Expense[1] <- content(income_res)[[1]]$incomeTaxExpense
        ticker_data$Retained.earnings..deficit.[1] <- content(balance_res)[[1]]$retainedEarnings
        ticker_data$Operating.Cash.Flow[1] <- content(cash_res)[[1]]$operatingCashFlow
        ticker_data$Operating.Expenses[1] <- content(income_res)[[1]]$operatingExpenses
        ticker_data$R.D.Expenses[1] <- content(income_res)[[1]]$researchAndDevelopmentExpenses
        ticker_data$Total.debt[1] <- content(balance_res)[[1]]$totalDebt
        ticker_data$Long.term.debt[1] <- content(balance_res)[[1]]$longTermDebt
        ticker_data$Current.Market.Cap[1] <- content(mc_res)[[1]]$marketCap
    }
    return(ticker_data)
}

#function that pulls the historical market cap info
fill_time_series_df <- function(tick){
    params <- list('datatype'='json')
    apikey <- 'apikey=81419012a8f4bf777c342c25e2ddaf77'
    url <-  'https://financialmodelingprep.com/api/v3/'
    mc_url <- paste(url,'historical-market-capitalization/',tick,'?limit=254&',apikey,sep = '')
    if (!is.na(tick)){
        mc_res <- GET(url = mc_url, query=params)}
    
    date_list <- unlist(lapply(content(mc_res),'[[','date'))
    mc_list <- unlist(lapply(content(mc_res),'[[','marketCap'))
    time_series_df <- data.frame(
        'date'=as.Date(date_list,'%Y-%m-%d'),
        'market.Cap'=(as.numeric(mc_list)/10^9)
    )
    upper_lim <- unname(quantile(time_series_df$market.Cap, 0.98))[1]
    lower_lim <- unname(quantile(time_series_df$market.Cap, 0.02))[1]
    time_series_df <- time_series_df %>% subset(market.Cap < upper_lim) %>% subset(market.Cap > lower_lim)
    return(time_series_df)
}
shinyServer(function(input, output) {

    inputData <- eventReactive(input$evaluate_man, {
        input$evaluate
        isolate(data.frame(
            'Sector'=input$sector,
            'Consolidated.Income'=as.numeric(input$Consolidated.Income),
            'Dividend.payments'=as.numeric(input$Dividend.payments),
            'Stock.based.compensation'=as.numeric(input$Stock.based.compensation),
            'Income.Tax.Expense'=as.numeric(input$Income.Tax.Expense),
            'Retained.earnings..deficit.'=as.numeric(input$Retained.earnings..deficit.),
            'Operating.Cash.Flow'=as.numeric(input$Operating.Cash.Flow),
            'Operating.Expenses'=as.numeric(input$Operating.Expenses),
            'R.D.Expenses'=as.numeric(input$R.D.Expenses),
            'Total.debt'=as.numeric(input$Total.debt),
            'Long.term.debt'=as.numeric(input$Long.term.debt),
            'cluster'=as.numeric(1),
            'Current.Market.Cap'=as.numeric(input$Current.Market.Cap)))
    })
    
    
    ticker_pred <- eventReactive(input$evaluate_tick, {
        if (input$ticker %in% tickers$symbols) {
            pred_data <- fill_ticker_df(input$ticker)
            if (input$ticker %in% cluster_info$X){
                index <- which(cluster_info$X==input$ticker)[1]
                cluster <- as.numeric(cluster_info$cluster[[index]])
            } else {
                cluster <- as.numeric(1)
            }
            sector <- input$sector_tick
            pred_data <- pred_data %>% mutate(sector=sector)
            pred_data <- pred_data %>% mutate(cluster=cluster)
            pred_data$cluster <- as.factor(pred_data$cluster)
            pred_data$sector <- as.factor(pred_data$sector)
            current_mc <- pred_data$Current.Market.Cap[1]
            pred <- predict(model, newdata=pred_data)
            percent_diff <- round((pred-current_mc)/current_mc,4)*100
            if (pred < current_mc){
                diff <- '% lower '
            } else {
                diff <- '% higher '
            }
            if (percent_diff >= 1.05){
                    rec <- 'a strong buy.'
            } else if(percent_diff > 1.02 & percent_diff < 1.05){
                    rec <- 'a weak buy.'
            } else if (percent_diff > 0.98 & percent_diff <= 1.02){
                    rec <- 'to hold.'
            } else if (percent_diff > 0.95 & percent_diff <= 0.98){
                    rec <- 'a weak sell.'
            } else if(percent_diff <= 0.95){
                    rec <- 'a strong sell.'
            }
            mc_as_dollar <- dollar(current_mc)
            isolate(local(paste('The current market cap is: ',
                                mc_as_dollar,
                                '. The predicted market cap is ',
                                abs(percent_diff),
                                diff,
                                'than the current market cap.',
                                ' The recommendation for this stock is ',
                                rec,
                                sep = '')))
        } else {
            isolate(local(paste('Cannot find symbol.')))
            }
    })
    
    time_series_plot <- eventReactive(input$evaluate_tick, {
        mc_data <- fill_time_series_df(input$ticker)
        lower <- 0.95*min(mc_data$market.Cap)
        upper <- 1.05*max(mc_data$market.Cap)
        isolate(mc_data %>% 
                    ggplot(aes(x=date, y=market.Cap)) +
                    geom_line(color='red', size=2) +
                    ylim(lower,upper) +
                    ggtitle(input$ticker) +
                    labs(x='Date', y='Market Capitalization (in billions $)') +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    theme_bw())
    })
    
    manual_pred <- eventReactive(input$evaluate_man, {
            pred_data <- inputData()
            pred_data$Sector <- as.factor(pred_data$Sector)
            pred_data$cluster <- as.factor(pred_data$cluster)
            pred <- predict(model, newdata=pred_data)
            current_mc <- pred_data$Current.Market.Cap[1]
            percent_diff <- round(1-(pred-current_mc)/current_mc,4)*100
            if (pred < current_mc){
                diff <- '% lower '
            } else {
                diff <- '% higher '
            }
            if (percent_diff >= 1.05){
                rec <- 'a strong buy'
            } else if(percent_diff > 1.02 & percent_diff < 1.05){
                rec <- 'a weak buy.'
            } else if (percent_diff > 0.98 & percent_diff <= 1.02){
                rec <- 'to hold.'
            } else if (percent_diff > 0.95 & percent_diff <= 0.98){
                rec <- 'a weak sell.'
            } else if(percent_diff <= 0.95){
                rec <- 'a strong sell.'
            }
            mc_as_dollar <- dollar(current_mc)
            isolate(local(paste('The current market cap is: ',
                                mc_as_dollar,
                                '. The predicted market cap is ',
                                abs(percent_diff),
                                diff,
                                'than the current market cap.',
                                ' The recommendation for this stock is ',
                                rec,
                                sep = '')))
    })
    
    output$prediction_tick <- renderText({
        ticker_pred()
    })
    
    output$time_series <- renderPlot({
        if (input$ticker %in% tickers$symbols) {
        time_series_plot()}
    })
    
    output$prediction_man <- renderText({
        manual_pred()
    })

})
