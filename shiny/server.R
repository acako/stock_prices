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
#load in tickers
options(scipen = 999)
tickers <- read.csv('tickers.csv')

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
        'Current.Market.Cap'=as.integer(0)
        )

    if (!is.na(tick)){
        mc_res <- GET(url = mc_url, query=params)
        cash_res <- GET(url = cash_url, query=params)
        balance_res <- GET(url=balance_url, query=params)
        income_res <- GET(url=income_url, query=params)
        ticker_data$ticker[1] <- as.character(tick)
        ticker_data$Consolidated.income[1] <- content(income_res)[[1]]$netIncome
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
    #upper_lim <- unname(quantile(time_series_df, 0.98))[1]
    #lower_lim <- unname(quantile(time_series_df, 0.02))[1]
    #time_series_df <- time_series_df %>% subset(market.Cap < upper_lim & market.Cap > lower_lim)
    return(time_series_df)
}
shinyServer(function(input, output) {

    inputData <- reactive({
        input$evaluate
        isolate(data.frame(
            'Company.name'=as.character(input$Company.name),
            'Consolidated.income'=as.integer(input$Consolidated.income),
            'Dividend.payments'=as.integer(input$Dividend.payments),
            'Stock.based.compensation'=as.integer(input$Stock.based.compensation),
            'Income.Tax.Expense'=as.integer(input$Income.Tax.Expense),
            'Retained.earnings..deficit.'=as.integer(input$Retained.earnings..deficit),
            'Operating.Cash.Flow'=as.integer(input$Operation.Cash.Flow),
            'Operating.Expenses'=as.integer(input$Operating.Expenses),
            'R.D.Expenses'=as.integer(input$R.D.Expenses),
            'Total.debt'=as.integer(input$Total.debt),
            'Long.term.debt'=as.integer(input$Long.term.debt),
            'Current.Market.Cap'=as.integer(input$Current.Market.Cap)))
    })
    
    
    
    text <- eventReactive(input$evaluate_tick, {
        if (input$ticker %in% tickers$symbols) {
            current_mc <- round((fill_ticker_df(input$ticker))$Current.Market.Cap)
            rint <- runif(1,0.95,1.05)
            pred <- round(rint-1,4)*100
            if (pred < 1){
                diff <- '% lower '
            } else {
                diff <- '% higher '
            }
            if (pred/current_mc >= 1.025){
                    rec <- 'a strong buy'
            } else if(pred/current_mc > 1.01 & pred/current_mc < 1.025){
                    rec <- 'a weak buy.'
            } else if (pred/current_mc > 0.99 & pred/current_mc <= 1.01){
                    rec <- 'to hold.'
            } else if (pred/current_mc > 0.975 & pred/current_mc <= 0.99){
                    rec <- 'a weak sell.'
            } else if( pred/current_mc <= 0.975){
                    rec <- 'a strong sell.'
            }
            isolate(local(paste('The current market cap is: $',
                                current_mc,
                                '. The predicted market cap is ',
                                abs(pred),
                                diff,
                                'than the current market cap.',
                                'The recommendation for this stock is ',
                                rec,
                                sep = '')))
        } else {
            isolate(local(paste('Cannot find symbol.')))
            }
    })
    
    time_series_plot <- eventReactive(input$evaluate_tick, {
        data <- fill_time_series_df(input$ticker)
        lower <- 0.95*min(data$market.Cap)
        upper <- 1.05*max(data$market.Cap)
        isolate(data %>% 
                    ggplot(aes(x=date, y=market.Cap)) +
                    geom_line(color='red', size=2) + geom_point(size=3) +
                    ylim(lower,upper) +
                    ggtitle(input$ticker) +
                    labs(x='Date', y='Market Capitalization (in billions $)') +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    theme_bw())
    })

    
    output$prediction_tick <- renderText({
        text()
    })
    output$time_series <- renderPlot({
        if (input$ticker %in% tickers$symbols) {
        time_series_plot()}
    })

})
