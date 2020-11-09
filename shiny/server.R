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
#load in tickers
tickers <- read.csv('tickers.csv')

fill_ticker_df <- function(tick){
    params <- list('datatype'='json')
    apikey <- '?apikey=81419012a8f4bf777c342c25e2ddaf77'
    url <-  'https://financialmodelingprep.com/api/v3/'
    mc_url <- paste(url,'market-capitalization/',tick,apikey,sep = '')
    full_statement_url <- paste(url,'financial-statement-full-as-reported/',tick,apikey, sep = '' )
    cash_url <- paste(url,'cash-flow-statement/', tick, apikey, sep='')
    balance_url <- paste(url, 'balance-sheet-statement/', tick, apikey, sep = '')
    income_url <- paste(url, 'income-statement/', tick, apikey, sep = '')
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
        'Current.Market.Cap'=as.integer(0))
    if (!is.na(tick)){
        mc_res <- GET(url = mc_url, query=params)
        full_res <- GET(url = full_statement_url, query=params)
        cash_res <- GET(url = cash_url, query=params)
        balance_res <- GET(url=balance_url, query=params)
        income_res <- GET(url=income_url, query=params)
        ticker_data$ticker[1] <- NA
        ticker_data$Consolidated.income[1] <- content(income_res)[[1]]$netIncome
        ticker_data$Dividend.payments[1] <- content(full_res)[[1]]$paymentsofdividends
        ticker_data$Stock.based.compensation[1] <- content(cash_res)[[1]]$stockBasedCompensation
        ticker_data$Income.Tax.Expense[1] <- content(full_res)[[1]]$incometaxespaidnet
        ticker_data$Retained.earnings..deficit.[1] <- content(full_res)[[1]]$retainedearningsaccumulateddeficit
        ticker_data$Operating.Cash.Flow[1] <- content(cash_res)[[1]]$operatingCashFlow
        ticker_data$Operating.Expenses[1] <- content(full_res)[[1]]$operatingexpenses
        ticker_data$R.D.Expenses[1] <- content(full_res)[[1]]$researchanddevelopmentexpense
        ticker_data$Total.debt[1] <- content(balance_res)[[1]]$totalDebt
        ticker_data$Long.term.debt[1] <- content(balance_res)[[1]]$longTermDebt
        ticker_data$Current.Market.Cap[1] <- content(mc_res)[[1]]$marketCap
    }
    return(ticker_data)
}

shinyServer(function(input, output) {

    inputData <- reactive({
        input$evaluate
        isolate(data.frame(
            'Ticker'=as.character(input$ticker),
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
    
    
    
    output$prediction_tick <- renderText({
        input$evaluate_tick
        if (input$evaluate_tick == 0) {
            paste('Server is ready for calculation.')
            return()
        if (input$ticker %in% tickers$symbols) {
            ticker_df <- fill_ticker_df(input$ticker)
            local(paste(ticker_df$Current.Market.Cap))
        } else {
            'Cannot find symbol.'
            }
        }
        
    })

})
