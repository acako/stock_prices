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
shinyServer(function(input, output) {

    inputData <- reactive({
        input$predict
        isolate(data.frame(
            'Ticker'=as.character(input$Ticker.symbol),
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
    
    output$prediction <- renderText({
        input$predict
        if (input$predict == 0) {
            paste('Server is ready for calculation.')
            return()
        }
        data <- inputData()
        
    })

})
