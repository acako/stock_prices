#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme=shinytheme('yeti'),
    # Application title
    titlePanel("Stock Evaluator"),

    navbarPage(
        "Stock Evaluator",
        tabPanel('Manual Entry',
            fluidRow(
                column(3,
                    textInput('Company.name',
                              'Company Name'),
                    textInput('Consolidated.Income',
                              'Consolidated Income'),
                    textInput('Dividend.payments',
                              'Dividend Payments')),
                column(3,
                    textInput('Stock.based.compensation',
                              'Stock Based Compensation'),
                    textInput('Income.Tax.Expense',
                              'Income Tax Expense'),
                    textInput('Retained.earnings..deficit.',
                              'Reatained Earnings')),
                column(3,
                    textInput('Operating.Cash.Flow',
                              'Operating Cash Flow'),
                    textInput('Operating.Expenses',
                              'Operating Expenses'),
                    textInput('R.D.Expenses',
                              'R&D Expenses')),
                column(3,
                    textInput('Total.debt',
                              'Total Debt'),
                    textInput('Long.term.debt',
                              'Long Term Debt'),
                    textInput('Current.Market.Cap',
                              'Current Market Cap')),
            )
        ),
        tabPanel('Search By Ticker',
            sidebarPanel(
                textInput('ticker',
                          'Ticker')
            )
        )
        
    )
))