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
sectors <- c('Basic Materials', 'Communication Services', 'Consumer Cyclical',
             'Consumer Defensive', 'Energy', 'Financial Services', 'Healthcare',
             'Industrials', 'Real Estate', 'Technology', 'Utilities')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme=shinytheme('yeti'),
    # Application title

    navbarPage(
        "Stock Evaluator",
        tabPanel('Search By Ticker',
            sidebarPanel(
                selectInput('sector_tick',
                            'Sector',
                            sectors),
                textInput('ticker',
                          'Ticker',
                          value=NA),
                actionButton('evaluate_tick',
                             'Evaluate')
            ),
            mainPanel(
                tags$style(
                    type = "text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }"
                ),
                htmlOutput('prediction_tick'),
                plotOutput('time_series'))
        ),
        tabPanel('Manual Entry',
                 fluidRow(
                     column(3,
                            textInput('Company.name',
                                      'Company Name'),
                            textInput('Consolidated.Income',
                                      'Consolidated Income'),
                            textInput('Dividend.payments',
                                      'Dividend Payments'),
                            selectInput('sector',
                                        'Sector',
                                        sectors),
                            actionButton('evaluate_man',
                                         'Evaluate')),
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
                 ),
                 
                 textOutput('prediction_man')
        )
        
    )
))