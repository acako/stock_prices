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
sectors <- list('Basic Materials', 'Communication Services', 'Consumer Cyclical',
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
                            selectInput('sector',
                                        'Sector',
                                        sectors),
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
                                      'Current Market Cap'),
                            actionButton('evaluate_man',
                                         'Evaluate')),
                 ),
                 
                 textOutput('prediction_man')
        ),
        tabPanel('Information',
                 h2('About the authors'),
                 h4('Sean X. Zhang'),
                 tags$line('Sean works as an Analyst specializing in health data privacy at a global analytics and consulting company. He completed his BSc. Neuroscience and MSc. Experimental Medicine (with a focus on Biomedical ethics) both at McGill University and is completing a Certificate in Machine Learning from York University. Coming from an interdisiplinary background of biomedical science, data science, and ethics, Sean aims to apply his diverse skillset to improve data-driven solutions within the healthcare sector.'),
                 h4('Lucy Zhang'),
                 tags$line('Lucy completed her degree in Software Engineering at McMaster University.'),
                 h4('Colin Green'),
                 tags$line('Colin Green holds an Honours Bachelor of Science with a major in Physics and a minor in Business from the University of Ottawa and is currently a student a York University studying machine learning.  As an aspiring data scientist, he hopes to one day contribute to the solution of the climate crisis with the use of machine learning.'),
                 h4('Albina Cako'),
                 tags$line('Education: Bachelor of Science (Honors with Distinction) Ryerson University. Currently on my 4th year of Doctor of Naturopathic Medicine Degree at the Canadian College of Naturopathic Medicine. As well, doing a Certificate on Machine learning at York University and a Masters of Biomedical Engineering at Ryerson University focusing on Machine Learning. Interests: Machine learning in the medical field, as well as use of Machine learning in technological innovation, marketing, social media and business development.'),
                 h2('Model Information'),
                 HTML('<p>The model uses the XGBoost model to predict the market cap and was trained using publically available data found <a href ="https://www.kaggle.com/cnic92/200-financial-indicators-of-us-stocks-20142018"> here.</a></p>'),
                 h4('Limitations'),
                 tags$line('The predictive capabilities are limited by the models accuracy (around 90%) as well as the availability of the data and the date it was published. This model should only be used as a supplemental tool and should not be considered as financial advice.')
        )
    )
))