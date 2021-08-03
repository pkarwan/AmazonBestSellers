#install.packages(c("shiny", "shinydashboard", "tidyverse", "dplyr", "ggplot2", "stringr", "caret", "DT", "plotly", "tree", "rattle", "randomForest", "shinycssloaders" ))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(plotly)
library(shinycssloaders)

#Load data
bestSellerData<- as.tibble(read.csv("./data/bestsellers_with_categories.csv"))
bestSellerData
#bestSellerData <- bestSellerData %>% unique.data.frame(bestSellerData$Name)
#str(bestSellerData)


# Filter for the day of the week, remove weekday column and convert columns to factor as appropriate.

factorCols <- c("Year","Genre")
bestSellerData <-  bestSellerData %>% select(everything()) %>%mutate(across(factorCols, factor))

bestSellerData


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Amazon Best Seller"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("clipboard")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Data Exploration", tabName = "dataexp", icon = icon("search",lib = "glyphicon")),
        menuItem("Data Exploration2", tabName = "Data_Exploration2", icon = icon("search",lib = "glyphicon")),
        menuItem("Modeling", tabName = "modeling", icon = icon("signal",lib = "glyphicon"))               
      )),
    
    #Data Exploration page
    dashboardBody(
      tabItems(
        tabItem(tabName = "dataexp",
                fluidRow( h2("3. Data Exploration"),br(), br(),
                          h4("  As we will be focusing on the the prediction of best selling book, the data exploration
                                      will concentrate for that on user ratings. The data table for the highest user ratings 
                                      shown for the demonstration purpose only, which can be dowloaded from the 
                                      `Dataset` page. Therefore no download option is provided here. 
                                      "),
                          br(), br(),
                          
                          h3("Select the user ratings to generate the numerical and graphical summaries:"),
                          column(width = 6,
                                 box(selectInput("var", "Select the User Ratings", 
                                                 selected = max(bestSellerData$User.Rating),
                                                 choices = (bestSellerData$User.Rating)),
                                     
                                     sliderInput("bins", "Number of bins for histogram:",
                                                 min=1, max=10, value=9))),
                          
                          box("Histogram for User Ratings ", uiOutput("sumh") , status = "primary", solidHeader = T,
                              collapsible = T, plotlyOutput("hist")),
                          box("Summary statistics for some of the attributes", verbatimTextOutput("summ")),
                          
                          box("An overview of Data Table for the User Ratings you select", dataTableOutput ("table")),
                          
                          #bar plot for seleceted year
                          h3("Bar plot for the selected selling book year as below :"),
                          box(selectInput("year", "Select the Year", 
                                          selected = "2015",
                                          choices = levels(bestSellerData$Year),
                          ),
                          br(),
                          
                          h4("Highest number of reviews recorded on the year selected=", textOutput("hiwin"))
                          ),
                          
                          column(width = 12,
                                 box("Bar Plot", status = "primary", solidHeader = T,
                                     collapsible = T, plotlyOutput("bar"))),
                          
                          box("Scatter Plot", plotlyOutput("scat"))
                          
                )),
        
        
        tabItem(tabName = "Data_Exploration2",
                 tags$div(selectInput("variableName", "Variables:", selected = "Reviews",
                                 choices = colnames(bestSellerData)),
                     br(),
                   ),
                tags$div(
                     h4("Summary"),
                     verbatimTextOutput("summary"),
                     br(),
                     tags$div( withSpinner(plotOutput("histPlot"),  type = getOption("spinner.type", default = 6),
                                           color = getOption("spinner.color", default = "#FFA500")), 
                               downloadButton(outputId = "histPlotDownload", label = "Download Plot")),
                     
                   )
                 
        ),
        
        #Modeling Page
        
        tabItem(tabName = "modeling",
                  fluidPage(
                    tabsetPanel(
                      tabPanel(
                        "Modeling Info",
                        
                        tags$div(
                          fluidRow(strong("Multiple Linear Model")),
                                  tags$div( "Multiple Linear Model is a statistical technique that uses more than one explanatory variables to predict the outcome of a response variable. 
                                            The goal of multiple linear regression (MLR) is to model the linear relationship between the explanatory (independent) variables and response (dependent) variable."),
                                  h3("Formula and Calculation of Multiple Linear Regression"),
                                  tags$div("${Y}_{i} = \beta_{0} + \beta_{1}{x}_{i}{1} + \beta_{2}{x}_{i}{2} +...+ \beta_p{x}_{i}{p} + {E}_{i}$"),
                          fluidRow("Advantages : "),
                                  tags$ul("Multiple regression allows a statistician to explore the effect of more than one variable on the outcome he wants to study."),
                                  tags$ul("It uses data very efficiently and can make useful predictions with small data sets."),        
                                  tags$ul("Ability to determine the relative influence of one or more predictor variables to the criterion value."),
                                  tags$ul("Ability to identify outliers, or anomalies."),
                                  tags$ul("It can allow to construct easy equations of various parameters which can help give predictions and can accordingly be optimized."),
                            br(),       
                          fluidRow("Drawbacks :"),
                                tags$ul("By its nature, multiple linear regression only looks at linear relationships between dependent and independent variables."),
                                tags$ul("Multiple Linear Regression Is Sensitive to Outliers"),
                                tags$ul("Data needs to be independent")
                          
                        ),br(),br(),
                        
                        tags$div(
                          fluidRow(strong("Random Forest Model")),
                          fluidRow("Advantages : "),
                          tags$ul("Random Forest works well with both categorical and continuous values"),
                          tags$ul("It reduces overfitting in decision trees and helps to improve the accuracy"),
                          tags$ul("It automates missing values present in the data"),
                          tags$ul("Normalising of data is not required as it uses a rule-based approach."),
                          tags$ul("It is flexible to both classification and regression problems"),br(),
                          fluidRow("Drawbacks :"),
                          tags$ul("Random Forest Model requires much computational power as well as resources as it builds numerous trees to combine their outputs. "),
                          tags$ul("It also requires much time for training as it combines a lot of decision trees to determine the class."),
                          tags$ul("Due to the ensemble of decision trees, it also suffers interpretability and fails to determine the significance of each variable"),
                        ),br(),br(),
                        
                        tags$div(
                          fluidRow(strong("Classification Tree Model")),
                          fluidRow("Advantages : "),
                          tags$ul("Simple to understand and easy to interpret output"),
                          tags$ul("Predictors don't need to be scaled"),
                          tags$ul("No statistical assumptions necessary"),
                          tags$ul("Built in variable selection"),
                          br(),
                          fluidRow("Drawbacks :"),
                          tags$ul("Small changes in data can vastly change tree"),
                          tags$ul("Greedy algorithm necessary (no optimal algorithm)"),
                          tags$ul("Need to prune"),
                        )
                      ),
                      tabPanel(
                        "Modeling Fitting",
                        fluidPage(
                          fluidRow(
                            #slider
                          ),
                          fluidRow(
                            #models
                            column(4, "Multiple Linear Model"),
                            column(4, "Random Forest Model"),
                            column(4, "Classification Tree Model")
                          )
                        )
                      ),
                      tabPanel(
                        "Prediction",
                        sidebarLayout(
                          sidebarPanel(
                            #Predictors Selection
                          ),
                          mainPanel(
                            #Prediction for the selected predictors
                          )
                        )
                      )
                    )
                )),
        
        #Dataset page
        
        tabItem(tabName = "data",
                fluidPage( h2("2. Dataset"), br(), br(),
                          h4("The intention of this page is to allow the user navigate throught the whole
                                    data set and play around to choose the attributes as per the requirement of
                                    user. Here you can also subset the data set and downoload as '.csv' file."), br(),
                          
                          tags$div( 
                            br(),
                            h4("Select options below to subset the data"),
                            br(),
                            
                            varSelectInput("dtsetvar", "Select variables to subset", bestSellerData, multiple = T),
                             ),
                          
                          tags$div(downloadButton("downloadData", "Download"),
                              dataTableOutput("dtset") ) 
                          

                          
                )),
        
        # About page            
        tabItem(tabName = "about",
                h2("About"),
                img(src = "https://github.com/pkarwan/WaterQualityDataExploration/blob/main/image/waterQuality.png", width = "800px", height = "100px"),
                h3("Purpose of the App"),
                h4("Our goal here will be to fit the model and predict the Amazon best seller for 2009 to 2019 and measure the statistics, how accurately can it be predicted. We will be using  the Random Forest model, Classical Regression Tree and
                         Multiple Linear model to fit the model and predict the best seller. Then choose the best one."),
                br(),
                br(),   
                
                h3("Navigate Through the App: This app will have 4 different components"),
                h3("1. About"), 
                h4("This page describes purpose of the app, purpose of each tab (page) of the app and information about the data"),
                
                h3("2. Data"),
                h4("In this page user can scroll through the data, subset it and save the data as a '.csv' file."),
                
                h3("3. Data Exploration"),
                h4("In this page displays numerical and graphical summaries. User can download plot, change type of plot and type of summary reported, change the variable and filter the rows to change the data in the plots/summaries"),
                
                
                h3("4. Modeling"),
                h4("This page will display three supervised learning models - multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model.This page contains three tabs :"),
                em(h4(strong("I. Modeling Info tab :"))),
                h4("This tab displays three modeling approaches, the benefits of each, and the drawbacks of each."),
                
                em(h4(strong("II. Model Fitting tab :"))),
                h4("User can split the data and choose the proportion of data used in each training and testing data set. User can choose model settings for each model. The models get compared on the test set and appropriate fit statistics reported."),
                em(h4(strong("III. Prediction tab :"))),
                h4("User can choose one of the models for prediction. User can select the values of the predictors and obtain a prediction for the response."),
                br(),
                br(),
                
                
                
                h2("Data Set Information"),
                h4("This data set is donloaded from ", 
                   a(href= "https://www.kaggle.com/sootersaalu/amazon-top-50-bestselling-books-2009-2019", "kaggle"),
                   "This dataset describes Amazon's Top 50 bestselling books from 2009 to 2019. It contains 550 books, data has been categorized into fiction and non-fiction using Goodreads "),
                br(),
                
                h3("Attribute Information"),
                br(),
                
                h4("1 - Name", "- Name of the book", br(), br(),
                   "2 - Author", "- Author of the book", br(),br(),
                   "3 - User Rating", "- User Rating", br(),br(), 
                   "4 - Reviews", "- User Reviews", br(), br(),
                   "5 - Price", "- Cost of the book", br(), br(),
                   "6 - Year","- Year of the selling book", br(),br(), 
                   "7 - Genre","- Genre of the book (fiction/non-fiction)", br(), br(),
                   br(), br(), br(), br(),
                )
        
        )
      )
    )
  )
)
