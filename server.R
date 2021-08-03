# Project III, ST 558
#Author: Pramodini Karwande

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(DT)
library(tree)
library(rattle)
library(randomForest)
library(plotly)
library(shinycssloaders)


bestSellerData<- as.tibble(read.csv("./data/bestsellers_with_categories.csv"))


# Filter for the day of the week, remove weekday column and convert columns to factor as appropriate.

factorCols <- c("Year","Genre")
bestSellerData <-  bestSellerData %>% select(everything()) %>%mutate(across(factorCols, factor))
bestSellerData


name<- names(bestSellerData)

# Define server 
shinyServer(function(input, output, session) {
  #dynamicUI
  output$sumh<- renderUI({(
    h4(toupper(str_to_title(input$User.Ratings)))
  )}) 
  
  #Histogram plot        
  output$hist<- renderPlotly({
    hist1<- ggplot(bestSellerData, aes(x=bestSellerData$User.Rating, ..density..)) +geom_histogram(bins=input$bins) + ylab("Density") + xlab("User Ratings") +
      geom_density(col="red", lwd=2, adjust=1)  
    
    ggplotly(hist1)
  })
  
  #summary 
  output$summ<- renderPrint({
    summary(bestSellerData %>% filter(User.Rating==input$var)  %>%  select(Price, Reviews))
  })
  
  output$table<- renderDataTable({
    datatable( bestSellerData %>% filter(User.Rating==input$var))
  })
  
  #Highest sells for the year
  
  output$hiwin<- renderText({
    dta4<- bestSellerData %>% filter(Year== input$year) %>% group_by(Year) %>% 
      summarise_at(vars(Reviews), list(totrev=sum) )
    max(dta4$totrev)
  })
 
  #scatter Plot
  
  output$scat<- renderPlotly ({
    scp<-  ggplot(bestSellerData, aes(x=input$year, y=bestSellerData$Reviews)) + geom_point(aes(color=Year)) +
      geom_smooth(method = 'lm', color='green') + ylab("User Reviews") + xlab("Year Selected")
    
    ggplotly(scp)
  })
  
  
  #Bar plot of reviews for choosen year
  
  output$bar<- renderPlotly({
    bar1<- ggplot(bestSellerData, aes(x=input$year))  + geom_bar(aes(color=Genre), position = "dodge") + 
       xlab("Year Selected")
    ggplotly(bar1)
  })
  
  # Full data table with options to subset attributes
  
  
  # Reactive value for data Table
  datasetInput<- reactive({
    
    if(length(input$dtsetvar) != 0){
      dtFull<- bestSellerData %>% dplyr::select(!!!input$dtsetvar)
      return(dtFull) 
    }
    
  else
    return(bestSellerData)
  
   })
  
  #Table of selected dataset
  
  output$dtset<- renderDataTable({
    datasetInput()
  }, filter = "top")
  
  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Best_Sellers_With_Categories", ".csv", sep = "")
    },
    content = function(file) {
      #setClass(data.frame(datasetInput()))
     # class(datasetInput())<- class(as.tibble(datasetInput()))
      write.csv(as.data.frame(datasetInput()), file, row.names = FALSE)
    }
  )
  
  
  #******************************************************#
  #*Data Exploration 2
  # Output numerical summary
  output$summary <- renderPrint({
    if(input$variableName == "Price" || input$variableName == "Reviews" || input$variableName == "User.Rating") 
      summary(bestSellerData[,input$variableName])
      
    
    else if(input$variableName == "Genre") 
      table(bestSellerData[,input$variableName])
    
    else if(input$variableName == "Year") 
      table(bestSellerData[,input$variableName],bestSellerData %>% group_by(bestSellerData$Year) %>% summarise(bestSellerData$Reviews))
      
  })
  
  #Plot Helper function
  histplotHelper <- function() {
    ggplot(bestSellerData, aes(input$variableName)) + geom_bar(aes(color = Genre)) + ggtitle(input$variableName)
  }
  
  output$histPlot <- renderPlot({
    if(input$variableName == "Price" || input$variableName == "Reviews" || input$variableName == "User.Rating") {
      histplotHelper()
    }
      
      
    if(input$variableName == "Year" || input$variableName == "Genre") {
      ggplot(bestSellerData, aes(Year, User.Rating)) +
            geom_boxplot(aes(color = Genre)) + xlab("Year") + ylab("User Ratings") +
            ggtitle("Year vs User Ratings ")
    }
  })
  
  #Download plots
  output$histPlotDownload <- downloadHandler(
    filename =  function() {
      paste0(input$variableName,".png")
    },
    content = function(file) {
      png(file) # open the png device
      print(histplotHelper())
      dev.off()  # turn the device off
      
    }
  )
  
  
})

