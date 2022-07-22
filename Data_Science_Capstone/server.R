source("helpers.R")
library(shiny)
library(ggplot2)

p1 <- readRDS("plots/plot1.rds")
p2 <- readRDS("plots/plot2.rds")
p3 <- readRDS("plots/plot3.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$predSen <- renderText({
      a <- pred(input$sen)
      paste(input$sen, a)
    })
    
    output$genSen <- renderText({
      a <- generate(input$num)
      a
    })
    
    output$uniPlot <- renderPlot({p1})
    output$biPlot <- renderPlot({p2})
    output$triPlot <- renderPlot({p3})
    
    output$uniPt <- renderTable({
      head(pt1,10)
    })
    
    output$biPt <- renderTable({
      head(pt2,10)
    })
    
    output$triPt <- renderTable({
      head(pt3,10)
    })

})
