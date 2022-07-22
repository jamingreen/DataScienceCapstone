library(shiny)
library(markdown)

navbarPage("Natural Language Processing",
  tabPanel("Prediction",
    sidebarLayout(
      sidebarPanel(
        h2("Prediction"),
        textInput("sen", "sentence")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("predSen")
      )
  )),
  tabPanel("generate", sidebarLayout(
      sidebarPanel(
       p("The tab generate sentence with n-gram algorith."),
       br(),
       p("You can enter the number of words in the sentence"),
       textInput("num", "number of words"),
      ),
      mainPanel(
       textOutput("genSen")
      )
  )),
  tabPanel("Unigram phrase table",
           sidebarLayout(
             sidebarPanel(tableOutput("uniPt")),
             mainPanel(
               plotOutput("uniPlot")
             )
           )
  ),
  tabPanel("Bigram phrase table",
   sidebarLayout(
     sidebarPanel(tableOutput("biPt")),
     mainPanel(
       plotOutput("biPlot")
     )
   )
  ),
  tabPanel("Trigram phrase table",
           sidebarLayout(
             sidebarPanel(tableOutput("triPt")),
             mainPanel(
               plotOutput("triPlot")
             )
           )
  ),
           
)
