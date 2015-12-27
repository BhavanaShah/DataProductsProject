#Coursera Data Product Project
#Bhavana Shah

#ui.R
library(shiny)
library(e1071)

shinyUI(pageWithSidebar(
        headerPanel('Heart Disease Prediction using Random Forest'),
        sidebarPanel(width = 3,
                #Input fns
               
                sliderInput(inputId = "numtrees", label = "Number of trees:",
                            100, 1000, value = 500, step = 50) ,
                hr(),
                hr(),
                hr(),
                hr(),
                hr(),
                h6 ("This Shiny application demonstrates Random Forest Model updation as the number of trees is changed by the user")
                
        ),
        mainPanel(
                width = 9,
                tabsetPanel(selected = "Model Visualization", 
                           
                            tabPanel("About", htmlOutput("about")),
                            tabPanel("Model Visualization",
                                     htmlOutput("aboutRF"),
                                     h3("Built Model"),
                                     verbatimTextOutput("modelSummary"),
                                     h3("Confusion Matrix"),
                                     verbatimTextOutput("cmRF"), 
                                     h3("Variable Importance"),
                                     htmlOutput("aboutVI"),
                                     plotOutput("varImpPlot"),
                                     h3("Error Rate per class and OOB Estimates"),
                                     htmlOutput("aboutER"),
                                     plotOutput("oobPlot", width = "100%"),
                                     h3("Margin of Predictions"),
                                     htmlOutput("aboutMP"),
                                     plotOutput("plotRFmp", width = "100%")
                            ),
                               tabPanel("Data", dataTableOutput("dtOutput") ),
                               tabPanel("Exploration",
                                        h3("Correlation Matrix"),
                                        plotOutput("corrPlot", width = "100%"),                                
                                        h3("Pairs Plot"),
                                        plotOutput("pairsPlot", width = "100%")
                                        )                                        
               )
        )
))

 