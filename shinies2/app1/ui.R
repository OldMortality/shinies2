# app 1
# Showing the population height distribution,
#   and allows you to find your own height
#
library(shinydashboard)
library(ggplot2) 

shinyUI <- dashboardPage(
  
  dashboardHeader(
     
    title = "p-values at play",
                  titleWidth = 800),
  dashboardSidebar(
    tags$head(
      # link to the css stylesheet. It is in the www folder.
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    sliderInput("N", label="Number of experiments", 
                min=20, max=1000, value=100, step = 1,
                round = TRUE),
    sliderInput("S", label="Sample size (per group)", 
                min=5, max=100, value=20, step = 1,
                round = TRUE),
    sliderInput("pperc", label="Proportion with true difference", 
                min=0, max=1, value=0.2, step = 0.01,
                round = FALSE),
    sliderInput("alpha", label="alpha", 
                min=0, max=0.1, value=0.05, step = 0.001,
                round = FALSE),
    sliderInput("mu", label="true difference in mu", 
                min=0, max=2, value=0.5, step = 0.01,
                round = FALSE),
    sliderInput("sd", label="standard deviation ", 
                min=0, max=2, value=0.5, step = 0.01,
                round = FALSE),
    actionButton("goButton", "run again")
    
     
      ), 
  dashboardBody( 
    tags$head(
      # link to the css stylesheet. It is in the www folder.
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow( 
      column(width = 12,
              box(
                title="", 
                width=NULL,
                plotOutput("plot1", height = 350)), 
              box( 
                title="", 
                width=NULL,
                htmlOutput('summary')
              )
            )
      )
  )
)

