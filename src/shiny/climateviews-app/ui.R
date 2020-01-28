#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Climate change in the United States"),
    
    tabsetPanel(
        id="maintabs",
        tabPanel("Exposure",
                 h3('Climate change will affect everyone. Are you prepared?'),
                 p('Climate change will occur')
        ),
        tabPanel("Perception",
                 # Application title
                 h3("What do American think about climate change?"),
                 h3("How does exposure to natural disasters affect perceptions of climate change?")
        ),
        tabPanel("Preparedness",
                 # Application title
                 h3('header')
        )
    ),

    
))
