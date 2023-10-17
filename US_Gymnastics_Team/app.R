#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# get.data.R data in the same directory level, inside which contains "data_2017_2021.csv" and "data_2022_2023.csv".
source('get.data.R')

## prep.data.R will output the cleaned data, "cleaned_combined_data.csv," into the data/ folder
source('prep.data.R')

## fit.model.R will output the per-athlete per-apparatus means and stddevs of scores into "means_per_app.csv" and "stddevs_per_app.csv" respectively into the data/ folder
source('fit.model.R')

## final script to get best teams and the US team simulations for all reasonable combinations 
source('get.best.teams.R')# outputs 2 lists (men & women) of 2 data frames each, 
# one is optimized top 12 teams, other is all 
# simulation results of country of interest (default: USA)
# with apparatus-athlete pairs and descending order of preference
# i.e. team on top is best

# Define UI
ui <- fluidPage(
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(
    
    # weight for gold medal
    numericInput("gold", "Enter a weight:", value = 50),
    
    # weight for silver medal
    numericInput("silver", "Enter a weight:", value = 30),
    
    # weight for bronze medal
    numericInput("bronze", "Enter a weight:", value = 15),
    
    # gender
    selectInput(inputId = "gender",
                label = "Gender: ",
                choices = c("Men", "Women"),
    
    # Select a state
    selectInput(inputId = "gold",
                label = "Gold Weight: ",
                choices = sort(unique(d$state)),
                selected = "CT"),
    
    # Select a network
    selectInput(inputId = "silver",
                label = "Silver Weight:", 
                choices = c("All", top5networks, "Other")),
    
    # Select a level
    selectInput(inputId = "level",
                label = "Level:",
                choices = c("Level 2" = "lev2",
                            "Level 3" = "lev3",
                            "Total" = "total"),
                selected = "lev3"),
    
    # Select a start date 
    sliderInput(inputId = "start_date",
                label = "Start Date:",
                min = as.Date("2010-01-01","%Y-%m-%d"), 
                max = as.Date("2023-01-15","%Y-%m-%d"),
                value = as.Date("2015-01-01"),
                timeFormat ="%Y-%m-%d"),
    
    # Select an end date 
    sliderInput(inputId = "end_date",
                label = "End Date:",
                min = as.Date("2010-01-01","%Y-%m-%d"), 
                max = as.Date("2023-01-15","%Y-%m-%d"),
                value = as.Date("2023-01-15"),
                timeFormat ="%Y-%m-%d")
  ),
    mainPanel = mainPanel(textOutput("output_1"))
  ))

# Server function
server <- function(input, output) {
  output$output_1 <- renderText({
    paste("You have selected", input$input_1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
