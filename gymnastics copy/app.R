library(shiny)
library(dplyr)

men_best <- readRDS("best.teams.mens.rds")
women_best <- readRDS("best.teams.womens.rds")
means_df <- readRDS("means_df.rds")
key <- readRDS("name_ID_key.rds")
source("query.existing.database.R")

ui <- fluidPage(
  titlePanel("Olympic Gymnastics Analysis"),
  sidebarLayout(
    sidebarPanel(
      # gender 
      selectInput("gender", "Gender: ", c("Men", "Women")),
      
      # country of interest 
      selectInput("country", "Country: ", choices = NULL, selected = "USA"),
      
      # weight for gold medal
      numericInput("gold", "Gold weight: ", value = 50, min = 0),
      
      # weight for silver medal
      numericInput("silver", "Silver weight: ", value = 30, min = 0),
      
      # weight for bronze medal
      numericInput("bronze", "Bronze weight: ", value = 15, min = 0),
      
      # included players
      selectInput("include_players", "Include Players (up to 5): ", 
                  choices = NULL, multiple = TRUE),
      
      # excluded players
      selectInput("exclude_players", "Exclude Players (up to 5): ", 
                  choices = NULL, multiple = TRUE),
      
      # table columns
      selectInput("display_columns", "Data to display: ", 
                  choices = NULL, multiple = TRUE),
      
      # Submit button
      actionButton("submit_btn", "Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Top 10 Results", dataTableOutput("top_results"))
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    # Update country choices based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "country", 
                        choices = unique(men_best$finalteams$Country[men_best$finalteams$Gender == "m"]))
    } else {
      updateSelectInput(session, "country", 
                        choices = unique(women_best$finalteams$Country[women_best$finalteams$Gender == "w"]))
    }
  })
  
  observe({
    # Update choices dynamically based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "include_players", 
                        choices = get_names_for_IDs(unique(na.omit(means_df$ID[means_df$Gender == "m" & means_df$Country == input$country])), key))
    } else {
      updateSelectInput(session, "include_players", 
                        choices = get_names_for_IDs(unique(na.omit(means_df$ID[means_df$Gender == "w" & means_df$Country == input$country])), key))
    }
    
    # Update choices dynamically based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "exclude_players", 
                        choices = get_names_for_IDs(unique(na.omit(means_df$ID[means_df$Gender == "m" & means_df$Country == input$country])), key))
    } else {
      updateSelectInput(session, "exclude_players", 
                        choices = get_names_for_IDs(unique(na.omit(means_df$ID[means_df$Gender == "w" & means_df$Country == input$country])), key))
    }
  })
  
  observe({
    # Update column choices based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "display_columns", 
                        choices = names(men_best$interestedteam))
    } else {
      updateSelectInput(session, "display_columns", 
                        choices = names(women_best$interestedteam))
    }
  })
  
  # Event handler for the Submit button
  observeEvent(input$submit_btn, {
    # Check the value of input$gender and update the data frame accordingly
    selected_data <- if (input$gender == "Women") {
      women_best$interestedteam
    } else {
      men_best$interestedteam
    }
    
    # Modify scores based on weights
    selected_data <- weights.modification(selected_data,
                                          gold = input$gold,
                                          silver = input$silver,
                                          bronze = input$bronze)
    
    if (!is.null(input$include_players)) {
      selected_data <- include.people(selected_data, 
                                      get_IDs_for_names(input$include_players, 
                                                        key))
    }
    
    if (!is.null(input$exclude_players)) {
      selected_data <- exclude.people(selected_data, 
                                      get_IDs_for_names(input$exclude_players, 
                                                        key))
    }
    
    output$top_results <- renderDataTable({
      head(selected_data, 100)
    })
  })
}

shinyApp(ui, server)
