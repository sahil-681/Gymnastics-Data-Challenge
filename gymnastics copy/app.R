library(shiny)
library(dplyr)
library(glue)

men_best <- readRDS("best.teams.mens.rds")
women_best <- readRDS("best.teams.womens.rds")
optimized_teams <- readRDS("optimized_teams.rds")
means_df <- readRDS("means_df.rds")
key <- readRDS("name_ID_key.rds")
men_countries <- c("JPN", "USA", "GBR", "CAN", "GER", "ITA", "SUI", "CHN", "ESP", "BRA",  "KOR", "FRA")
women_countries <- c("USA", "GBR", "CAN", "BRA", "ITA", "CHN", "JPN", "FRA", "ROC", "AUS", "GER", "BEL")
men_apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
women_apps <- c("VT", "BB", "UB", "FX")

app_fullname <- list(
  "VT" = "Vault",
  "FX" = "Floor Exercise",
  "PB" = "Parallel Bars",
  "PH" = "Pummel Horse",
  "HB" = "Horizontal Bar",
  "SR" = "Rings",
  "BB" = "Balance Beam",
  "UB" = "Uneven Bars"
)

source("query.existing.database.R")

get_app_gender <- function(input){
  if(input=="Men"){
    return(men_apps)
  }else{
    return(women_apps)
  }
}


ui <- fluidPage(
  titlePanel("Olympic Gymnastics Analysis"),
  navbarPage("Navbar Title",
    tabPanel("Filter Existing Data",
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
    ),
    
    
    tabPanel("Run Custom Simulation",
        
        selectInput("simgender", "Gender: ", c("Men", "Women")),
        
        div(style = "height: 50px;"),
        fluidRow(
          column(7, selectizeInput("T1_athletes",
                                   glue("{men_countries[1]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T1_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
          conditionalPanel(condition="input.T1_apps == 1", style = "margin-left: 50px;",
                           uiOutput("T1_appbox")
          )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T2_athletes",
                                   glue("{men_countries[2]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T2_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
          conditionalPanel(condition="input.T2_apps == 1", style = "margin-left: 50px;",
                          uiOutput("T2_appbox")
          )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T3_athletes",
                                   glue("{men_countries[3]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T3_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
          conditionalPanel(condition="input.T3_apps == 1", style = "margin-left: 50px;",
                          uiOutput("T3_appbox")
          
          )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T4_athletes",
                                   glue("{men_countries[4]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T4_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T4_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T4_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T5_athletes",
                                   glue("{men_countries[5]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T5_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T5_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T5_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T6_athletes",
                                   glue("{men_countries[6]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T6_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T6_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T6_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T7_athletes",
                                   glue("{men_countries[7]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T7_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T7_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T7_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T8_athletes",
                                   glue("{men_countries[8]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T8_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T8_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T8_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T9_athletes", glue("{men_countries[9]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T9_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T9_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T9_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T10_athletes",
                                   glue("{men_countries[10]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T10_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T10_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T10_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T11_athletes",
                                   glue("{men_countries[11]} Athletes (5):"),
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T11_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T11_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T11_appbox")
                 )
        ),
        
        div(style = "height: 30px;"),
        fluidRow(
          column(7, selectizeInput("T12_athletes",
                                   glue("{men_countries[12]} Athletes (5):"), 
                                   choices=NULL, multiple=TRUE,
                                   options = list(maxItems = 5),
                                   width = "600px")),
          column(4, checkboxInput("T12_apps", "Custom assign apparatus"))
        ),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T12_apps == 1", style = "margin-left: 50px;",
                                  uiOutput("T12_appbox")
                 )
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
  

  # initialize all of the optional input boxes (for assigning to apps)
  lapply(1:12, function(i){
    
    output[[glue("T{i}_appbox")]] <- renderUI({
      apps <- get_app_gender(input$simgender)
      selectize_inputs <- lapply(apps, function(app) {
        column(4,
               selectizeInput(glue("T{i}_{app}"),
                              glue("{app_fullname[[app]]} (4)"),
                              choices= input[[glue("T{i}_athletes")]],
                              multiple=TRUE,
                              options = list(maxItems = 4)))
      })
      do.call(tagList, selectize_inputs)
    })
    
  })
  
  
  # dynamically change top-12 countries when switching between men and women 
  observeEvent(input$simgender, {
    if(input$simgender == "Men"){
      gender <- "m"
      countries <- men_countries
      apps <- men_apps
    }
    else{
      gender <- "w"
      countries <- women_countries
      apps <- women_apps
    }
    
    for(i in 1:length(countries)){
      updateSelectizeInput(
        session, glue("T{i}_athletes"), 
        label=glue("{countries[i]} Athletes (5):"),
        choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == countries[i] & means_df$Gender == gender], key)),
        selected=na.omit(get_names_for_IDs(optimized_teams$ID[optimized_teams$Country == countries[i] & optimized_teams$Gender == gender], key))
      )
    }
  })
  
  
  # dynamically change options for app assignment to only allow selected athletes
  for(i in 1:12){
    
    observeEvent(list(input$simgender, input[[glue("T{i}_athletes")]], input[[glue("T{i}_apps")]]), {
      
      apps <- get_app_gender(input$simgender) 
      selected_values <- input[[glue("T{i}_athletes")]]
      
      for(app in apps){
        updateSelectizeInput(session, glue("T{i}_{app}"), choices=selected_values)
      }
      
    })
  }
}



shinyApp(ui, server)
