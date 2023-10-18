library(shiny)
library(dplyr)
library(glue)

men_best <- readRDS("best.teams.mens.rds")
women_best <- readRDS("best.teams.womens.rds")
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

# build_appassn <- function(num, gender){
#   
#   apps <- get_app_gender(gender)
#   
#   output = tagList()
#   for(i in length(apps)){
#     output[[i]] <- column(4, selectizeInput(glue(""), glue(""), choices=NULL, multiple=TRUE, options=list(maxItems=4)))
#   }
# }

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
        column(4, selectizeInput("T1_athletes", glue("{men_countries[1]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T1_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
          conditionalPanel(condition="input.T1_apps == 1", style = "margin-left: 50px;",
                           uiOutput("T1_appbox"),
                           # column(4, selectizeInput("T1_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                           # column(4, selectizeInput("T1_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                           # column(4, selectizeInput("T1_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                           # column(4, selectizeInput("T1_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                           # column(4, selectizeInput("T1_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                           # column(4, selectizeInput("T1_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
          )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T2_athletes", glue("{men_countries[2]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T2_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T2_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T2_appbox"),                                
                                  column(4, selectizeInput("T2_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T2_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T2_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T2_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T2_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T2_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T3_athletes", glue("{men_countries[3]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T3_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T3_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T3_appbox"),                                    
                                  column(4, selectizeInput("T3_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T3_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T3_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T3_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T3_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T3_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T4_athletes", glue("{men_countries[4]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T4_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T4_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T4_appbox"),                                     
                                  column(4, selectizeInput("T4_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T4_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T4_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T4_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T4_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T4_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T5_athletes", glue("{men_countries[5]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T5_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T5_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T5_appbox"),                                     
                                  column(4, selectizeInput("T5_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T5_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T5_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T5_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T5_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T5_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T6_athletes", glue("{men_countries[6]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T6_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T6_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T6_appbox"),                                     
                                  column(4, selectizeInput("T6_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T6_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T6_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T6_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T6_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T6_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T7_athletes", glue("{men_countries[7]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T7_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T7_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T7_appbox"),                                     
                                  column(4, selectizeInput("T7_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T7_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T7_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T7_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T7_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T7_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T8_athletes", glue("{men_countries[8]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T8_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T8_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T8_appbox"),                                     
                                  column(4, selectizeInput("T8_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T8_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T8_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T8_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T8_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T8_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T9_athletes", glue("{men_countries[9]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T9_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T9_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T9_appbox"),                                     
                                  column(4, selectizeInput("T9_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T9_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T9_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T9_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T9_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T9_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T10_athletes", glue("{men_countries[10]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T10_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T10_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T10_appbox"),                                    
                                  column(4, selectizeInput("T10_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T10_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T10_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T10_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T10_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T10_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T11_athletes", glue("{men_countries[11]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T11_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T11_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T11_appbox"),                                     
                                  column(4, selectizeInput("T11_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T11_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T11_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T11_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T11_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T11_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        div(style = "height: 30px;"),
        column(4, selectizeInput("T12_athletes", glue("{men_countries[12]} Athletes (5):"), choices=NULL, multiple=TRUE, options = list(maxItems = 5))),
        column(4, checkboxInput("T12_apps", "Custom assign apparatus")),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.T12_apps == 1", style = "margin-left: 50px;",
                                  # uiOutput("T12_appbox"),                                     
                                  column(4, selectizeInput("T12_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T12_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T12_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T12_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T12_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(4, selectizeInput("T12_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
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
  
  #Raymond Stuff
  
  for(i in 1:1){
    output[[glue("T{i}_appbox")]] <- renderUI({
      apps <- get_app_gender(input$simgender)
      
      selectize_inputs <- lapply(apps, function(app) {
        column(4, selectizeInput(glue("T1_{app}"), glue("{app_fullname[[app]]} (4)"), choices=input$T1_athletes, multiple=TRUE, options = list(maxItems = 4)))
      })
      do.call(tagList, selectize_inputs)
    })
  }
  
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
        choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == countries[i] & means_df$Gender == gender], key))
      )
    }
  })
  
  observeEvent(list(input$simgender, input$T1_athletes, input$T1_apps), {
    apps <- get_app_gender(input$simgender) 
    
    selected_values <- input$T1_athletes
    print(selected_values)
    print("T1 Update triggered")
    for(app in apps){
      # print(input[[glue("T1_{app}")]])
      updateSelectizeInput(session, glue("T1_{app}"), label="Created a new label!", choices=selected_values)
    }
  })
  observeEvent(list(input$simgender, input$T2_athletes, input$T2_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T2_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T2_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T3_athletes, input$T3_apps), {

    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T3_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T3_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T4_athletes, input$T4_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T4_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T4_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T5_athletes, input$T5_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T5_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T5_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T6_athletes, input$T6_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T6_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T6_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T7_athletes, input$T7_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T7_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T7_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T8_athletes, input$T8_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T8_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T8_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T9_athletes, input$T9_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T9_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T9_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T10_athletes, input$T10_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T10_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T10_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T11_athletes, input$T11_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T11_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T11_{app}"), choices=selected_values)      
    }
  })
  observeEvent(list(input$simgender, input$T12_athletes, input$T12_apps), {
    
    apps <- get_app_gender(input$simgender)
    
    selected_values <- input$T12_athletes
    for(app in apps){
      updateSelectizeInput(session, glue("T12_{app}"), choices=selected_values)      
    }
  })
  
}


shinyApp(ui, server)
