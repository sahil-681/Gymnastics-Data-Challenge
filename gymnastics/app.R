library(shiny)
library(dplyr)
library(DT)

men_best <- readRDS("best.teams.mens.rds")
women_best <- readRDS("best.teams.womens.rds")
means_df <- readRDS("means_df.rds")
key <- readRDS("name_ID_key.rds")
source("query.existing.database.R")

ui <- fluidPage(
  titlePanel("Olympic Gymnastics Analysis"),
  navbarPage("Options:",
    tabPanel("Best Team Combinations",
      sidebarLayout(
        sidebarPanel(
          # gender 
          selectInput("gender", "Gender: ", c("Men", "Women")),
          
          # country of interest 
          selectInput("country", "Country: ", choices = NULL, selected = "USA"),
          
          # weight for gold medal
          numericInput("gold", "Gold weight: ", value = 0.6, min = 0),
          
          # weight for silver medal
          numericInput("silver", "Silver weight: ", value = 0.3, min = 0),
          
          # weight for bronze medal
          numericInput("bronze", "Bronze weight: ", value = 0.1, min = 0),
          
          # included players
          selectInput("include_players", "Include Players (up to 5): ", 
                      choices = NULL, multiple = TRUE),
          
          # excluded players
          selectInput("exclude_players", "Exclude Players (up to 5): ", 
                      choices = NULL, multiple = TRUE),
          
          # Submit button
          actionButton("submit_btn", "Submit")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Top Performing Combinations", DTOutput("top_results"))
          )
        )
      )
    ),
    
    
    tabPanel("Run Men Custom Simulation",
        
        selectizeInput("JPN_athletes", "Japanese Athletes (5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "JPN" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("JPN_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
          conditionalPanel(condition="input.JPN_apps == 1", style = "margin-left: 50px;",
          
            column(6, selectizeInput("JPN_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
            column(6, selectizeInput("JPN_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
            column(6, selectizeInput("JPN_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
            column(6, selectizeInput("JPN_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
            column(6, selectizeInput("JPN_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
            column(6, selectizeInput("JPN_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
          )
        ),
        
        selectizeInput("USA_athletes", "American Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "USA" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("USA_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.USA_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("USA_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("USA_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("USA_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("USA_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("USA_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("USA_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("GBR_athletes", "British Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "GBR" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("GBR_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.GBR_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("GBR_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GBR_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GBR_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GBR_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GBR_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GBR_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("CAN_athletes", "Canadian Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "CAN" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("CAN_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.CAN_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("CAN_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CAN_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CAN_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CAN_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CAN_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CAN_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("GER_athletes", "German Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "GER" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("GER_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.GER_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("GER_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GER_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GER_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GER_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GER_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("GER_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("ITA_athletes", "Italian Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "ITA" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("ITA_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.ITA_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("ITA_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ITA_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ITA_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ITA_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ITA_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ITA_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("SUI_athletes", "Swiss Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "SUI" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("SUI_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.SUI_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("SUI_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("SUI_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("SUI_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("SUI_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("SUI_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("SUI_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("CHN_athletes", "Chinese Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "CHN" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("CHN_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.CHN_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("CHN_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CHN_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CHN_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CHN_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CHN_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("CHN_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("ESP_athletes", "Spanish Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "ESP" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("ESP_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.ESP_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("ESP_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ESP_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ESP_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ESP_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ESP_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("ESP_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("TUR_athletes", "Turkish Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "TUR" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("TUR_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.TUR_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("TUR_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("TUR_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("TUR_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("TUR_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("TUR_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("TUR_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("NED_athletes", "Dutch Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "NED" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("NED_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.NED_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("NED_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("NED_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("NED_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("NED_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("NED_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("NED_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        selectizeInput("UKR_athletes", "Ukrainian Athletes (Choose 5):", choices=na.omit(get_names_for_IDs(means_df$ID[means_df$Country == "UKR" & means_df$Gender == "m"], key)), multiple=TRUE, options = list(maxItems = 5)),
        checkboxInput("UKR_apps", "Custom assign apparatus"),
        fluidRow(class = "bordered-row",
                 conditionalPanel(condition="input.UKR_apps == 1", style = "margin-left: 50px;",
                                  
                                  column(6, selectizeInput("UKR_VT", "Vault (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("UKR_FX", "Floor (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("UKR_HB", "Horizontal Bar (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("UKR_PB", "Parallel Bars (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("UKR_PH", "Pommel Horse (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4))),
                                  column(6, selectizeInput("UKR_SR", "Rings (4)", choices=NULL, multiple=TRUE, options = list(maxItems = 4)))
                 )
        ),
        
        )
    ),
)

server <- function(input, output, session) {
  observe({
    # Update country choices based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "country", 
                        choices = unique(men_best$optimizedteams$Country[men_best$optimizedteams$Gender == "m"]))
    } else {
      updateSelectInput(session, "country", 
                        choices = unique(women_best$optimizedteams$Country[women_best$optimizedteams$Gender == "w"]))
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
  
  # Event handler for the Submit button
  observeEvent(input$submit_btn, {
    # Check the value of input$gender and update the data frame accordingly
    selected_data <- if (input$gender == "Women") {
      women_best$simresults %>%
        filter(Country == input$country)
    } else {
      men_best$simresults %>%
        filter(Country == input$country)
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
    
    selected_data$P1 <- get_names_for_IDs(selected_data$P1, key)
    selected_data$P2 <- get_names_for_IDs(selected_data$P2, key)
    selected_data$P3 <- get_names_for_IDs(selected_data$P3, key)
    selected_data$P4 <- get_names_for_IDs(selected_data$P4, key)
    selected_data$P5 <- get_names_for_IDs(selected_data$P5, key)
    
    if (input$gender == "Men") {
      for (i in 1:nrow(selected_data)) {
        selected_data$VT[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$VT[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$FX[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$FX[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$HB[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$HB[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$PB[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$PB[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$PH[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$PH[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$SR[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$SR[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
      }
      
      selected_data <- selected_data %>%
        select(P1, P2, P3, P4, P5, VT, FX, HB, PB, PH, SR)
      
      # Define columns to display
      columns_to_display <- c("P1", "P2", "P3", "P4", "P5", "VT", "FX", "HB", 
                              "PB", "PH", "SR")
      
      # Define custom column names
      custom_column_names <- c("Athlete 1", "Athlete 2", "Athlete 3", 
                               "Athlete 4", "Athlete 5", "Vault", "Floor Exercise", 
                               "Horizontal Bar", "Parallel Bars", "Pommel Horse",
                               "Still Rings")
    }
    
    if (input$gender == "Women") {
      for (i in 1:nrow(selected_data)) {
        selected_data$VT[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$VT[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$BB[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$BB[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$UB[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$UB[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
        selected_data$FX[i] <- paste(get_names_for_IDs(lapply(strsplit(as.character(selected_data$FX[i]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")
      }
      
      selected_data <- selected_data %>%
        select(P1, P2, P3, P4, P5, VT, BB, UB, FX)
      
      # Define columns to display
      columns_to_display <- c("P1", "P2", "P3", "P4", "P5", 
                              "VT", "BB", "UB", "FX")
      
      # Define custom column names
      custom_column_names <- c("Athlete 1", "Athlete 2", "Athlete 3", 
                               "Athlete 4", "Athlete 5", "Vault", "Balance Beam",
                               "Uneven Bars", "Floor Exercise")
    }
    
    # Rename columns in the dataframe
    colnames(selected_data)[match(columns_to_display, names(selected_data))] <- custom_column_names
    
    output$top_results <- renderDT({
      datatable(selected_data, options = list(pageLength = 5))
    })
  })
  
  #Raymond Stuff
  
  observeEvent(input$JPN_athletes, {
    selected_values <- input$JPN_athletes
    updateSelectizeInput(session, "JPN_VT", choices=selected_values)
    updateSelectizeInput(session, "JPN_FX", choices=selected_values)
    updateSelectizeInput(session, "JPN_HB", choices=selected_values)
    updateSelectizeInput(session, "JPN_PB", choices=selected_values)
    updateSelectizeInput(session, "JPN_PH", choices=selected_values)
    updateSelectizeInput(session, "JPN_SR", choices=selected_values)
    })
  observeEvent(input$USA_athletes, {
    selected_values <- input$USA_athletes
    updateSelectizeInput(session, "USA_VT", choices=selected_values)
    updateSelectizeInput(session, "USA_FX", choices=selected_values)
    updateSelectizeInput(session, "USA_HB", choices=selected_values)
    updateSelectizeInput(session, "USA_PB", choices=selected_values)
    updateSelectizeInput(session, "USA_PH", choices=selected_values)
    updateSelectizeInput(session, "USA_SR", choices=selected_values)
  })
  observeEvent(input$GBR_athletes, {
    selected_values <- input$GBR_athletes
    updateSelectizeInput(session, "GBR_VT", choices=selected_values)
    updateSelectizeInput(session, "GBR_FX", choices=selected_values)
    updateSelectizeInput(session, "GBR_HB", choices=selected_values)
    updateSelectizeInput(session, "GBR_PB", choices=selected_values)
    updateSelectizeInput(session, "GBR_PH", choices=selected_values)
    updateSelectizeInput(session, "GBR_SR", choices=selected_values)
  })
  observeEvent(input$CAN_athletes, {
    selected_values <- input$CAN_athletes
    updateSelectizeInput(session, "CAN_VT", choices=selected_values)
    updateSelectizeInput(session, "CAN_FX", choices=selected_values)
    updateSelectizeInput(session, "CAN_HB", choices=selected_values)
    updateSelectizeInput(session, "CAN_PB", choices=selected_values)
    updateSelectizeInput(session, "CAN_PH", choices=selected_values)
    updateSelectizeInput(session, "CAN_SR", choices=selected_values)
  })
  observeEvent(input$GER_athletes, {
    selected_values <- input$GER_athletes
    updateSelectizeInput(session, "GER_VT", choices=selected_values)
    updateSelectizeInput(session, "GER_FX", choices=selected_values)
    updateSelectizeInput(session, "GER_HB", choices=selected_values)
    updateSelectizeInput(session, "GER_PB", choices=selected_values)
    updateSelectizeInput(session, "GER_PH", choices=selected_values)
    updateSelectizeInput(session, "GER_SR", choices=selected_values)
  })
  observeEvent(input$ITA_athletes, {
    selected_values <- input$ITA_athletes
    updateSelectizeInput(session, "ITA_VT", choices=selected_values)
    updateSelectizeInput(session, "ITA_FX", choices=selected_values)
    updateSelectizeInput(session, "ITA_HB", choices=selected_values)
    updateSelectizeInput(session, "ITA_PB", choices=selected_values)
    updateSelectizeInput(session, "ITA_PH", choices=selected_values)
    updateSelectizeInput(session, "ITA_SR", choices=selected_values)
  })
  observeEvent(input$SUI_athletes, {
    selected_values <- input$SUI_athletes
    updateSelectizeInput(session, "SUI_VT", choices=selected_values)
    updateSelectizeInput(session, "SUI_FX", choices=selected_values)
    updateSelectizeInput(session, "SUI_HB", choices=selected_values)
    updateSelectizeInput(session, "SUI_PB", choices=selected_values)
    updateSelectizeInput(session, "SUI_PH", choices=selected_values)
    updateSelectizeInput(session, "SUI_SR", choices=selected_values)
  })
  observeEvent(input$CHN_athletes, {
    selected_values <- input$CHN_athletes
    updateSelectizeInput(session, "CHN_VT", choices=selected_values)
    updateSelectizeInput(session, "CHN_FX", choices=selected_values)
    updateSelectizeInput(session, "CHN_HB", choices=selected_values)
    updateSelectizeInput(session, "CHN_PB", choices=selected_values)
    updateSelectizeInput(session, "CHN_PH", choices=selected_values)
    updateSelectizeInput(session, "CHN_SR", choices=selected_values)
  })
  observeEvent(input$ESP_athletes, {
    selected_values <- input$ESP_athletes
    updateSelectizeInput(session, "ESP_VT", choices=selected_values)
    updateSelectizeInput(session, "ESP_FX", choices=selected_values)
    updateSelectizeInput(session, "ESP_HB", choices=selected_values)
    updateSelectizeInput(session, "ESP_PB", choices=selected_values)
    updateSelectizeInput(session, "ESP_PH", choices=selected_values)
    updateSelectizeInput(session, "ESP_SR", choices=selected_values)
  })
  observeEvent(input$TUR_athletes, {
    selected_values <- input$TUR_athletes
    updateSelectizeInput(session, "TUR_VT", choices=selected_values)
    updateSelectizeInput(session, "TUR_FX", choices=selected_values)
    updateSelectizeInput(session, "TUR_HB", choices=selected_values)
    updateSelectizeInput(session, "TUR_PB", choices=selected_values)
    updateSelectizeInput(session, "TUR_PH", choices=selected_values)
    updateSelectizeInput(session, "TUR_SR", choices=selected_values)
  })
  observeEvent(input$NED_athletes, {
    selected_values <- input$NED_athletes
    updateSelectizeInput(session, "NED_VT", choices=selected_values)
    updateSelectizeInput(session, "NED_FX", choices=selected_values)
    updateSelectizeInput(session, "NED_HB", choices=selected_values)
    updateSelectizeInput(session, "NED_PB", choices=selected_values)
    updateSelectizeInput(session, "NED_PH", choices=selected_values)
    updateSelectizeInput(session, "NED_SR", choices=selected_values)
  })
  observeEvent(input$UKR_athletes, {
    selected_values <- input$UKR_athletes
    updateSelectizeInput(session, "UKR_VT", choices=selected_values)
    updateSelectizeInput(session, "UKR_FX", choices=selected_values)
    updateSelectizeInput(session, "UKR_HB", choices=selected_values)
    updateSelectizeInput(session, "UKR_PB", choices=selected_values)
    updateSelectizeInput(session, "UKR_PH", choices=selected_values)
    updateSelectizeInput(session, "UKR_SR", choices=selected_values)
  })
  
}


shinyApp(ui, server)
