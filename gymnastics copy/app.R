library(shiny)
library(dplyr)
library(glue)

source("query.existing.database.R")
source("simulate_medals.R")
source("get_default_assignments.R")
source("run_sims.R")

men_best <- readRDS("best.teams.mens.rds")
women_best <- readRDS("best.teams.womens.rds")
optimized_teams <- readRDS("optimized_teams.rds")
means_df <- readRDS("means_df.rds")
stddevs_df <- readRDS("stddevs.rds")

key <- readRDS("name_ID_key.rds")
alt36m <- readRDS("alt36m.rds")
alt36w <- readRDS("alt36w.rds")

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

long_means <- means_df %>% pivot_longer(cols=unique(c(men_apps, women_apps)), names_to="App", values_to="Mean") 

get_gender_app <- function(input){
  if(input=="Men"){
    return(men_apps)
  }else{
    return(women_apps)
  }
}

translate_gender <- function(input){
  if(input=="Men"){
    return("m")
  }else{
    return("w")
  }
}

get_gender_country <- function(input){
  if(input=="Men"){
    return(men_countries)
  }else{
    return(women_countries)
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
        ),
        
        # Submit button
        actionButton("submit_btn2", "Submit"),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Simulation Results", dataTableOutput("sim_results"))
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
      apps <- get_gender_app(input$simgender)
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
      
      apps <- get_gender_app(input$simgender) 
      selected_values <- input[[glue("T{i}_athletes")]]
      
      for(app in apps){
        updateSelectizeInput(session, glue("T{i}_{app}"), choices=selected_values)
      }
      
    })
  }
  
  observeEvent(input$submit_btn2, {
    
    # get needed context
    apps <- get_gender_app(input$simgender)
    gender <- translate_gender(input$simgender)
    countries <- get_gender_country(input$simgender)
    
    # Parse the selected Players into df
    assigned_list <- list()
    for(i in 1:12){
      if(!input[[glue("T{i}_apps")]]){ # use get_default_assignment when no custom app assignments
        
        athletes_df <- data.frame(
          "ID" = get_IDs_for_names(input[[glue("T{i}_athletes")]], key),
          "Country" = rep(countries[i], 5),
          "Gender" = rep(gender, 5)
        )
        
        assigned_list[[i]] <- get_default_assignments(athletes_df, means_df)
        
      }else{ # parse custom assignment input

        assigned_list[[i]] <- rbind()

        temp_list <- list()
        for(j in 1:length(apps)){
          temp_list[[j]] <- data.frame(
            "ID" = get_IDs_for_names(input[[glue("T{i}_{apps[j]}")]], key),
            "Country" = countries[i],
            "Gender" = gender,
            "App" = apps[j]
          )
        }
        
        temp_df <- bind_rows(temp_list)
        assigned_list[[i]] <- merge(temp_df, long_means, all.x = T)
        
      }
    }
    assigned_competitors <- bind_rows(assigned_list)
    output$sim_results <- renderDataTable({assigned_competitors})
    
    # Call Run Sims
    # sim_results <- 
    
  })
  
  # Event handler for the Submit button 2
  # observeEvent(input$submit_btn2, {
  #   
  #   print('in button!!')
  #   
  #   all_athletes <- c(input$T1_athletes,  input$T2_athletes,  input$T3_athletes,  
  #                     input$T4_athletes,  input$T5_athletes,  input$T6_athletes,  
  #                     input$T7_athletes,  input$T8_athletes,  input$T9_athletes, 
  #                     input$T10_athletes, input$T11_athletes, input$T12_athletes)
  # 
  #   get_mean <- function(app, df2) {
  #     app_col <- grep(app, colnames(df2))
  #     if (length(app_col) == 0) return(NA)
  #     mean(df2[, app_col])
  #   }
  #   
  #   if (input$simgender == "Men") {
  #     teams <- data.frame(
  #       ID = get_IDs_for_names(all_athletes, key),
  #       Country = rep(men_countries, each = 5),  # Repeat each country 5 times
  #       Gender = rep("m", 60)  # Assuming it's for men
  #     )
  #   }
  #   
  #   if (input$simgender == "Women") {
  #     teams <- data.frame(
  #       ID = get_IDs_for_names(all_athletes,key),
  #       Country = rep(women_countries, each = 5),  # Repeat each country 5 times
  #       Gender = rep("w", 60) 
  #     )
  #   }
  #   
  #   # if there are no custom assignments then just simulate directly
  #   if (sum(input$T1_apps, input$T2_apps, input$T3_apps, input$T4_apps, 
  #           input$T5_apps, input$T6_apps, input$T7_apps, input$T8_apps, 
  #           input$T9_apps, input$T10_apps, input$T11_apps, input$T12_apps) == 0) 
  #     {
  #       if (input$simgender == "Men") {
  #         x <- simulate_medals(teams, alt36m, means_df, stddevs_df, "m")
  #       }
  #       if (input$simgender == "Women") {
  #         x <- simulate_medals(teams, alt36w, means_df, stddevs_df, "w")
  #       }
  #     }
  # 
  #   # if there are custom assignments
  #   if (sum(input$T1_apps, input$T2_apps, input$T3_apps, input$T4_apps,
  #           input$T5_apps, input$T6_apps, input$T7_apps, input$T8_apps,
  #           input$T9_apps, input$T10_apps, input$T11_apps, input$T12_apps) != 0)
  #     {
  #       if (input$simgender == "Men") {
  #           if (input$T1_apps == 0) {
  #             T1_assignments <- get_default_assignments(get_IDs_for_names(input$T1_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T1_assignments <- data.frame(ID = c(get_IDs_for_names(input$T1_VT, key),
  #                                                 get_IDs_for_names(input$T1_FX, key),
  #                                                 get_IDs_for_names(input$T1_HB, key),
  #                                                 get_IDs_for_names(input$T1_PB, key),
  #                                                 get_IDs_for_names(input$T1_PH, key),
  #                                                 get_IDs_for_names(input$T1_SR, key)),
  #                                          Country = rep(men_countries[1], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T2_apps == 0) {
  #             T2_assignments <- get_default_assignments(get_IDs_for_names(input$T2_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T2_assignments <- data.frame(ID = c(get_IDs_for_names(input$T2_VT, key),
  #                                                 get_IDs_for_names(input$T2_FX, key),
  #                                                 get_IDs_for_names(input$T2_HB, key),
  #                                                 get_IDs_for_names(input$T2_PB, key),
  #                                                 get_IDs_for_names(input$T2_PH, key),
  #                                                 get_IDs_for_names(input$T2_SR, key)),
  #                                          Country = rep(men_countries[2], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T3_apps == 0) {
  #             T3_assignments <- get_default_assignments(get_IDs_for_names(input$T3_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T3_assignments <- data.frame(ID = c(get_IDs_for_names(input$T3_VT, key),
  #                                                 get_IDs_for_names(input$T3_FX, key),
  #                                                 get_IDs_for_names(input$T3_HB, key),
  #                                                 get_IDs_for_names(input$T3_PB, key),
  #                                                 get_IDs_for_names(input$T3_PH, key),
  #                                                 get_IDs_for_names(input$T3_SR, key)),
  #                                          Country = rep(men_countries[3], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T4_apps == 0) {
  #             T4_assignments <- get_default_assignments(get_IDs_for_names(input$T4_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T4_assignments <- data.frame(ID = c(get_IDs_for_names(input$T4_VT, key),
  #                                                 get_IDs_for_names(input$T4_FX, key),
  #                                                 get_IDs_for_names(input$T4_HB, key),
  #                                                 get_IDs_for_names(input$T4_PB, key),
  #                                                 get_IDs_for_names(input$T4_PH, key),
  #                                                 get_IDs_for_names(input$T4_SR, key)),
  #                                          Country = rep(men_countries[4], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T5_apps == 0) {
  #             T5_assignments <- get_default_assignments(get_IDs_for_names(input$T5_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T5_assignments <- data.frame(ID = c(get_IDs_for_names(input$T5_VT, key),
  #                                                 get_IDs_for_names(input$T5_FX, key),
  #                                                 get_IDs_for_names(input$T5_HB, key),
  #                                                 get_IDs_for_names(input$T5_PB, key),
  #                                                 get_IDs_for_names(input$T5_PH, key),
  #                                                 get_IDs_for_names(input$T5_SR, key)),
  #                                          Country = rep(men_countries[5], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T6_apps == 0) {
  #             T6_assignments <- get_default_assignments(get_IDs_for_names(input$T6_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T6_assignments <- data.frame(ID = c(get_IDs_for_names(input$T6_VT, key),
  #                                                 get_IDs_for_names(input$T6_FX, key),
  #                                                 get_IDs_for_names(input$T6_HB, key),
  #                                                 get_IDs_for_names(input$T6_PB, key),
  #                                                 get_IDs_for_names(input$T6_PH, key),
  #                                                 get_IDs_for_names(input$T6_SR, key)),
  #                                          Country = rep(men_countries[6], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T7_apps == 0) {
  #             T7_assignments <- get_default_assignments(get_IDs_for_names(input$T7_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T7_assignments <- data.frame(ID = c(get_IDs_for_names(input$T7_VT, key),
  #                                                 get_IDs_for_names(input$T7_FX, key),
  #                                                 get_IDs_for_names(input$T7_HB, key),
  #                                                 get_IDs_for_names(input$T7_PB, key),
  #                                                 get_IDs_for_names(input$T7_PH, key),
  #                                                 get_IDs_for_names(input$T7_SR, key)),
  #                                          Country = rep(men_countries[7], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T8_apps == 0) {
  #             T8_assignments <- get_default_assignments(get_IDs_for_names(input$T8_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T8_assignments <- data.frame(ID = c(get_IDs_for_names(input$T8_VT, key),
  #                                                 get_IDs_for_names(input$T8_FX, key),
  #                                                 get_IDs_for_names(input$T8_HB, key),
  #                                                 get_IDs_for_names(input$T8_PB, key),
  #                                                 get_IDs_for_names(input$T8_PH, key),
  #                                                 get_IDs_for_names(input$T8_SR, key)),
  #                                          Country = rep(men_countries[8], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T9_apps == 0) {
  #             T9_assignments <- get_default_assignments(get_IDs_for_names(input$T9_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T9_assignments <- data.frame(ID = c(get_IDs_for_names(input$T9_VT, key),
  #                                                 get_IDs_for_names(input$T9_FX, key),
  #                                                 get_IDs_for_names(input$T9_HB, key),
  #                                                 get_IDs_for_names(input$T9_PB, key),
  #                                                 get_IDs_for_names(input$T9_PH, key),
  #                                                 get_IDs_for_names(input$T9_SR, key)),
  #                                          Country = rep(men_countries[9], 24),
  #                                          App = rep(men_apps, each = 4))
  #           }
  #           if (input$T10_apps == 0) {
  #             T10_assignments <- get_default_assignments(get_IDs_for_names(input$T10_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T10_assignments <- data.frame(ID = c(get_IDs_for_names(input$T10_VT, key),
  #                                                  get_IDs_for_names(input$T10_FX, key),
  #                                                  get_IDs_for_names(input$T10_HB, key),
  #                                                  get_IDs_for_names(input$T10_PB, key),
  #                                                  get_IDs_for_names(input$T10_PH, key),
  #                                                  get_IDs_for_names(input$T10_SR, key)),
  #                                           Country = rep(men_countries[10], 24),
  #                                           App = rep(men_apps, each = 4))
  #           }
  #           if (input$T11_apps == 0) {
  #             T11_assignments <- get_default_assignments(get_IDs_for_names(input$T11_athletes, key),
  #                                                        means_df)
  #           } else {
  #             T11_assignments <- data.frame(ID = c(get_IDs_for_names(input$T11_VT, key),
  #                                                  get_IDs_for_names(input$T11_FX, key),
  #                                                  get_IDs_for_names(input$T11_HB, key),
  #                                                  get_IDs_for_names(input$T11_PB, key),
  #                                                  get_IDs_for_names(input$T11_PH, key),
  #                                                  get_IDs_for_names(input$T11_SR, key)),
  #                                           Country = rep(men_countries[11], 24),
  #                                           App = rep(men_apps, each = 4))
  #           }
  #           if (input$T12_apps == 0) {
  #             T12_assignments <- get_default_assignments(get_IDs_for_names(input$T12_athletes, key),
  #                                                        means_df)
  #           } else {
  #             T12_assignments <- data.frame(ID = c(get_IDs_for_names(input$T12_VT, key),
  #                                                  get_IDs_for_names(input$T12_FX, key),
  #                                                  get_IDs_for_names(input$T12_HB, key),
  #                                                  get_IDs_for_names(input$T12_PB, key),
  #                                                  get_IDs_for_names(input$T12_PH, key),
  #                                                  get_IDs_for_names(input$T12_SR, key)),
  #                                           Country = rep(men_countries[12], 24),
  #                                           App = rep(men_apps, each = 4))
  #           }
  #         if (input$simgender == "Women") {
  #           if (input$T1_apps == 0) {
  #             T1_assignments <- get_default_assignments(get_IDs_for_names(input$T1_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T1_assignments <- data.frame(ID = c(get_IDs_for_names(input$T1_VT, key),
  #                                                 get_IDs_for_names(input$T1_BB, key),
  #                                                 get_IDs_for_names(input$T1_UB, key),
  #                                                 get_IDs_for_names(input$T1_FX, key)),
  #                                          Country = rep(women_countries[1], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T2_apps == 0) {
  #             T2_assignments <- get_default_assignments(get_IDs_for_names(input$T2_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T2_assignments <- data.frame(ID = c(get_IDs_for_names(input$T2_VT, key),
  #                                                 get_IDs_for_names(input$T2_BB, key),
  #                                                 get_IDs_for_names(input$T2_UB, key),
  #                                                 get_IDs_for_names(input$T2_FX, key)),
  #                                          Country = rep(women_countries[2], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T3_apps == 0) {
  #             T3_assignments <- get_default_assignments(get_IDs_for_names(input$T3_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T3_assignments <- data.frame(ID = c(get_IDs_for_names(input$T3_VT, key),
  #                                                 get_IDs_for_names(input$T3_BB, key),
  #                                                 get_IDs_for_names(input$T3_UB, key),
  #                                                 get_IDs_for_names(input$T3_FX, key)),
  #                                          Country = rep(women_countries[3], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T4_apps == 0) {
  #             T4_assignments <- get_default_assignments(get_IDs_for_names(input$T4_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T4_assignments <- data.frame(ID = c(get_IDs_for_names(input$T4_VT, key),
  #                                                 get_IDs_for_names(input$T4_BB, key),
  #                                                 get_IDs_for_names(input$T4_UB, key),
  #                                                 get_IDs_for_names(input$T4_FX, key)),
  #                                          Country = rep(women_countries[4], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T5_apps == 0) {
  #             T5_assignments <- get_default_assignments(get_IDs_for_names(input$T5_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T5_assignments <- data.frame(ID = c(get_IDs_for_names(input$T5_VT, key),
  #                                                 get_IDs_for_names(input$T5_BB, key),
  #                                                 get_IDs_for_names(input$T5_UB, key),
  #                                                 get_IDs_for_names(input$T5_FX, key)),
  #                                          Country = rep(women_countries[5], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T6_apps == 0) {
  #             T6_assignments <- get_default_assignments(get_IDs_for_names(input$T6_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T6_assignments <- data.frame(ID = c(get_IDs_for_names(input$T6_VT, key),
  #                                                 get_IDs_for_names(input$T6_BB, key),
  #                                                 get_IDs_for_names(input$T6_UB, key),
  #                                                 get_IDs_for_names(input$T6_FX, key)),
  #                                          Country = rep(women_countries[6], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T7_apps == 0) {
  #             T7_assignments <- get_default_assignments(get_IDs_for_names(input$T7_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T7_assignments <- data.frame(ID = c(get_IDs_for_names(input$T7_VT, key),
  #                                                 get_IDs_for_names(input$T7_BB, key),
  #                                                 get_IDs_for_names(input$T7_UB, key),
  #                                                 get_IDs_for_names(input$T7_FX, key)),
  #                                          Country = rep(women_countries[7], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T8_apps == 0) {
  #             T8_assignments <- get_default_assignments(get_IDs_for_names(input$T8_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T8_assignments <- data.frame(ID = c(get_IDs_for_names(input$T8_VT, key),
  #                                                 get_IDs_for_names(input$T8_BB, key),
  #                                                 get_IDs_for_names(input$T8_UB, key),
  #                                                 get_IDs_for_names(input$T8_FX, key)),
  #                                          Country = rep(women_countries[8], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T9_apps == 0) {
  #             T9_assignments <- get_default_assignments(get_IDs_for_names(input$T9_athletes, key),
  #                                                       means_df)
  #           } else {
  #             T9_assignments <- data.frame(ID = c(get_IDs_for_names(input$T9_VT, key),
  #                                                 get_IDs_for_names(input$T9_BB, key),
  #                                                 get_IDs_for_names(input$T9_UB, key),
  #                                                 get_IDs_for_names(input$T9_FX, key)),
  #                                          Country = rep(women_countries[9], 16),
  #                                          App = rep(women_apps, each = 4))
  #           }
  #           if (input$T10_apps == 0) {
  #             T10_assignments <- get_default_assignments(get_IDs_for_names(input$T10_athletes, key),
  #                                                        means_df)
  #           } else {
  #             T10_assignments <- data.frame(ID = c(get_IDs_for_names(input$T10_VT, key),
  #                                                  get_IDs_for_names(input$T10_BB, key),
  #                                                  get_IDs_for_names(input$T10_UB, key),
  #                                                  get_IDs_for_names(input$T10_FX, key)),
  #                                           Country = rep(women_countries[10], 16),
  #                                           App = rep(women_apps, each = 4))
  #           }
  #           if (input$T11_apps == 0) {
  #             T11_assignments <- get_default_assignments(get_IDs_for_names(input$T11_athletes, key),
  #                                                        means_df)
  #           } else {
  #             T11_assignments <- data.frame(ID = c(get_IDs_for_names(input$T11_VT, key),
  #                                                  get_IDs_for_names(input$T11_BB, key),
  #                                                  get_IDs_for_names(input$T11_UB, key),
  #                                                  get_IDs_for_names(input$T11_FX, key)),
  #                                           Country = rep(women_countries[11], 16),
  #                                           App = rep(women_apps, each = 4))
  #           }
  #           if (input$T12_apps == 0) {
  #             T12_assignments <- get_default_assignments(get_IDs_for_names(input$T12_athletes, key),
  #                                                        means_df)
  #           } else {
  #             T12_assignments <- data.frame(ID = c(get_IDs_for_names(input$T12_VT, key),
  #                                                  get_IDs_for_names(input$T12_BB, key),
  #                                                  get_IDs_for_names(input$T12_UB, key),
  #                                                  get_IDs_for_names(input$T12_FX, key)),
  #                                           Country = rep(women_countries[12], 16),
  #                                           App = rep(women_apps, each = 4))
  #           }
  #     }
  # 
  #     }
  #     }
  #   custom_assignment_men <- function(VT, FX, HB, PB, PH, SR, key, country) {
  #    df <- data.frame(ID = c(get_IDs_for_names(VT, key), 
  #                             get_IDs_for_names(FX, key), 
  #                             get_IDs_for_names(HB, key), 
  #                             get_IDs_for_names(PB, key), 
  #                             get_IDs_for_names(PH, key), 
  #                             get_IDs_for_names(SR, key)), 
  #                      Country = rep(country, 24),
  #                      App = rep(apps, each = 4))
  #    return(df)
  #   }
  #   
  #   custom_assignment_women <- function(VT, BB, UB, FX, key, country) {
  #     df <- data.frame(ID = c(get_IDs_for_names(VT, key), 
  #                             get_IDs_for_names(BB, key), 
  #                             get_IDs_for_names(UB, key), 
  #                             get_IDs_for_names(FX, key)),
  #                     Country = rep(country, 16),
  #                     App = rep(apps, each = 4))
  #     return(df)
  #   }
  #   print(input$T1_VT)
  #     
  #   output$sim_results <- renderDataTable({
  #     
  #     x$medal_details
  #   })
  # })
  
}


shinyApp(ui, server)
