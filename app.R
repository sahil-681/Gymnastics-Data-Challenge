library(shiny)
library(dplyr)
library(glue)
library(DT)
library(shinyjs)

source("query.existing.database.R")
source("simulate_medals.R")
source("get_default_assignments.R")
source("run_sims.R")

men_best <- readRDS("data/best.teams.mens.optimized2.rds")
women_best <- readRDS("data/best.teams.womens.optimized2.rds")
optimized_teams <- readRDS("data/optimized_teams.new.rds")
means_df <- readRDS("data/means_df.new.rds")
stddevs_df <- readRDS("data/stddevs_df.new.rds")
sim_players <- readRDS("data/sim.players.rds")

key <- readRDS("data/name_ID_key.new.rds")
alt36m <- readRDS("data/alt36m.rds")
alt36w <- readRDS("data/alt36w.rds")

men_countries <- c("USA", "JPN", "GBR", "CAN", "GER", "ITA", "SUI", "CHN", "ESP", "UKR",  "TUR", "NED")
women_countries <- c("USA", "GBR", "CAN", "BRA", "ITA", "CHN", "JPN", "FRA", "KOR", "AUS", "NED", "ROU")

men_apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
women_apps <- c("VT", "BB", "UB", "FX")

app_fullname <- list(
  "VT" = "Vault",
  "FX" = "Floor Exercise",
  "PB" = "Parallel Bars",
  "PH" = "Pummel Horse",
  "HB" = "Horizontal Bar",
  "SR" = "Still Rings",
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

get_qual36 <- function(g){
  if(g=="Men"){
    return(alt36m)
  }else{
    return(alt36w)
  }
}


ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")  # Link to an external CSS file
  ),
  titlePanel("Paris 2024 Olympics: Gymnastics Team Optimization"),
  navbarPage("Options:",
             tabPanel("Best Team Combinations",
                      HTML("<div class='description'>
              <p><strong>Welcome to the Team Combinations Tab!</strong></p>
              <p>This tab provides the best team combinations based on your selection criteria:</p>
              <ul>
                <li>Custom weights for different medals</li>
                <li>Player inclusion or exclusion preferences</li>
              </ul>
              <p>Simply adjust the settings, and the app will calculate the optimal team for you. The teams displayed on top will be the best combinations, with decreasing performance as you move down the list.</p>
            </div>"),
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     # gender 
                                     selectInput("gender", "Gender: ", c("Women", "Men")),
                                     
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
             
             
             tabPanel("Custom Simulator & Results Visualizer",
                      HTML("<div class='description2'>
              <p><strong>Welcome to the Custom Simulator & Results Visualizer Tab!</strong></p>
              <p>This tab provides detailed insights of winning probabilities using:</p>
              <ul>
                <li>An option to run custom simulations by deciding the athletes for all the teams</li>
                <li>An option to input custom apparatus assignments as well.</li>
                <li>An option to choose the number of simulations to run.</li>
                <li>Visualization options to represent simulation results for Team USA.</li>
              </ul>
              <p>The output includes the following: 
              <ul>
                <li>A table showing the probability of achieving the positions in all the events for all athletes/teams</li>
                <li>Multiple tabs for visualizations generated dynamically based on the simulation outcomes</li>
              </ul>
            </div>"), 
                      fluidRow(
                        column(4, selectInput("simgender", "Gender: ", c("Women", "Men"))),
                        column(4, numericInput("n_sims", "Number of Simulations: ",  value = 10, min = 1, max=1000))
                      ),
                      
                      
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
                          tabPanel("Simulation Results: Probability Table", 
                                   div(id = "loading", class = "loader", 
                                       tags$div(class = "loading-text", "Simulating..."), 
                                       style = "display: none; position: absolute; top: 50%; right: 10px; transform: translate(0%, -50%);"),
                                   DTOutput("sim_results")),
                          tabPanel("Athlete Specific Heatmap",
                                   fluidRow(
                                     column(12, plotOutput("plot1", height = "650px"))
                                   )),
                          tabPanel("Athlete Specific Scatterplot",
                                   fluidRow(
                                     column(12, plotOutput("plot2", height = "650px"))
                                   )),
                          tabPanel("Team Heatmap",
                                   fluidRow(
                                     column(12, plotOutput("plot3", height = "650px"))
                                   ))
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
                        choices = men_countries)
    } else {
      updateSelectInput(session, "country", 
                        choices = women_countries)
    }
  })
  
  observe({
    # Update choices dynamically based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "include_players", 
                        choices = get_names_for_IDs(unique(na.omit(sim_players$ID[sim_players$Gender == "m" & sim_players$Country == input$country])), key))
    } else {
      updateSelectInput(session, "include_players", 
                        choices = get_names_for_IDs(unique(na.omit(sim_players$ID[sim_players$Gender == "w" & sim_players$Country == input$country])), key))
    }
    
    # Update choices dynamically based on selected gender
    if (input$gender == "Men") {
      updateSelectInput(session, "exclude_players", 
                        choices = get_names_for_IDs(unique(na.omit(sim_players$ID[sim_players$Gender == "m" & sim_players$Country == input$country])), key))
    } else {
      updateSelectInput(session, "exclude_players", 
                        choices = get_names_for_IDs(unique(na.omit(sim_players$ID[sim_players$Gender == "w" & sim_players$Country == input$country])), key))
    }
  })
  
  # Event handler for the Submit button
  observeEvent(input$submit_btn, {
    # Check the value of input$gender and update the data frame accordingly
    selected_data <- if (input$gender == "Women") {
      women_best %>%
        filter(Country == input$country)
    } else {
      men_best %>%
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
    
    
    # Apply the function to each column without using loops
    
    if (input$gender == "Men") {
      
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
    } else {
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
    shinyjs::show("loading") 
    
    # get needed context
    apps <- get_gender_app(input$simgender)
    gender <- translate_gender(input$simgender)
    countries <- get_gender_country(input$simgender)
    qual36 <- get_qual36(input$simgender)
    
    long_meanstds <- read.csv("data/long_meanstds.csv")
    
    
    # Parse the selected Players into df
    assigned_list <- list()
    for(i in 1:12){
      if(!input[[glue("T{i}_apps")]]){ # when no custom app assigns, use get_default_assignment
        
        athletes_df <- data.frame(
          "ID" = get_IDs_for_names(input[[glue("T{i}_athletes")]], key),
          "Country" = rep(countries[i], 5),
          "Gender" = rep(gender, 5)
        )
        assigned_list[[i]] <- get_default_assignments(athletes_df, means_df)
        assigned_list[[i]]$Gender <- gender
        
        
      }else{ # parse custom assignment input
        
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
    
    competitors <- bind_rows(assigned_list)
    
    n_sims <- input$n_sims
    sim_results <- list()
    
    for(i in 1:n_sims){
      sim_results[[i]] <- run_sims(competitors, qual36, long_meanstds, gender, do_sampling=T)[[2]] %>% select(-Score)
    }
    
    sim_results <- bind_rows(sim_results)
    sim_results <- sim_results %>% group_by(ID, Country, App, Place) %>% summarize(Count=round(n()/n_sims, 2)) %>% pivot_wider(names_from=Place, values_from=Count)
    sim_results <- sim_results[, c("ID", "Country", "App",
                                   "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth")] %>%
      rename(Name = ID, Apparatus = App)
    sim_results$Name <- get_names_for_IDs(sim_results$Name, key)
    sim_results$Gender <- input$simgender
    simres <- sim_results
    sim_results = sim_results %>%
      select(-Gender)
    #saveRDS(simres, "simrestest.rds")
    simres <- simres %>%
      mutate(Name = ifelse(is.na(Name), Country, Name))
    
    if(simres$Gender[1] == "Women"){
      apparatus_mapping <- c("VT" = "Vault",
                             "UB" = "Uneven Bars",
                             "BB" = "Balance Beam",
                             "FX" = "Floor Exercise",
                             "AA" = "All-Around",
                             "Team" = "Team")
    }else{
      apparatus_mapping <- c("VT" = "Vault",
                             "PB" = "Parallel Bars",
                             "HB" = "High Bar",
                             "SR" = "Still Rings",
                             "PH" = "Pommel Horse",
                             "FX" = "Floor Exercise",
                             "AA" = "All-Around",
                             "Team" = "Team")
    }
    
    gender_suffix <- ifelse(simres$Gender[1] == "Men", "Men's", "Women's")
    gender <- ifelse(simres$Gender[1] == "Men", "m", "w")
    
    # Fill NA values with 0
    simres <- simres %>%
      replace_na(list(First = 0, Second = 0, Third = 0, Fourth = 0, Fifth = 0,
                      Sixth = 0, Seventh = 0, Eighth = 0)) %>%
      rename(
        Gold = First,
        Silver = Second,
        Bronze = Third
      )
    
    simres <- simres %>%
      mutate(Apparatus = apparatus_mapping[Apparatus])
    
    # Filter the data to include only the athletes with probabilities in first, second, and third positions
    simres_filtered <- simres %>%
      filter(Country %in% c("USA")) %>%
      select(Name, Country, Apparatus, Gold, Silver, Bronze) %>%
      pivot_longer(cols = c(Gold, Silver, Bronze), names_to = "Position", values_to = "Probability") 
    
    # Reorder levels of Position variable
    simres_filtered$Position <- factor(simres_filtered$Position, levels = c("Gold", "Silver", "Bronze"))
    
    if(gender == "m") {
      simres_filtered$Apparatus <- factor(simres_filtered$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Pommel Horse", "Still Rings", "Parallel Bars", "High Bar"))
    } else {
      simres_filtered$Apparatus <- factor(simres_filtered$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Balance Beam", "Uneven Bars"))
    }
    
    # Reorder levels of Position variable
    simres_g1 <- data.frame(simres_filtered) # makes a copy
    simres_g1 <- data.frame(lapply(simres_g1, function(x) gsub("Gold", "G", x)))
    simres_g1 <- data.frame(lapply(simres_g1, function(x) gsub("Silver", "S", x)))
    simres_g1 <- data.frame(lapply(simres_g1, function(x) gsub("Bronze", "B", x)))
    simres_g1$Position <- factor(simres_g1$Position, levels = c("G", "S", "B"))
    simres_g1$Probability <- as.numeric(simres_g1$Probability)
    
    if(gender == "m") {
      simres_g1$Apparatus <- factor(simres_g1$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Pommel Horse", "Still Rings", "Parallel Bars", "High Bar"))
    } else {
      simres_g1$Apparatus <- factor(simres_g1$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Balance Beam", "Uneven Bars"))
    }
    
    usa_data <- simres %>% filter(Country == "USA")
    # Replace NA with 0 in the dataset
    usa_data[is.na(usa_data)] <- 0
    
    # Calculate the probability of winning in each apparatus
    usa_probabilities <- usa_data %>%
      group_by(Apparatus) %>%
      summarise(
        Gold = sum(Gold, na.rm = TRUE),
        Silver = sum(Silver, na.rm = TRUE),
        Bronze = sum(Bronze, na.rm = TRUE)
      )
    
    # Reshape the data for plotting
    usa_probabilities_long <- usa_probabilities %>%
      pivot_longer(cols = c(Gold, Silver, Bronze),
                   names_to = "Medal",
                   values_to = "Probability")
    
    usa_probabilities_long$Medal <- factor(usa_probabilities_long$Medal, levels = c("Bronze", "Silver", "Gold"))
    
    if(gender == "m") {
      usa_probabilities_long$Apparatus <- factor(usa_probabilities_long$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Pommel Horse", "Still Rings", "Parallel Bars", "High Bar"))
    } else {
      usa_probabilities_long$Apparatus <- factor(usa_probabilities_long$Apparatus, levels = c("Team", "All-Around", "Vault", "Floor Exercise", "Balance Beam", "Uneven Bars"))
    }
    
    output$plot1 <- renderPlot({
      # Heatmap with larger gradient for probability and probability labels
      ggplot(simres_filtered, aes(x = Position, y = Name, fill = Probability)) +
        geom_tile() +
        geom_text(aes(label = scales::percent(Probability, accuracy = 0.1)), color = "black", size = 5) +  # Add text labels for probability
        scale_fill_gradient(low = "white", high = "#9B00FF", limits = c(0, 1), guide = guide_colorbar(barwidth = 15)) +  # Adjust gradient for probability
        facet_wrap(~Apparatus, scales = "free") +
        labs(title = paste("Team USA", gender_suffix, "Medal Positions per Athlete and Apparatus"),
             x = "Position",
             y = "Athlete",
             fill = "Probability") +
        pubtheme::theme_pub()   # Adjust legend position
    })
    
    output$plot2 <- renderPlot({
      # Plot the data with pubtheme
      ggplot(simres_g1, aes(x = Position, y = Probability, fill = , color = Name)) +
        geom_point(size = 5, alpha=0.6, position = position_jitter(width = 0.2, height=0)) +
        facet_wrap(~Apparatus, ncol=8) +
        labs(title = paste("Team USA", gender_suffix, "Medalling Probabilities per Athlete"),
             x = "Medal",
             y = "Probability",
             color = "Athlete") +
        pubtheme::theme_pub() +  # Apply the pubtheme
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(14)) +
        ylim(0, 1)
    })
    
    output$plot3 <- renderPlot({
      # Plot heatmap with annotations
      ggplot(usa_probabilities_long, aes(x = Apparatus, y = Medal, fill = Probability, label = paste0(round(Probability * 100, 1), "%"))) +
        geom_tile(color = "white") +
        geom_text(color = "black", size = 5) +  # Add text annotations
        scale_fill_gradient(low = "white", high = "#9B00FF", limits = c(0, 1), guide = guide_colorbar(barwidth = 15)) +
        labs(title = paste("Team USA", gender_suffix, "Medal Probability by Event"),
             x = "Events",
             y = "Medal",
             fill = "Probability") +
        pubtheme::theme_pub() +  # Apply the pubtheme
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    })
    
    shinyjs::hide("loading")
    output$sim_results <- renderDT({
      datatable(sim_results, options = list(pageLength = 50))
    })
  })
}


shinyApp(ui, server)
