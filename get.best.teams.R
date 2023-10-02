means = read.csv("data/means_per_app.csv")
stddevs = read.csv("data/stddevs_per_app.csv")
startteams = read.csv("data/team_country_qualified_individuals.csv")
alt36 <- read.csv("data/mens_36_athletes.csv")

# helper function for replacing temp players for each combination
replace_players <- function(start_teams, country, combinations, gender, i = 1) {
  start_teams[start_teams$Country == country & start_teams$Gender == gender, 
              "ID"] <- combinations[[country]][i]
  return(start_teams)
}

# helper function to get medaling scores for each country
medalscore <- function(sim_output, weights = c(50, 30, 15, 10, 9, 8, 7, 6)){
  scores <- apply(sim_output[, -1], 1, function(row) sum(row * weights))
  sim_output$Score <- scores
  sim_output <- sim_output[order(sim_output$Score, decreasing = TRUE), ]
  return(sim_output)
}

# main function for best teams
get.best.teams <- function(start_teams, countries, gender, means_df, 
                           stddevs_df, qual36, 
                           weights = c(50, 30, 15, 10, 9, 8, 7, 6),
                           ctry_interest = "USA"){
  reasonable_set <- list()
  combinations <- list()
  interested_sims <- data.frame(P1 = character(),
                                P2 = character(),
                                P3 = character(),
                                P4 = character(),
                                P5 = character(),
                                Gold = numeric(),
                                Silver = numeric(),
                                Bronze = numeric(),
                                TotalScore = numeric(),
                                MedalCount = numeric())
  
  for (i in 1:length(countries)) {
    temp <- get_reasonable_sets(country = countries[i], gender = gender, 
                        means_df = means_df, stddevs_df = stddevs_df, top_n = 5)
    reasonable_set[[countries[i]]] <- temp
    combinations[[countries[i]]] <- combn(temp, 5, simplify = F)
  }
    
  for (j in 1:length(combinations)) {
    combcountry <- combinations[[j]]
    ctry <- names(combinations[j])
    top_score <- 0
    
    print(paste("Iteration:", j, "of", (length(combinations)+1), "for", names(combinations[j])))
    
    for (k in 1:length(combcountry)) {
      starttemp <- replace_players(start_teams, ctry, combinations, gender, k)
      sim <- simulate_medals(starttemp, qual36, means_df, stddevs_df, gender)
      sim <- medalscore(sim, weights)
      
      if (k %% 50 == 0) {
        print(paste("Simulation:", k, "of", length(combcountry), "for", names(combinations[j])))
      }
      
      if (k == length(combcountry)) {
        print(paste("Simulation:", k, "of", length(combcountry), "for", names(combinations[j])))
      }
      
      if (sim$Score[sim$Country == ctry] > top_score) {
        top_score <- sim$Score[sim$Country == ctry]
        start_teams <- replace_players(start_teams, ctry, combinations, gender, k)
      }
      
    }
  }
  
  combcountry2 <- combinations[[ctry_interest]]
  ctry <- ctry_interest
  top_score <- 0
  
  print(paste("Iteration:", (length(combinations)+1), "of", (length(combinations)+1), "for", ctry_interest))
  
  for (l in 1:length(combcountry2)) {
    starttemp <- replace_players(start_teams, ctry, combinations, gender, l)
    sim <- simulate_medals(starttemp, qual36, means_df, stddevs_df, gender)
    sim <- medalscore(sim, weights)
    
    if (sim$Score[sim$Country == ctry] > top_score) {
      top_score <- sim$Score[sim$Country == ctry]
      start_teams <- replace_players(start_teams, ctry, combinations, gender, l)
    }
    
    if (ctry == ctry_interest) {
      interested_sims[l, "P1"] <- combinations[[ctry_interest]][l][[1]][1]
      interested_sims[l, "P2"] <- combinations[[ctry_interest]][l][[1]][2]
      interested_sims[l, "P3"] <- combinations[[ctry_interest]][l][[1]][3]
      interested_sims[l, "P4"] <- combinations[[ctry_interest]][l][[1]][4]
      interested_sims[l, "P5"] <- combinations[[ctry_interest]][l][[1]][5]
      interested_sims[l, "Gold"] <- sim$Firsts[sim$Country == ctry_interest]
      interested_sims[l, "Silver"] <- sim$Seconds[sim$Country == ctry_interest]
      interested_sims[l, "Bronze"] <- sim$Thirds[sim$Country == ctry_interest]
      interested_sims[l, "TotalScore"] <- sim$Score[sim$Country == ctry_interest]
      interested_sims[l, "MedalCount"] <- sim$Firsts[sim$Country == ctry_interest] + sim$Seconds[sim$Country == ctry_interest]
    }
  }
  return(list(finalteams = start_teams, interestedteam = interested_sims))
}

# example (takes close to 2 hours to run)
z <- get.best.teams(start_teams = startteams, 
                    countries = unique(startteams$Country[startteams$Gender == "m"]),
                    gender = "m",
                    means, 
                    stddevs, alt36, ctry_interest = "USA")

# saved
saveRDS(z, file = "data/best.teams.rds")

