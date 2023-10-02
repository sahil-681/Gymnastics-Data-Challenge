means = read.csv("data/means_per_app.csv")
stddevs = read.csv("data/stddevs_per_app.csv")
startteams = read.csv("data/team_country_qualified_individuals.csv")
alt36 <- read.csv("data/mens_36_athletes.csv")
alt36w <- read.csv("data/womens_36_athletes.csv")

#### helper functions:

## get reasonable set
source('get_reasonable_set.R')

## simulate medals
source('simulate_medals.R')

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

# helper function to select players
select_players <- function(players) {
  selected_players <- sample(players, 4)
  return(paste(selected_players, collapse = ", "))
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
    
    if (l %% 50 == 0) {
      print(paste("Simulation:", l, "of", length(combcountry2), "for", ctry_interest))
    }
    
    if (l == length(combcountry2)) {
      print(paste("Simulation:", l, "of", length(combcountry2), "for", ctry_interest))
    }
    
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

# mens (takes close to 2 hours to run)
men_best <- get.best.teams(start_teams = startteams, 
                    countries = unique(startteams$Country[startteams$Gender == "m"]),
                    gender = "m",
                    means, 
                    stddevs, alt36, ctry_interest = "USA")

# creating apparatus-athlete combinations for country of interest
## mens
men_best$interestedteam$VT <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
men_best$interestedteam$FX <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
men_best$interestedteam$HB <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
men_best$interestedteam$PB <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
men_best$interestedteam$PH <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
men_best$interestedteam$SR <- apply(men_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)

men_best$interestedteam <- men_best$interestedteam[order(-men_best$interestedteam$TotalScore), ]

# womens (takes close to 2 hours to run)
women_best <- get.best.teams(start_teams = startteams, 
                    countries = unique(startteams$Country[startteams$Gender == "w"]),
                    gender = "w",
                    means, 
                    stddevs, alt36w, ctry_interest = "USA")

# creating apparatus-athlete combinations for country of interest
## womens
women_best$interestedteam$VT <- apply(women_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
women_best$interestedteam$BB <- apply(women_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
women_best$interestedteam$UB <- apply(women_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)
women_best$interestedteam$FX <- apply(women_best$interestedteam[, c("P1", "P2", "P3", "P4", "P5")], 1, random_select)

women_best$interestedteam <- women_best$interestedteam[order(-women_best$interestedteam$TotalScore), ]

# saved
saveRDS(men_best, file = "data/best.teams.mens.rds")
saveRDS(women_best, file = "data/best.teams.womens.rds")

# saving data as csv
write.csv(women_best$finalteams, "data/optimized.womens.teams.csv", row.names = FALSE)
write.csv(men_best$finalteams, "data/optimized.mens.teams.csv", row.names = FALSE)
write.csv(women_best$interestedteam, "data/best.womens.teams.country.of.interest.csv", row.names = FALSE)
write.csv(men_best$interestedteam, "data/best.mens.teams.country.of.interest.csv", row.names = FALSE)

