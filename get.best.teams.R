# for each coubntry x, randomly sample all subsets from that country's reasonable set
# for each subset, replace the current teams data to replace the currect X country's team
# call simulate_medals (returns medalling_output table) for each subset and calculate their weighted medal score
# calculate the weighted medaling score is 5-G, 3-S, 1-B
# choose the team with the highest medaling score (if new medaling score higher than current medaling score then replace the current team with new team)
# at the end edit the real list of top country athletes and replace the best team for each country
# add weighting set as argument (50, 30, 15, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)


means = read.csv("data/means_per_app.csv")
stddevs = read.csv("data/stddevs_per_app.csv")
startteams = read.csv("data/team_country_qualified_individuals.csv")
alt36 <- read.csv("data/mens_36_athletes.csv")

#x <- get_reasonable_sets(country = "USA", gender = "m", means_df = means_df, 
#                    stddevs_df = stddevs_df, top_n = 5)

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
  interested_sims <- data.frame(P1 = character(0),
                                P2 = character(0),
                                P3 = character(0),
                                P4 = character(0),
                                P5 = character(0),
                                Gold = numeric(0),
                                Silver = numeric(0),
                                Bronze = numeric(0),
                                TotalScore = numeric(0),
                                MedalCount = numeric(0))
  
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
    
    # if (ctry == ctry_interest) {
    #   interested_sims$P1[l] <- combinations[[ctry_interest]][l][[1]][1]
    #   interested_sims$P2[l] <- combinations[[ctry_interest]][l][[1]][2]
    #   interested_sims$P3[l] <- combinations[[ctry_interest]][l][[1]][3]
    #   interested_sims$P4[l] <- combinations[[ctry_interest]][l][[1]][4]
    #   interested_sims$P5[l] <- combinations[[ctry_interest]][l][[1]][5]
    #   interested_sims$Gold[l] <- sim$Firsts[sim$Country == ctry_interest]
    #   interested_sims$Silver[l] <- sim$Seconds[sim$Country == ctry_interest]
    #   interested_sims$Bronze[l] <- sim$Thirds[sim$Country == ctry_interest]
    #   interested_sims$TotalScore[l] <- sim$Score[sim$Country == ctry_interest]
    #   interested_sims$MedalCount[l] <- sim$Firsts[[sim$Country == ctry_interest]] + sim$Seconds[[sim$Country == ctry_interest]]
    # }
  }
  return(list(finalteams = start_teams, interestedteam = interested_sims))
}

z <- get.best.teams(start_teams = startteams, 
                    countries = unique(startteams$Country[startteams$Gender == "m"]),
                    gender = "m",
                    means, 
                    stddevs, alt36, ctry_interest = "USA")

saveRDS(z, file = "best.teams.rds")

