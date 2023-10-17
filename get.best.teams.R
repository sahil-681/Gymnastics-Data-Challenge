library(dplyr)
library(tidyr)

means = read.csv("data/means_per_app.csv")
stddevs = read.csv("data/stddevs_per_app.csv")
startteams = read.csv("data/team_country_qualified_individuals.csv")
alt36m <- read.csv("data/mens_36_athletes.csv")
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


# main function for best teams
get.best.teams <- function(start_teams, countries, gender, means_df, 
                           stddevs_df, qual36, 
                           weights = c(50, 30, 15, 10, 9, 8, 7, 6)){
  reasonable_set <- list()
  combinations <- list()
  if (gender == "m") {
    all_sims <- data.frame(SimCount = numeric(),
                           Country = character(),
                           P1 = character(),
                           P2 = character(),
                           P3 = character(),
                           P4 = character(),
                           P5 = character(),
                           Gold = numeric(),
                           Silver = numeric(),
                           Bronze = numeric(),
                           VT = character(),
                           FX = character(),
                           HB = character(),
                           PB = character(),
                           PH = character(),
                           SR = character(),
                           TotalScore = numeric(),
                           MedalCount = numeric())
  }
  
  if (gender == "w") {
    all_sims <- data.frame(SimCount = numeric(),
                           Country = character(),
                           P1 = character(),
                           P2 = character(),
                           P3 = character(),
                           P4 = character(),
                           P5 = character(),
                           Gold = numeric(),
                           Silver = numeric(),
                           Bronze = numeric(),
                           VT = character(),
                           BB = character(),
                           UB = character(),
                           FX = character(),
                           TotalScore = numeric(),
                           MedalCount = numeric())
  }
  
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
    sim_count <- nrow(all_sims)
    
    if (gender == "m") {
      temp_sim <- data.frame(SimCount = numeric(),
                             Country = character(),
                             P1 = character(),
                             P2 = character(),
                             P3 = character(),
                             P4 = character(),
                             P5 = character(),
                             Gold = numeric(),
                             Silver = numeric(),
                             Bronze = numeric(),
                             VT = character(),
                             FX = character(),
                             HB = character(),
                             PB = character(),
                             PH = character(),
                             SR = character(),
                             TotalScore = numeric(),
                             MedalCount = numeric())
    }
    
    if (gender == "w") {
      temp_sim <- data.frame(SimCount = numeric(),
                             Country = character(),
                             P1 = character(),
                             P2 = character(),
                             P3 = character(),
                             P4 = character(),
                             P5 = character(),
                             Gold = numeric(),
                             Silver = numeric(),
                             Bronze = numeric(),
                             VT = character(),
                             BB = character(),
                             UB = character(),
                             FX = character(),
                             TotalScore = numeric(),
                             MedalCount = numeric())
    }
    
    if ((length(combinations)- j) == 1) {
      print(paste("Optimization in progress for", names(combinations[j]), ":", 
                  (length(combinations)- j), "more country to go"))
    } 
    else {
      print(paste("Optimization in progress for", names(combinations[j]), ":", 
                  (length(combinations)- j), "more countries to go"))
    }
    
    for (k in 1:length(combcountry)) {
      starttemp <- replace_players(start_teams, ctry, combinations, gender, k)
      sim <- simulate_medals(starttemp, qual36, means_df, stddevs_df, gender)
      sim[[1]] <- medalscore(sim[[1]], weights)
      sim_count = sim_count + 1
      
      if (k %% 50 == 0) {
        print(paste("Running simulation no.", k, "of", length(combcountry), "for", names(combinations[j])))
      }
      
      if (k == length(combcountry)) {
        print(paste("Running final simulation for", names(combinations[j])))
      }
      
      if (sim[[1]]$Score[sim[[1]]$Country == ctry] > top_score) {
        top_score <- sim[[1]]$Score[sim[[1]]$Country == ctry]
        start_teams <- replace_players(start_teams, ctry, combinations, gender, k)
      }
      
      temp_sim[k, "SimCount"] <- sim_count
      temp_sim[k, "Country"] <- ctry
      temp_sim[k, "P1"] <- combinations[[ctry]][k][[1]][1]
      temp_sim[k, "P2"] <- combinations[[ctry]][k][[1]][2]
      temp_sim[k, "P3"] <-  combinations[[ctry]][k][[1]][3]
      temp_sim[k, "P4"] <- combinations[[ctry]][k][[1]][4]
      temp_sim[k, "P5"] <- combinations[[ctry]][k][[1]][5]
      temp_sim[k, "Gold"] <- sim[[1]]$First[sim[[1]]$Country == ctry]
      temp_sim[k, "Silver"] <- sim[[1]]$Second[sim[[1]]$Country == ctry]
      temp_sim[k, "Bronze"] <- sim[[1]]$Third[sim[[1]]$Country == ctry]
      temp_sim[k, "TotalScore"] <- sim[[1]]$Score[sim[[1]]$Country == ctry]
      temp_sim[k, "MedalCount"] <- sim[[1]]$First[sim[[1]]$Country == ctry] + 
                                   sim[[1]]$Second[sim[[1]]$Country == ctry] + 
                                   sim[[1]]$Third[sim[[1]]$Country == ctry]
      if (gender == "m") {
        temp_sim[k, "VT"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "VT"]
        temp_sim[k, "FX"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "FX"]
        temp_sim[k, "HB"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "HB"]
        temp_sim[k, "PB"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "PB"]
        temp_sim[k, "PH"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "PH"]
        temp_sim[k, "SR"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "SR"]
      }
      
      if (gender == "w") {
        temp_sim[k, "VT"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "VT"]
        temp_sim[k, "BB"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "BB"]
        temp_sim[k, "UB"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "UB"]
        temp_sim[k, "FX"] <- sim[[2]]$Athletes[sim[[2]]$Country == ctry & 
                                                 sim[[2]]$App == "FX"]
      }
        
    }
    all_sims <- rbind(all_sims, temp_sim)
  }
  print("Done!")
  return(list(optimizedteams = start_teams, simresults = all_sims))
}

# mens (takes close to 2 hours to run)
men_best <- get.best.teams(start_teams = startteams,
                    countries = unique(startteams$Country[startteams$Gender == "m"]),
                    gender = "m",
                    means,
                    stddevs, alt36m)

#### men_best <- readRDS("data/best.teams.mens.rds")

# men_best$simresults <- men_best$simresults[order(-men_best$interestedteam$TotalScore), ]

# womens (takes close to 1 hours to run)
#women_best <- get.best.teams(start_teams = startteams, 
                    # countries = unique(startteams$Country[startteams$Gender == "w"]),
                    # gender = "w",
                    # means, 
                    # stddevs, alt36w)

#### women_best <- readRDS("data/best.teams.womens.rds")

# women_best$simresults <- women_best$simresults[order(-women_best$interestedteam$TotalScore), ]

# saved
saveRDS(men_best, file = "data/best.teams.mens.rds")
saveRDS(women_best, file = "data/best.teams.womens.rds")

# saving data as csv
# write.csv(women_best$finalteams, "data/optimized.womens.teams.csv", row.names = FALSE)
# write.csv(men_best$finalteams, "data/optimized.mens.teams.csv", row.names = FALSE)
# write.csv(women_best$interestedteam, "data/best.womens.teams.country.of.interest.csv", row.names = FALSE)
# write.csv(men_best$interestedteam, "data/best.mens.teams.country.of.interest.csv", row.names = FALSE)

