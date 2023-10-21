library(tidyverse)
library(data.table)
source("run_sims.R")
source("get_default_assignments.R")

simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, competitors = NULL){

  
  all_apps <- c("VT", "FX", "HB", "PB", "PH", "SR", "UB", "BB")
  
  
  top12teams <- top12teams[top12teams$Gender == gender,]
  competitors <- get_default_assignments(top12teams, means_df)
  print(competitors)
  
  # Run the simulation
  long_means <- means_df %>% pivot_longer(cols=all_apps, names_to="App", values_to="Mean") 
  long_stddevs <- stddevs_df %>% pivot_longer(cols=all_apps, names_to="App", values_to="Stddev")
  long_meanstds <- merge(long_means, long_stddevs)
  
  
  sim_output <- run_sims(competitors, qual36, long_meanstds, gender, do_sampling=T)
  
  # get IDs competing per country per apparatus, sorted in order
  team_app_athletes <- suppressMessages(competitors %>% arrange(desc(Mean)) %>%
                                          group_by(Country, App) %>% summarise(Athletes = toString(unique(ID))))
  
  return(list(medals_table = sim_output[[1]],
              app_assignments = team_app_athletes,
              medal_details = sim_output[[2]]))
}