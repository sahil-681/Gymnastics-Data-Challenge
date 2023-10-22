library(tidyverse)
library(data.table)
source("run_sims.R")
source("get_default_assignments.R")

simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, ptime=F){
  start_t <- Sys.time()
  
  if(gender=="m"){
    apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }else{
    apps <- c("VT", "BB", "UB", "FX")
  }
  qual36 <- qual36[!(qual36$Country %in% top12teams$Country),] # make sure alternates aren't from qualified NOC (just in case)
  
  top12teams <- top12teams[top12teams$Gender == gender, ]  
  valid_ids <- c(qual36$ID, top12teams$ID) #already only male
  means_df <- means_df[means_df$ID %in% valid_ids & means_df$Gender == gender, c(c("ID", "Country"), apps)]
  stddevs_df <- stddevs_df[stddevs_df$ID %in% valid_ids & stddevs_df$Gender == gender, c(c("ID", "Country"), apps)]
  long_means <- means_df %>% pivot_longer(cols=apps, names_to="App", values_to="Mean") 
  long_stddevs <- stddevs_df %>% pivot_longer(cols=apps, names_to="App", values_to="Stddev")
  long_meanstds <- merge(long_means, long_stddevs)
  
  # get default assignments of athletes to apps
  competitors <- get_default_assignments(top12teams, means_df)
  
  # Run the simulation
  sim_output <- run_sims(competitors, qual36, long_meanstds, gender, apps, do_sampling=F)
  
  
  # get IDs competing per country per apparatus, sorted in order
  team_app_athletes <- suppressMessages(competitors %>% arrange(desc(Mean)) %>%
    group_by(Country, App) %>% summarise(Athletes = toString(unique(ID))))
  
  
  end_t <- Sys.time()
  if(ptime == T){
    print(paste("Simulate medals took", end_t - start_t, "s"))
  }
  
  return(list(medals_table = sim_output[[1]],
              app_assignments = team_app_athletes,
              medal_details = sim_output[[2]]))
}
  










