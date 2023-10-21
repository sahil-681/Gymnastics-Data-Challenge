# this function needs tidyr
library(tidyverse)
library(data.table)
source("old_run_sims.R")
source("get_default_assignments.R")

simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, competitors = NULL){
  
  if(gender=="m"){
    my_app <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }else{
    my_app <- c("VT", "BB", "UB", "FX")
  }

  
  # top12teams <- top12teams[top12teams$Gender == gender, ]  
  # valid_ids <- c(qual36$ID, top12teams$ID) #already only male
  # means_df <- means_df[means_df$ID %in% valid_ids & means_df$Gender == gender, c(c("ID", "Country"), my_app)]
  # stddevs_df <- stddevs_df[stddevs_df$ID %in% valid_ids & stddevs_df$Gender == gender, c(c("ID", "Country"), my_app)]
  # long_means <- means_df %>% pivot_longer(cols=my_app, names_to="App", values_to="Mean") 
  # long_stddevs <- stddevs_df %>% pivot_longer(cols=my_app, names_to="App", values_to="Stddev")
  # long_meanstds <- merge(long_means, long_stddevs)
  
  
  # create what we are trying to input
  long_means <- means_df %>% pivot_longer(cols=my_app, names_to="App", values_to="Mean") 
  long_stddevs <- stddevs_df %>% pivot_longer(cols=my_app, names_to="App", values_to="Stddev")
  long_meanstds <- merge(long_means, long_stddevs)
  
  
  #try to stuff this part into run_sims
  qual36 <- qual36[!(qual36$Country %in% top12teams$Country),] # make sure alternates aren't from qualified NOC (just in case)  
  top12teams <- top12teams[top12teams$Gender == gender, ]
  valid_ids <- c(qual36$ID, top12teams$ID)
  long_meanstds <- long_meanstds[long_meanstds$ID %in% valid_ids &
                                   long_meanstds$Gender == gender &
                                   long_meanstds$App %in% my_app, ]
  
  
  
  
  
  
  # get default assignments of athletes to apps
  if (is.null(competitors)) {
    competitors <- get_default_assignments(top12teams, means_df)
  }
  
  # Run the simulation

  sim_output <- old_run_sims(competitors, qual36, long_meanstds, gender, my_app, do_sampling=T)
  
  # get IDs competing per country per apparatus, sorted in order
  team_app_athletes <- suppressMessages(competitors %>% arrange(desc(Mean)) %>%
    group_by(Country, App) %>% summarise(Athletes = toString(unique(ID))))
  
  return(list(medals_table = sim_output[[1]],
              app_assignments = team_app_athletes,
              medal_details = sim_output[[2]]))
}
  










