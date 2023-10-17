# this function needs tidyr
library(tidyverse)
library(data.table)
source("run_sims.R")

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
  
  # get top-2 all-around for each qualified country (to become our list of AA-athletes)
  means_df$aa_sums <- rowSums(means_df[, apps])
  t2_AA <- means_df[
    complete.cases(means_df) & means_df$Country %in% unique(top12teams$Country), ] %>% 
    group_by(Country) %>% arrange(desc(aa_sums)) %>% slice_head(n=2) %>% 
    pivot_longer(cols=apps, names_to="App", values_to="Mean") %>% select(-aa_sums)
  means_df <- means_df %>% select(-aa_sums) 
  
  #get top 4 per app per country, excluding AA-athletes
  t2_nAA <- means_df[
    means_df$Country %in% unique(top12teams$Country) & !(means_df$ID %in% t2_AA$ID), ] %>% 
    pivot_longer(cols=apps, names_to="App", values_to="Mean") %>% group_by(Country, App) %>%
    slice_max(order_by=Mean, n=4)
  
  
  quals_means <- rbind(t2_AA, t2_nAA) %>% group_by(Country, App) %>% slice_head(n = 4) # combine w/ AA-athletes first so that they are guaranteed, then also keep top non-AAs such that each app will have 4 total
  competitors <- quals_means[, c("ID", "Country", "App")]
  
  # Run the simulation
  sim_output <- run_sims(competitors, qual36, long_meanstds, gender, apps, do_sampling=F)
  
  
  # get IDs competing per country per apparatus, sorted in order
  team_app_athletes <- suppressMessages(quals_means %>% arrange(desc(Mean)) %>%
    group_by(Country, App) %>% summarise(Athletes = toString(unique(ID))))
  
  
  end_t <- Sys.time()
  if(ptime == T){
    print(paste("Simulate medals took", end_t - start_t, "s"))
  }
  
  return(list(medals_table = sim_output[[1]],
              app_assignments = team_app_athletes,
              medal_details = sim_output[[2]]))
}
  










