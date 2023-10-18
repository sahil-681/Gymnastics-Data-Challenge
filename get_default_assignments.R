library(dplyr)
library(tidyverse)

get_default_assignments <- function(top12teams, means_df){
  
  gender <- unique(top12teams$Gender)
  if(gender=="m"){
    apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }else{
    apps <- c("VT", "BB", "UB", "FX")
  }
  
  means_df$aa_sums <- rowSums(means_df[, apps]) # temporarily create combined mean column
  top_AAs <- means_df[
    complete.cases(means_df) & means_df$Country %in% unique(top12teams$Country), ] %>%
    group_by(Country) %>% arrange(desc(aa_sums)) %>% slice_head(n=2) %>%
    pivot_longer(cols=apps, names_to="App", values_to="Mean") %>% select(-aa_sums)
  means_df <- means_df %>% select(-aa_sums) # remove combined mean column
  
  top_nAAs <- means_df[
    means_df$Country %in% unique(top12teams$Country) & !(means_df$ID %in% top_AAs$ID), ] %>%
    pivot_longer(cols=apps, names_to="App", values_to="Mean") %>% group_by(Country, App) %>%
    slice_max(order_by=Mean, n=4)
  
  quals_means <- rbind(top_AAs, top_nAAs) %>% group_by(Country, App) %>% slice_head(n = 4) # combine w/ AA-athletes first so that they are guaranteed, then also keep top non-AAs such that each app will have 4 total
  competitors <- quals_means[, c("ID", "Country", "App", "Mean")]
  return(competitors)
  
}