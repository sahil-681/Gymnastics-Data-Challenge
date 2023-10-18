library(tidyverse)
library(data.table)

tally_medals <- function(long_medals, countries, places){
  
  if(is.data.frame(countries)){
    countries_df <- countries
  }else{
    countries_df <- data.frame(
      Country = countries,
      Place = places,
      Add = 1
    )
  }
  long_medals <- merge(long_medals, countries_df, all.x=T)
  long_medals$Add[is.na(long_medals$Add)] <- 0
  long_medals$Total_Count <- long_medals$Total_Count + long_medals$Add
  long_medals$Add <- NULL
  return(long_medals)
}

run_sims <- function(competitors, qual36, long_meanstds, gender, apps, do_sampling=F){
  
  #columns: ID, Gender, Country, App
  
  teamed_quals <- merge(competitors, long_meanstds, all.x=T)
  
  alt_quals <- qual36[, c("ID", "Country")] # get list of alternates (competitors not part of a qualified country)
  alt_quals <- merge(alt_quals, data.frame(App = apps), by=NULL)
  alt_quals <- merge(alt_quals, long_meanstds, all.x=T)
  
  # 0. Generating qual scores 
  quals <- rbind(teamed_quals, alt_quals)
  samples_df <- quals %>% select(-c(Mean, Stddev))
  if(do_sampling){
    samples_df$Score <- rnorm(length(quals$ID), mean=quals$Mean, sd=quals$Stddev)
  }else{
    samples_df$Score <- quals$Mean
  }
  
  
  # 1. Team Qualifications 
  country_scores <- samples_df[samples_df$Country %in% teamed_quals$Country,] %>%
    group_by(Country, App) %>% slice_max(order_by=Score, n = 3) %>% ungroup %>%
    group_by(Country) %>% summarise(Total_Score = sum(Score)) %>% arrange(desc(Total_Score))
  qual_teams <- country_scores$Country[1:8]
  
  
  aa_df <- samples_df %>% pivot_wider(id_cols = c("ID", "Country"), names_from="App", values_from="Score")
  aa_df <- aa_df[complete.cases(aa_df),]  # 2. AA Qualifications
  
  aa_df$Total_Score <- rowSums(aa_df[, apps])
  aa_scores <- aa_df[, c("ID", "Country", "Total_Score")] %>% group_by(Country) %>%
    arrange(desc(Total_Score)) %>% slice_head(n=2)
  aa_scores <- aa_scores %>% arrange(desc(Total_Score))
  qual_aa <- aa_scores[1:24, c("ID", "Country")]
  
  
  #3. Event Qualifications
  evs_df <- samples_df %>% group_by(App, Country) %>% arrange(desc(Score)) %>% slice_head(n=2)
  evs_df <- evs_df %>% group_by(App) %>% arrange(desc(Score)) %>% mutate(Rank=row_number()) %>% slice_head(n=8)
  qual_ind <- evs_df[, c("ID", "Country", "App")]
  
  # -----------------------FINALS SIMULATIONS-----------------------------------
  
  # PREP
  country_medals <- data.frame(
    Country = unique(long_meanstds$Country),
    Total_Count = 0
  )
  medal_detail_slots <- list()
  places <- c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth")
  country_medals <- merge(country_medals, data.frame(Place = places), by=NULL)
  
  # Simulate Team Finals (M)
  teamed_finals <- teamed_quals[teamed_quals$Country %in% qual_teams,]
  teamed_finals <- teamed_finals %>% group_by(Country, App) %>%
    arrange(desc(Mean)) %>% slice_head(n=3)
  if(do_sampling){
    teamed_finals$Score <- rnorm(length(teamed_finals$Mean), mean=teamed_finals$Mean, sd=teamed_finals$Stddev)
  }else{
    teamed_finals$Score <- teamed_finals$Mean # use mean if only one sample 
  }
  country_scores <- teamed_finals %>% group_by(Country) %>%
    summarise(Total_Score = sum(Score), NAs = sum(is.na(Score))) %>% arrange(desc(Total_Score))
  country_medals <- tally_medals(country_medals, country_scores$Country, places)
  
  medal_detail_slots[[1]] <- data.frame(
    App = "Team",
    Place = places,
    Score = country_scores$Total_Score,
    Country = country_scores$Country,
    ID = NA
  )
  
  
  # Simulate AA Finals(M)
  qual_aa <- merge(qual_aa, data.frame(App=apps), by=NULL)
  qual_aa <- merge(qual_aa, long_meanstds, all.x=T)
  if(do_sampling){
    qual_aa$Score <- rnorm(length(qual_aa$Mean), mean=qual_aa$Mean, sd=qual_aa$Stddev)
  }else{
    qual_aa$Score <- qual_aa$Mean # use mean if only trying one sample 
  }
  aa_scores <- qual_aa %>% group_by(ID, Country) %>%
    summarise(Total_Score = sum(Score), NAs = sum(is.na(Score))) %>%
    arrange(desc(Total_Score))
  top8_countries <- aa_scores$Country[1:8]
  country_medals <- tally_medals(country_medals, top8_countries, places)
  medal_detail_slots[[2]] <- data.frame(
    App = "AA",
    Place = places,
    Score = aa_scores$Total_Score[1:8],
    Country = aa_scores$Country[1:8],
    ID = aa_scores$ID[1:8]
  )
  
  # Simulate Event Finals
  qual_ind <- merge(qual_ind, long_meanstds, all.x=T)
  if(do_sampling){
    qual_ind$Score <- rnorm(length(qual_ind$Mean), mean=qual_ind$Mean, sd=qual_ind$Stddev)
  }else{
    qual_ind$Score <- qual_ind$Mean # use mean if only trying one sample
  }
  ind_scores <- qual_ind %>% arrange(desc(Score)) %>% group_by(App) %>% mutate(Place=places)
  country_scores <- ind_scores[, c("Country", "Place")] %>% group_by(Country, Place) %>% summarise(Add = n())
  country_medals <- tally_medals(country_medals, country_scores, places)
  medal_detail_slots[[3]] <- ind_scores[, c("App", "Place", "Score", "Country", "ID")]
  
  country_medals = suppressWarnings(country_medals)
  country_medals <- country_medals %>% pivot_wider(names_from=Place, values_from=Total_Count)
  return(list(country_medals, rbindlist(medal_detail_slots)))
}
