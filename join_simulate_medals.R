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
  # long_medals <- merge(long_medals, countries_df, all.x=T)
  long_medals <- left_join(long_medals, countries_df)
  long_medals$Add[is.na(long_medals$Add)] <- 0
  long_medals$Total_Count <- long_medals$Total_Count + long_medals$Add
  long_medals$Add <- NULL
  return(long_medals)
}

# call using: simulate_medals(top_12_teams, alt36, means, stddevs, g, ptime=T)
join_simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, ptime=F){
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
  long_meanstds <- left_join(long_means, long_stddevs)
  
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
  
  # teamed_quals <- merge(quals_means, long_stddevs, all.x=T) # list of competing athletes also part of competing country (trying for team-finals)
  teamed_quals <- left_join(quals_means, long_stddevs)                   
  
  alt_quals <- qual36[, c("ID", "Country")] # get list of alternates (competitors not part of a qualified country)
  # alt_quals <- merge(alt_quals, data.frame(App = apps), by=NULL)
  alt_quals <- cross_join(alt_quals, data.frame(App = apps))
  # alt_quals <- merge(alt_quals, long_meanstds, all.x=T)
  alt_quals <- left_join(alt_quals, long_meanstds)
  
  # 0. Generating qual scores 
  quals <- rbind(teamed_quals, alt_quals)
  samples_df <- quals %>% select(-c(Mean, Stddev))
  samples_df$Score <- rnorm(length(quals$ID), mean=quals$Mean, sd=quals$Stddev) # generate samples for qual
  
  
  # 1. Team Qualifications 
  country_scores <- samples_df[samples_df$Country %in% top12teams$Country,] %>%
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
  # 
  # PREP
  # the output df, tallying the number of each medal for each country
  country_medals <- data.frame(
    Country = unique(means_df$Country),
    Total_Count = 0
  )
  places <- c("Firsts", "Seconds", "Thirds", "Fourths", "Fifths", "Sixths", "Sevenths", "Eighths")
  # country_medals <- merge(country_medals, data.frame(Place = places), by=NULL)
  country_medals <- cross_join(country_medals, data.frame(Place = places))
  
  # Simulate Team Finals (M)
  teamed_finals <- teamed_quals[teamed_quals$Country %in% qual_teams,]
  teamed_finals <- teamed_finals %>% group_by(Country, App) %>%
    arrange(desc(Mean)) %>% slice_head(n=3)
  teamed_finals$Score <- rnorm(length(teamed_finals$Mean), mean=teamed_finals$Mean, sd=teamed_finals$Stddev)
  country_scores <- teamed_finals %>% group_by(Country) %>%
    summarise(Total_Score = sum(Score), NAs = sum(is.na(Score))) %>% arrange(desc(Total_Score))
  country_medals <- tally_medals(country_medals, country_scores$Country, places)
  
  
  # Simulate AA Finals(M)
  # qual_aa <- merge(qual_aa, data.frame(App=apps), by=NULL)
  qual_aa <- cross_join(qual_aa, data.frame(App=apps))
  # qual_aa <- merge(qual_aa, long_meanstds, all.x=T)
  qual_aa <- left_join(qual_aa, long_meanstds)
  qual_aa$Score <- rnorm(length(qual_aa$Mean), mean=qual_aa$Mean, sd=qual_aa$Stddev)
  aa_scores <- qual_aa %>% group_by(ID, Country) %>%
    summarise(Total_Score = sum(Score), NAs = sum(is.na(Score))) %>%
    arrange(desc(Total_Score))
  top8_countries <- aa_scores$Country[1:8]
  country_medals <- tally_medals(country_medals, top8_countries, places)
  
  
  # Simulate Event Finals
  # qual_ind <- merge(qual_ind, long_meanstds, all.x=T)
  qual_ind <- left_join(qual_ind, long_meanstds)
  qual_ind$Score <- rnorm(length(qual_ind$Mean), mean=qual_ind$Mean, sd=qual_ind$Stddev)
  ind_scores <- qual_ind %>% arrange(desc(Score)) %>% group_by(App) %>% mutate(Place=places)
  country_scores <- ind_scores[, c("Country", "Place")] %>% group_by(Country, Place) %>% summarise(Add = n())
  country_medals <- tally_medals(country_medals, country_scores, places)
  
  end_t <- Sys.time()
  if(ptime == T){
    print(paste("Simulate medals took", end_t - start_t, "s"))
  }
  
  country_medals = suppressWarnings(country_medals)
  print(sum(country_medals$Total_Count))
  country_medals <- country_medals %>% pivot_wider(names_from=Place, values_from=Total_Count)
  return(country_medals)
}