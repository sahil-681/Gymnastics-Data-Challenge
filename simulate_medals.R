# this function needs tidyr
# call using: simulate_medals(top_12_teams, alt36, means, stddevs, g, ptime=T)
simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, ptime=F){
  start_t <- Sys.time()
  
  means_df <- means_df[means_df$Gender == gender, ]
  stddevs_df <- stddevs_df[stddevs_df$Gender == gender, ]
  top12teams <- top12teams[top12teams$Gender == gender, ]
  
  
  if(gender=="m"){
    apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }else{
    apps <- c("VT", "BB", "UB", "FX")
  }
  
  
  # Create Qualifying Samples
  get_samples_df <- function(meansdf, stddevsdf){
    long_means <- pivot_longer(meansdf,
                               cols=c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR"),
                               names_to="App", values_to="mean")
    long_stddevs <- pivot_longer(stddevsdf,
                                 cols=c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR"),
                                 names_to="App", values_to="stddev")
    long_data <- merge(x=long_means, y=long_stddevs, by=c("ID", "Gender", "Country", "App"), all=TRUE)
    
    samples_df <- long_data[, c("ID", "Gender", "Country", "App")]
    samples_df$Score <- rnorm(length(long_data$ID), mean=long_data$mean, sd=long_data$stddev)
    samples_df <- pivot_wider(samples_df, names_from="App", values_from="Score")
  
    return(samples_df)
  }
  samples_df <- get_samples_df(means_df, stddevs_df)
  
  
  # Team Qualifications
  means_q <- means_df %>%
    filter(ID %in% top12teams$ID)
  
  means_q$Total_Mean <- rowSums(means_q[, c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR")], na.rm = TRUE)
  
  means_q <- means_q %>%
    mutate(across(VT:SR, ~ifelse(is.na(.), 0, .)))
  
  # Selecting top 4 means to compete in qualifying
  means_q2 <- means_q %>%
    pivot_longer(cols = VT:SR, names_to = "Apparatus", values_to = "Mean_Score")
  
  top_4_athletes_per_apparatus <- means_q2 %>%
    group_by(Country, Gender, Apparatus) %>%
    arrange(desc(Mean_Score)) %>%
    slice_head(n = 4) %>%
    ungroup() %>%
    filter(!(Gender == "m" & (Apparatus %in% c("BB", "UB")) 
             & Mean_Score == 0.000000)) %>%
    filter(!(Gender == "w" & (Apparatus %in% c("HB", "PB", "PH", "SR")) 
             & Mean_Score == 0.000000))
  
  samples_longer <- samples_df %>%
    pivot_longer(cols = VT:SR, names_to = "Apparatus", values_to = "Final_Score")
  
  # Left join top_4_athletes_per_apparatus with samples_longer by ID and Apparatus
  top_4_athletes_per_apparatus <- top_4_athletes_per_apparatus %>%
    left_join(samples_longer, by = c("ID", "Apparatus")) %>%
    mutate(Final_Score = ifelse(is.na(Final_Score), 0, Final_Score)) %>%
    rename(Gender = Gender.x, Country = Country.x) %>%
    select(-Mean_Score, -Country.y, -Gender.y)  # Remove the Mean_Score column if you no longer need it
  
  # Scores for top countries
  top_df <- top_4_athletes_per_apparatus %>%
    filter(Gender == gender) %>%
    group_by(Country, Apparatus) %>%
    arrange(desc(Final_Score))
  
  # Slice the top 3 rows for each group (dropping the 4th highest Final_Score)
  filtered_df <- top_df %>%
    slice(1:3) %>%
    group_by(Country) %>%
    summarize(Final_Score = sum(Final_Score, na.rm = TRUE)) %>%
    arrange(desc(Final_Score)) %>%
    mutate(Gender = "m")
  
  # Select the top 8 countries with the highest Final_Score
  top_8_countries <- head(filtered_df, n = 8)
  qual_teams <- top_8_countries$Country # 
  
  
  # AA QUalifications
  combined_id_aa <- c(qual36$ID, top12teams$ID)
  
  allaround <- samples_df %>%
    filter(ID %in% combined_id_aa)
  
  filtered <- allaround[complete.cases(allaround[, apps]), ]
  filtered$aa_score <- rowSums(filtered[, apps], na.rm=T)
  qual_aa_IDs <- filtered$ID[order(-filtered$aa_score)[1:24]]
  
  
  # Event Qualifications
  combined_ids <- c(qual36$ID, top12teams$ID)
  
  events_q <- samples_df %>%
    filter(ID %in% combined_ids) %>%
    pivot_longer(cols = -c(ID, Country, Gender), 
                 names_to = "Apps", 
                 values_to = "Score")
  
  # Define the number of top athletes to keep
  top_n_athletes <- 8
  
  # Subset the dataframe for men's and women's events separately
  top_athletes <- events_q %>%
    filter(!is.na(Score)) %>% 
    group_by(Apps) %>%
    arrange(desc(Score)) %>%  # Sort by Score in descending order
    slice_head(n = top_n_athletes)  # Select the top N athletes
  
  top_athletes <- top_athletes %>%
    group_by(Apps) %>%
    mutate(Rank = row_number())
  
  qual_ind <- top_athletes %>%
    pivot_wider(
      id_cols = c("Apps"),
      names_from = Rank,
      values_from = ID
    )
  
  
  # -----------------------FINALS SIMULATIONS-----------------------------------
  
  # PREP
  # the output df, tallying the number of each medal for each country
  country_medals <- data.frame(
    Country = unique(means_df$Country),
    Firsts = 0, Seconds = 0, Thirds = 0, Fourths = 0,
    Fifths = 0, Sixths = 0, Sevenths = 0, Eighths = 0
  )
  
  # use as dictionary to get the country of each ID (player)
  ID_country_dict <- means_df[, c("ID", "Country")]
  ID_country_dict <- ID_country_dict[!duplicated(ID_country_dict$ID, fromLast=T), ]
  
  
  # Simulate Team Finals (M)
  country_scores <- data.frame(
    country = qual_teams,
    scores = rep(0, 8), 
    nas = rep(0, 8)
  )
  for(country in qual_teams){
    n_nas <- 0
    athlete_names <- top12teams$ID[top12teams$Country==country]
    
    for(app in apps){
      curr_df <- means_df[means_df$ID %in% athlete_names,]
      top3 <- order(-curr_df[, app])[1:3]
      top3_id <- curr_df$ID[top3]
      for(id in top3_id){
        score <- rnorm(1, 
                       mean=means_df[means_df$ID==id, app],
                       sd=stddevs_df[stddevs_df$ID==id, app]
                    )
        if(!is.na(score)){ # only add if not na, and tally up nas
          country_scores$scores[country_scores$country == country] <- country_scores$scores[country_scores$country == country] + score
        }
        else{
          n_nas <- n_nas + 1
        }
      }
    }
    country_scores$nas[country_scores$country == country] <- n_nas
  }
  
  #assigning medals
  top8_countries <- country_scores$country[order(-country_scores$scores)[1:8]]
  country_medals$Firsts[country_medals$Country == top8_countries[1]] = country_medals$Firsts[country_medals$Country == top8_countries[1]] + 1
  country_medals$Seconds[country_medals$Country == top8_countries[2]] = country_medals$Seconds[country_medals$Country == top8_countries[2]] + 1
  country_medals$Thirds[country_medals$Country == top8_countries[3]] = country_medals$Thirds[country_medals$Country == top8_countries[3]] + 1
  country_medals$Fourths[country_medals$Country == top8_countries[4]] = country_medals$Fourths[country_medals$Country == top8_countries[4]] + 1
  country_medals$Fifths[country_medals$Country == top8_countries[5]] = country_medals$Fifths[country_medals$Country == top8_countries[5]] + 1
  country_medals$Sixths[country_medals$Country == top8_countries[6]] = country_medals$Sixths[country_medals$Country == top8_countries[6]] + 1
  country_medals$Sevenths[country_medals$Country == top8_countries[7]] = country_medals$Sevenths[country_medals$Country == top8_countries[7]] + 1
  country_medals$Eighths[country_medals$Country == top8_countries[8]] = country_medals$Eighths[country_medals$Country == top8_countries[8]] + 1
  
  
  # Simulate AA Finals(M)
  qual_aa <- read.csv("data/finaled_aas.csv")
  aa_scores <- data.frame(
    ID = qual_aa_IDs,
    scores = rep(0, 24),
    nas = rep(0, 24)
  )
  
  for(id in qual_aa_IDs){
    n_nas <- 0
    for(app in apps){
        score <- rnorm(1,
                       mean=means_df[means_df$ID==id, app],
                       sd=stddevs_df[stddevs_df$ID==id, app]
                    )
        if(!is.na(score)){
          aa_scores$scores[aa_scores$ID == id] <- aa_scores$scores[aa_scores$ID == id] + score
        }
        else{
          print(c(id, app, means_df[means_df$ID==id, app], stddevs_df[stddevs_df$ID==id, app]))
          n_nas <- n_nas + 1
        }
    }
    aa_scores$nas[aa_scores$ID == id] <- n_nas
  }
  
  #assigning medals
  top8_i <- aa_scores$ID[order(-aa_scores$scores)[1:8]]
  top8_countries <- as.character(sapply(top8_i, function(i) ID_country_dict$Country[ID_country_dict$ID == i]))
  country_medals$Firsts[country_medals$Country == top8_countries[1]] = country_medals$Firsts[country_medals$Country == top8_countries[1]] + 1
  country_medals$Seconds[country_medals$Country == top8_countries[2]] = country_medals$Seconds[country_medals$Country == top8_countries[2]] + 1
  country_medals$Thirds[country_medals$Country == top8_countries[3]] = country_medals$Thirds[country_medals$Country == top8_countries[3]] + 1
  country_medals$Fourths[country_medals$Country == top8_countries[4]] = country_medals$Fourths[country_medals$Country == top8_countries[4]] + 1
  country_medals$Fifths[country_medals$Country == top8_countries[5]] = country_medals$Fifths[country_medals$Country == top8_countries[5]] + 1
  country_medals$Sixths[country_medals$Country == top8_countries[6]] = country_medals$Sixths[country_medals$Country == top8_countries[6]] + 1
  country_medals$Sevenths[country_medals$Country == top8_countries[7]] = country_medals$Sevenths[country_medals$Country == top8_countries[7]] + 1
  country_medals$Eighths[country_medals$Country == top8_countries[8]] = country_medals$Eighths[country_medals$Country == top8_countries[8]] + 1
  
  
  # Simulate Event Finals
  get_event_final_scores <- function(gender, app, IDs){
    
    out_df <- data.frame(
      ID = IDs,
      gender = rep(gender, length(IDs)),
      app = rep(app, length(IDs)),
      score = rep(0, length(IDs))
    )
    out_df$Score <- sapply(out_df$ID, 
                           function(id) rnorm(1,
                                              mean=means_df[means_df$ID==id, app],
                                              sd=stddevs_df[stddevs_df$ID==id, app]
                                              )
                           )
    return(out_df)
  }
  
  
  for(i in 1:dim(qual_ind)[1]){
    curr_event <- qual_ind[i, ]
    
    scores <- get_event_final_scores(
      gender=gender,
      app=curr_event$Apps,
      IDs=as.character(curr_event[, 2:ncol(curr_event)]))
    
    top8_i <- scores$ID[order(-scores$score)[1:8]]
    top8_countries <- as.character(sapply(top8_i, function(i) ID_country_dict$Country[ID_country_dict$ID == i]))
    country_medals$Firsts[country_medals$Country == top8_countries[1]] = country_medals$Firsts[country_medals$Country == top8_countries[1]] + 1
    country_medals$Seconds[country_medals$Country == top8_countries[2]] = country_medals$Seconds[country_medals$Country == top8_countries[2]] + 1
    country_medals$Thirds[country_medals$Country == top8_countries[3]] = country_medals$Thirds[country_medals$Country == top8_countries[3]] + 1
    country_medals$Fourths[country_medals$Country == top8_countries[4]] = country_medals$Fourths[country_medals$Country == top8_countries[4]] + 1
    country_medals$Fifths[country_medals$Country == top8_countries[5]] = country_medals$Fifths[country_medals$Country == top8_countries[5]] + 1
    country_medals$Sixths[country_medals$Country == top8_countries[6]] = country_medals$Sixths[country_medals$Country == top8_countries[6]] + 1
    country_medals$Sevenths[country_medals$Country == top8_countries[7]] = country_medals$Sevenths[country_medals$Country == top8_countries[7]] + 1
    country_medals$Eighths[country_medals$Country == top8_countries[8]] = country_medals$Eighths[country_medals$Country == top8_countries[8]] + 1
  }
  
  end_t <- Sys.time()
  
  if(ptime == T){
    print(paste("Simulate medals took", end_t - start_t, "s"))
  }

  return(country_medals)
}




