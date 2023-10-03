# this function needs tidyr
# call using: simulate_medals(top_12_teams, alt36, means, stddevs, g, ptime=T)
simulate_medals <- function(top12teams, qual36, means_df, stddevs_df, gender, ptime=F){
  start_t <- Sys.time()
  
  if(gender=="m"){
    apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }else{
    apps <- c("VT", "BB", "UB", "FX")
  }
  qual36 <- qual36[!(qual36$Country %in% top12teams$Country),] # make sure alternates aren't from qualified NOC (just in case)
  
  top12teams <- top12teams[top12teams$Gender == gender, ]  
  valid_ids <- c(qual36$ID, top12teams$ID)
  
  means_df <- means_df[means_df$ID %in% valid_ids, c(c("ID", "Country", "Gender"), apps)]
  stddevs_df <- stddevs_df[stddevs_df$ID %in% valid_ids, c(c("ID", "Country", "Gender"), apps)]
  long_means <- means_df %>% pivot_longer(cols=apps, names_to="App", values_to="Mean") 
  long_stddevs <- stddevs_df %>% pivot_longer(cols=apps, names_to="App", values_to="Stddev")  
  
  
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
  
  
  teamed_quals <- merge(quals_means, long_stddevs, by.x=c("ID", "Country", "Gender", "App"), # list of competing athletes also part of competing country (trying for team-finals)
                 by.y=c("ID", "Country", "Gender", "App"), all.x=T)
  
  alt_quals <- qual36[, c("ID", "Country")] # get list of alternates (competitors not part of a qualified country)
  alt_quals$Gender <- gender
  alt_quals <- merge(alt_quals, data.frame(App = apps), by=NULL)
  alt_quals <- merge(alt_quals, long_means, by.x = c("ID", "Country", "Gender", "App"),
                     by.y=c("ID", "Country", "Gender", "App"), all.x=T)
  alt_quals <- merge(alt_quals, long_stddevs, by.x = c("ID", "Country", "Gender", "App"),
                     by.y=c("ID", "Country", "Gender", "App"), all.x=T)
  
  # 0. Generating qual scores 
  quals <- rbind(teamed_quals, alt_quals)
  samples_df <- quals %>% select(-c(Mean, Stddev))
  samples_df$Score <- rnorm(length(quals$ID), mean=quals$Mean, sd=quals$Stddev) # generate samples for qual
  
  
  # 1. Team Qualifications 
  country_scores <- samples_df[samples_df$Country %in% top12teams$Country,] %>%
    group_by(Country, App) %>% slice_max(order_by=Score, n = 3) %>% ungroup %>%
    group_by(Country) %>% summarise(Total_Score = sum(Score)) %>% arrange(desc(Total_Score))
  qual_teams <- country_scores$Country[1:8]
 
  
  # 2. AA Qualifications
  aa_df <- samples_df %>% pivot_wider(id_cols = c("ID", "Country", "Gender"), names_from="App", values_from="Score")
  aa_df <- aa_df[complete.cases(aa_df),]
  aa_df$Total_Score <- rowSums(aa_df[, apps])
  aa_scores <- aa_df[, c("ID", "Country", "Total_Score")] %>% group_by(Country) %>%
    arrange(desc(Total_Score)) %>% slice_head(n=2)
  aa_scores <- aa_scores %>% arrange(desc(Total_Score))
  qual_aa_IDs <- aa_scores$ID[1:24]
  
  
  #3. Event Qualifications
  evs_df <- samples_df %>% group_by(App, Country) %>% arrange(desc(Score)) %>% slice_head(n=2)
  evs_df <- evs_df %>% group_by(App) %>% arrange(desc(Score)) %>% mutate(Rank=row_number()) %>% slice_head(n=8)
  qual_ind <- evs_df[, c("App", "Rank", "ID")] %>% pivot_wider(names_from="Rank", values_from="ID")


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
    app=curr_event$App,
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

country_medals = suppressWarnings(country_medals)
return(country_medals)
}







