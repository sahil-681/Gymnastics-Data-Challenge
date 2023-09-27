get_reasonable_sets <- function(country, gender, means_df, stddevs_df, top_n=5){
  
  if(gender == "m"){
    apps <- c("VT", "FX", "HB", "PB", "PH", "SR")
  }
  else{
    apps <- c("VT", "BB", "UB", "FX")
  }
  
  curr_means <- means_df[means_df$Country == country & means_df$Gender == gender, ]
  curr_stddevs <- stddevs_df[stddevs_df$Country == country & means_df$Gender == gender, ]
  
  reasonables <- numeric(length(apps)*top_n)
  for(i in 1:length(apps)){
    sorted <- curr_means$ID[order(curr_means[, apps[i]])][1:top_n]
    reasonables[top_n*(i-1)+c(1:top_n)] <- sorted
  }
  return(unique(reasonables))
}