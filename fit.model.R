library(dplyr)
library(tidyverse)

# calculate the mean scores in the data per athlete per apparatus
means_df <- data %>%
  group_by(ID, Gender, Country, Apparatus) %>%
  summarise(score_mean = mean(Score)) %>%
  pivot_wider(names_from=Apparatus, values_from=score_mean)

# calculate the stddev of scores in the data per athlete per apparatus
stddevs_df <- data %>%
  group_by(ID, Gender, Country, Apparatus) %>%
  summarise(score_sd = sd(Score)) %>%
  pivot_wider(names_from=Apparatus, values_from=score_sd)

events <- c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR")
samples_df <- as.data.frame(matrix(nrow=dim(means_df)[1], ncol=length(events)+3))
colnames(samples_df) <- c(c("ID"), events)
samples_df$ID <- means_df$ID
samples_df$Country <- means_df$Country
samples_df$Gender <- means_df$Gender

# Loop through the means and standard deviations dataframes
for (i in 1:nrow(means_df)) {
  for (j in events) {
    # Generate a random sample using rnorm()
    if(is.na(means_df[[i, j]]) || is.na(stddevs_df[[i, j]])){
      random_sample <- NA
    }
    else{
      # print(means_df[[i, j]], stddevs_d[[i, j]])
      random_sample <- rnorm(1, mean = means_df[[i, j]], sd = stddevs_df[[i, j]])  
    }
    
    # Append the random sample to the samples 
    samples_df[[i, j]] <- random_sample
  }
}

# write the means and stddevs output files
write.csv(means_df, "data/means_per_app.csv", row.names = F)
write.csv(stddevs_df, "data/stddevs_per_app.csv", row.names = F)
write.csv(samples_df, "data/score_predictions.csv", row.names = F)
