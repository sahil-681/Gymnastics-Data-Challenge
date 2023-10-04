library(dplyr)
library(tidyverse)

# calculate the mean scores in the data per athlete per apparatus
means_df <- data %>%
  group_by(ID, Gender, Country, Apparatus) %>%
  summarise(score_mean = mean(Score, na.rm=T)) %>%
  pivot_wider(names_from=Apparatus, values_from=score_mean)

events <- c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR")

# calculate the stddev of scores in the data per athlete per apparatus
stddevs_df <- data %>%
  group_by(ID, Gender, Country, Apparatus) %>%
  summarise(score_sd = sd(Score, na.rm=T)) %>%
  pivot_wider(names_from=Apparatus, values_from=score_sd)

# replace all gender-apparatus appropriate NA stddevs with 0.4
stddevs_df[stddevs_df$Gender == "m", ] = stddevs_df[stddevs_df$Gender == "m", ] %>%
  mutate_at(vars("VT", "FX", "HB", "PB", "PH", "SR"), ~ifelse(is.na(.), 0.4, .))
stddevs_df[stddevs_df$Gender == "w", ] = stddevs_df[stddevs_df$Gender == "w", ] %>%
  mutate_at(vars("VT", "BB", "UB", "FX"), ~ifelse(is.na(.), 0.4, .))


samples_df <- as.data.frame(matrix(nrow=dim(means_df)[1], ncol=length(events)))
colnames(samples_df) <- events

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
print(colnames(samples_df))
