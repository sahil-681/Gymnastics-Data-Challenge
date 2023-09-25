library(dplyr)
library(readr)

top12teams <- read_csv("data/team_country_qualified_individuals.csv")

#36 athletes men
m36 <- read_csv("data/mens_36_athletes.csv")
w36 <- read_csv("data/womens_36_athletes.csv")

# Combine the two lists into one
combined_id_aa <- c(m36$ID, w36$ID, top12teams$ID)

allaround <- samples_df %>%
  filter(ID %in% combined_id_aa) 

# Keep entries where gender is "m" and have scores for VT, FX, HB, PB, PH, SR
male_filtered <- subset(allaround, Gender == "m" & !is.na(VT) & !is.na(FX) & !is.na(HB) & !is.na(PB) & !is.na(PH) & !is.na(SR))

# Keep entries where gender is "f" and have scores for VT, BB, UB, FX
female_filtered <- subset(allaround, Gender == "w" & !is.na(VT) & !is.na(BB) & !is.na(UB) & !is.na(FX))

# Combine the filtered dataframes
filtered_allaround <- rbind(male_filtered, female_filtered)
# Calculate the sum of scores for VT, FX, HB, PB, PH, and SR for each row
filtered_allaround$score <- rowSums(filtered_allaround[, c("VT", "BB", "UB", "FX", "HB", "PB", "PH", "SR")], na.rm = TRUE)

# Select the top 24 highest-scoring men
top_24_men <- head(subset(filtered_allaround, Gender == "m"), n = 24)

# Select the top 24 highest-scoring women
top_24_women <- head(subset(filtered_allaround, Gender == "w"), n = 24)

# Create a dataframe for the top 24 highest-scoring men and women
top_24_both <- rbind(top_24_men, top_24_women)

# Create a dataframe with Gender and Score columns
result_df <- data.frame(
  Gender = c("m", "f"),
  Score = c(sum(top_24_men$score), sum(top_24_women$score))
)

# Populate columns A1 to A24 with the top scorer IDs for men
for (i in 1:24) {
  if (i <= nrow(top_24_men)) {
    result_df[, paste("A", i, sep = "")] <- top_24_men$ID[i]
  } else {
    result_df[, paste("A", i, sep = "")] <- ""
  }
}

# Populate columns A1 to A24 with the top scorer IDs for women
for (i in 1:24) {
  if (i <= nrow(top_24_women)) {
    result_df[2, paste("A", i, sep = "")] <- top_24_women$ID[i]
  } else {
    result_df[2, paste("A", i, sep = "")] <- ""
  }
}

write_csv(result_df, "finaled_aas.csv")
