library(dplyr)
library(readr)

top12teams <- read_csv("data/team_country_qualified_individuals.csv")

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

# Scores for top countries for men
top_men_df <- top_4_athletes_per_apparatus %>%
  filter(Gender == "m") %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(Final_Score))

# Slice the top 3 rows for each group (dropping the 4th highest Final_Score)
filtered_df_m <- top_men_df %>%
  slice(1:3) %>%
  group_by(Country) %>%
  summarize(Final_Score = sum(Final_Score, na.rm = TRUE)) %>%
  arrange(desc(Final_Score)) %>%
  mutate(Gender = "m")

# Select the top 8 countries with the highest Final_Score
top_8_countries_men <- head(filtered_df_m, n = 8)

# Scores for top countries for women
top_women_df <- top_4_athletes_per_apparatus %>%
  filter(Gender == "w") %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(Final_Score))

# Slice the top 3 rows for each group (dropping the 4th highest Final_Score)
filtered_df_w <- top_women_df %>%
  slice(1:3) %>%
  group_by(Country) %>%
  summarize(Final_Score = sum(Final_Score, na.rm = TRUE)) %>%
  arrange(desc(Final_Score)) %>%
  mutate(Gender = "w")

# Select the top 8 countries with the highest Final_Score
top_8_countries_women <- head(filtered_df_w, n = 8)

# Combine the data frames
qualifying_countries <- rbind(top_8_countries_men, top_8_countries_women)

# Create a data frame with the desired structure
qualified_teams <- data.frame(Gender = character(),
                              T1 = character(),
                              T2 = character(),
                              T3 = character(),
                              T4 = character(),
                              T5 = character(),
                              T6 = character(),
                              T7 = character(),
                              T8 = character(),
                              stringsAsFactors = FALSE)

qualified_teams[1, 1] = "m"
qualified_teams[2, 1] = "w"

for (i in 1:nrow(top_8_countries_men)) {
  qualified_teams[1, i+1] = top_8_countries_men$Country[i]
}

for (i in 1:nrow(top_8_countries_women)) {
  qualified_teams[2, i+1] = top_8_countries_women$Country[i]
}

# Save dataframe
write_csv(qualified_teams, "finaled_teams.csv")
