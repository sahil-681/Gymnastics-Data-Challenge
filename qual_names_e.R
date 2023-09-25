library(dplyr)
library(readr)

top12teams <- read_csv("data/team_country_qualified_individuals.csv")

#36 athletes men
m36 <- read_csv("data/mens_36_athletes.csv")
w36 <- read_csv("data/womens_36_athletes.csv")

# Combine the two lists into one
combined_id_aa <- c(m36$ID, w36$ID, top12teams$ID)

events_q <- samples_df %>%
  filter(ID %in% combined_id_aa) %>%
  pivot_longer(cols = -c(ID, Country, Gender), 
               names_to = "Apparatus", 
               values_to = "Score")

# Define the number of top athletes to keep
top_n_athletes <- 8

# Subset the dataframe for men's and women's events separately
top_men_athletes <- events_q %>%
  filter(Gender == "m", !is.na(Score)) %>% 
  group_by(Apparatus) %>%
  arrange(desc(Score)) %>%  # Sort by Score in descending order
  slice_head(n = top_n_athletes)  # Select the top N athletes

top_women_athletes <- events_q %>%
  filter(Gender == "w", !is.na(Score)) %>%
  group_by(Apparatus) %>%
  arrange(desc(Score)) %>%  # Sort by Score in descending order
  slice_head(n = top_n_athletes)  # Select the top N athletes

# Combine the subsets for men and women
top_athletes <- bind_rows(top_men_athletes, top_women_athletes)

# Create a rank variable within each apparatus
top_athletes <- top_athletes %>%
  group_by(Apparatus, Gender) %>%
  mutate(Rank = row_number())

# Pivot the dataframe to wide format
qual_ind <- top_athletes %>%
  pivot_wider(
    id_cols = c("Apparatus", "Gender"),
    names_from = Rank,
    values_from = ID
  )

# Rename the columns
colnames(qual_ind) <- c("App", "Gender", paste0("ID", 1:top_n_athletes))

# Save dataframe
write_csv(qual_ind, "finaled_events.csv")
