library(ggplot2)
library(dplyr)

# Assuming 'simres' contains your simulation results
# Filter the data to include only the first, second, and third positions
simres_filtered <- simres %>% 
  select(Name, Country, Apparatus, First, Second, Third) %>%
  pivot_longer(cols = c(First, Second, Third), names_to = "Position", values_to = "Probability") %>%
  filter(Position %in% c("First", "Second", "Third"))

# Plot the heatmap
ggplot(simres_filtered, aes(x = Position, y = Name, fill = Probability)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Simulation Results (First, Second, Third Positions Only)",
       x = "Position",
       y = "Athlete Name",
       fill = "Probability") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)  # Load ggrepel package

# Filter the data to include only the athletes with probabilities in first, second, and third positions
simres_filtered <- simres %>%
  filter(!is.na(First) | !is.na(Second) | !is.na(Third)) %>%
  select(Name, Country, Apparatus, First, Second, Third) %>%
  pivot_longer(cols = c(First, Second, Third), names_to = "Position", values_to = "Probability") 

# Plot the data
ggplot(simres_filtered, aes(x = Position, y = Probability, color = Name, label = paste(Name, "(", Country, ")"))) +
  geom_point(size = 3) +
  facet_wrap(~Apparatus, scales = "free") +
  labs(title = "Probabilities of First, Second, and Third Positions by Athlete and Apparatus",
       x = "Position",
       y = "Probability",
       color = "Athlete") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  geom_text_repel(size = 3, nudge_y = 0.1)




