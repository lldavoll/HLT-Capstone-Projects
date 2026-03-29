# 1. Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Load the dataset
file_path <- "data/most_visited_nps_species_data.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# 3. Select only the required columns
df_filtered <- df %>%
  select(ParkName, SciName, CommonNames, GRank)

# 4. Removing duplicate entries of common names in each national park
df_cleaned <- df_filtered %>%
  distinct(ParkName, CommonNames, .keep_all = TRUE)

# 5. Removing duplicate entries of scientific names in each national park
df_clean <- df_cleaned %>%
  distinct(ParkName, SciName, .keep_all = TRUE)

# 6. Clean and categorize GRank values
df_clean$GRank <- case_when(
  grepl("^G1", df_clean$GRank, ignore.case = TRUE) ~ "G1",
  grepl("^G2", df_clean$GRank, ignore.case = TRUE) ~ "G2",
  grepl("^G3", df_clean$GRank, ignore.case = TRUE) ~ "G3",
  grepl("^G4", df_clean$GRank, ignore.case = TRUE) ~ "G4",
  grepl("^G5", df_clean$GRank, ignore.case = TRUE) ~ "G5",
  grepl("^G", df_clean$GRank, ignore.case = TRUE) ~ "GG",# Other G-Rank values
  TRUE ~ "Null"  # Everything else
)
df_selected <- df_clean %>%
  filter(GRank != "GG")
#df_selected <- df_selected %>%
#filter(GRank != "Null")

# 7. Count the number of species per GRank per ParkName
grank_counts <- df_selected %>%
  group_by(ParkName, GRank) %>%
  summarise(Count = n(), .groups = 'drop')

# 8. Convert to long format for plotting
grank_long <- grank_counts %>%
  pivot_wider(names_from = GRank, values_from = Count, values_fill = list(Count = 0)) %>%
  pivot_longer(cols = -ParkName, names_to = "GRank", values_to = "Count")

custom_colors <- c("G1" = "#FF204E",
                   "G2" = "#F94C10",
                   "G3" = "#F8DE22",
                   "G4" = "#45FFCA",
                   "G5" = "#06D001",
                   "Null" = "#FFFFFF",
                   "X"= "cyan") 
grank_long$GRank <- factor(grank_long$GRank, levels = c("G5", "G4", "G3", "G2", "G1", "X", "Null"))
# 9. Plot the stacked bar chart
stacked_bar <- ggplot(grank_long, aes(x = reorder(ParkName, -Count), y = Count, fill = GRank)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Species Count per National Park by GRank",
       x = "National Parks",
       y = "Number of Species",
       fill = "GRank") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = custom_colors) +
  coord_flip()

# 16. Show final plot
print(stacked_bar)

# 17. Saving final plot
ggsave("Stacked_Bar_Species_Conservation.png", plot = stacked_bar, width = 19.2, height = 10.8, dpi = 900)
