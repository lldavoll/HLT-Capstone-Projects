# 1. Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(ggplotify)
library(cowplot)
library(grid)
library(gridExtra)

# 2. Load the dataset
file_path <- "most_visited_nps_species_data.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# 3. Select only the required columns
df_filtered <- df %>%
  select(ParkName, SciName, CommonNames, CategoryName)

# 4. Define the category mapping
category_mapping <- c(
  'Mammal' = 'Vertebrates',
  'Bird' = 'Vertebrates',
  'Reptile' = 'Vertebrates',
  'Amphibian' = 'Vertebrates',
  'Fish' = 'Vertebrates',
  'Crab/Lobster/Shrimp' = 'Invertebrates',
  'Slug/Snail' = 'Invertebrates',
  'Spider/Scorpion' = 'Invertebrates',
  'Insect' = 'Invertebrates',
  'Other Non-vertebrates' = 'Invertebrates',
  'Vascular Plant' = 'Plants',
  'Non-vascular Plant' = 'Plants',
  'Fungi' = 'Fungi',
  'Chromista' = 'Microorganisms',
  'Protozoa' = 'Microorganisms',
  'Bacteria' = 'Microorganisms'
)

# 5. Apply mapping to the dataframe
df_filtered <- df_filtered %>%
  mutate(Category = category_mapping[CategoryName])

# 6. Removing duplicate entries of common names in each national park
df_cleaned <- df_filtered %>%
  distinct(ParkName, CommonNames, .keep_all = TRUE)

# 7. Removing duplicate entries of scientific names in each national park
df_clean <- df_cleaned %>%
  distinct(ParkName, SciName, .keep_all = TRUE)

# 8. Count total species in each park (for bubble sizes)
park_species_counts <- df_clean %>%
  count(ParkName)

# 9. Get category distribution per park
category_counts <- df_clean %>%
  count(ParkName, Category) %>%
  tidyr::spread(Category, n, fill = 0)

# 10. Define colors for categories
category_colors <- c(
  'Plants' = '#A1D6B2',
  'Vertebrates' = '#FF8A8A',
  'Invertebrates' = '#FFBE98',
  'Fungi' = '#C5705D',
  'Microorganisms' = '#91DDCF'
)

# 11. Merge species counts with category counts
df_plot <- left_join(park_species_counts, category_counts, by = "ParkName")

# 12. Normalize sizes for pie charts (ensuring they match bubbles)
min_size <- 0.2  # Minimum size for pie chart scaling
max_size <- 0.45  # Maximum size (adjust as needed)
df_plot <- df_plot %>%
  mutate(scaled_size = min_size + (n - min(n)) / (max(n) - min(n)) * (max_size - min_size))

# 13. Create base bubble plot
bubble_plot <- ggplot(df_plot, aes(x = ParkName, y = 1, size = n)) +
  geom_point(shape = 21, fill = "lightblue", color = "black", alpha = 0.6) +
  scale_size(range = c(15, 30),   # Ensures bubbles have varying sizes
             breaks = c(2000, 4000, 6000) # Ensures legend has 3 breaks 
             ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
        ) +
  labs(x = "National Parks", y = "", title = "Bubble Plot with Pie Charts: Species Distribution in Each Park")

# 14. Function to create properly scaled inset pie charts
pie_grobs <- lapply(1:nrow(df_plot), function(i) {
  park_data <- df_plot[i, ]
  pie_data <- tibble::tibble(
    Category = names(category_colors),
    Count = as.numeric(park_data[names(category_colors)])
  ) %>%
    filter(Count > 0)  # Remove zero-count categories
  
  if (nrow(pie_data) > 0) {
    pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = category_colors) +
      theme_void() +
      theme(legend.position = "none")
    
    # Ensure pie chart size is proportional to bubble size
    size_factor <- park_data$scaled_size[1] * 1.5  # Fine-tune multiplier if needed
    return(annotation_custom(ggplotGrob(pie_chart), xmin = i - size_factor, xmax = i + size_factor, ymin = 0.95, ymax = 1.05))
  } else {
    return(NULL)
  }
})

# 15. Add pie charts to the bubble plot
bubble_plot <- bubble_plot + pie_grobs

# 16. Show final plot
print(bubble_plot)

# 17. Saving final plot
ggsave("bubble_plot_species_distribution.png", plot = bubble_plot, width = 19.2, height = 10.8, dpi = 900)

