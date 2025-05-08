# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(viridis)

# ─────────────────────────────────────────────────────────────────────────────
# Load Cleaned Datasets from Data Tidying Script
# ─────────────────────────────────────────────────────────────────────────────
source("literature_review/scripts/1. data_tidying.R")

# ─────────────────────────────────────────────────────────────────────────────
# Function to Prepare Effects Data per Dataset
# ─────────────────────────────────────────────────────────────────────────────
prepare_effects_data <- function(df, dataset_name) {
  df %>%
    select(citation, year_posnev) %>%
    group_by(year_posnev) %>%
    tally(name = "count") %>%
    mutate(dataset = dataset_name)
}

# ─────────────────────────────────────────────────────────────────────────────
# Apply Function Across All Three Datasets
# ─────────────────────────────────────────────────────────────────────────────
mortality_effects     <- prepare_effects_data(tidied_mortality, "Mortality")
habitat_effects       <- prepare_effects_data(tidied_habitat, "Habitat")
conservation_effects  <- prepare_effects_data(tidied_conservation, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Combine + Format Dataset for Plotting
# ─────────────────────────────────────────────────────────────────────────────
combined_effects <- bind_rows(mortality_effects, habitat_effects, conservation_effects)

combined_effects$dataset <- factor(combined_effects$dataset, levels = c("Mortality", "Habitat", "Conservation"))

effect_labels <- c("1" = "Old/Negative", "2" = "Old/Positive", "3" = "Recent/Negative", "4" = "Recent/Positive")
combined_effects$year_posnev <- factor(combined_effects$year_posnev, levels = names(effect_labels), labels = effect_labels)

# ─────────────────────────────────────────────────────────────────────────────
# Create Final Faceted Plot (Discrete Viridis Colour Scale)
# ─────────────────────────────────────────────────────────────────────────────
effect_plot <- ggplot(combined_effects, aes(x = year_posnev, y = count, fill = year_posnev)) +
  geom_bar(stat = "identity", width = 0.6) +
  facet_grid(rows = vars(dataset)) +
  labs(
    title = "Positive/Negative Effects in Otter Research",
    x = "Effect Category",
    y = "Number of Studies"
  ) +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 18)
  ) +
  scale_y_continuous(limits = c(0, max(combined_effects$count) + 5))

# ─────────────────────────────────────────────────────────────────────────────
# Save Final Plot to Figures Folder
# ─────────────────────────────────────────────────────────────────────────────
ggsave("literature_review/figures/4. effect_trends.pdf", effect_plot, width = 10, height = 7, dpi = 300)