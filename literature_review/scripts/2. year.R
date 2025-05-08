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
# Prepare Data: Tally Studies Per Year Per Dataset
# ─────────────────────────────────────────────────────────────────────────────
prepare_temporal_data <- function(df, dataset_name) {
  df %>%
    select(citation, year) %>%
    group_by(year) %>%
    tally(name = "count") %>%
    mutate(dataset = dataset_name)
}

mortality_tally <- prepare_temporal_data(tidied_mortality, "Mortality")
habitat_tally <- prepare_temporal_data(tidied_habitat, "Habitat")
conservation_tally <- prepare_temporal_data(tidied_conservation, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Combine All Tallied Data
# ─────────────────────────────────────────────────────────────────────────────
combined_tally <- bind_rows(mortality_tally, habitat_tally, conservation_tally)
combined_tally$dataset <- factor(combined_tally$dataset, levels = c("Mortality", "Habitat", "Conservation"))

# ─────────────────────────────────────────────────────────────────────────────
# Create Final Plot of Temporal Trends
# ─────────────────────────────────────────────────────────────────────────────
temporal_plot <- ggplot(combined_tally, aes(x = year, y = count)) +
  geom_line(aes(color = dataset), linewidth = 1) +
  geom_point(aes(color = dataset), size = 2) +
  geom_smooth(aes(color = dataset), method = "loess", se = FALSE, linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Mortality" = "#440154FF",     # Viridis Purple
      "Habitat" = "#21908CFF",       # Viridis Teal
      "Conservation" = "#FDE725FF"   # Viridis Yellow
    )
  ) +
  facet_grid(rows = vars(dataset)) +
  labs(
    title = "Temporal Trends in Otter Research",
    x = "Year",
    y = "Number of Studies"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),
    axis.text.x = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 18)
  ) +
  scale_y_continuous(limits = c(0, max(combined_tally$count) + 2))

# ─────────────────────────────────────────────────────────────────────────────
# Save and Display Final Plot
# ─────────────────────────────────────────────────────────────────────────────
ggsave("literature_review/figures/1. temporal_trends.pdf", plot = temporal_plot, width = 10, height = 7, dpi = 300)