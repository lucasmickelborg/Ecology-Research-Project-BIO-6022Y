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
# Prepare Study Type Data
# ─────────────────────────────────────────────────────────────────────────────
prepare_study_type_data <- function(df, dataset_name) {
  df %>%
    select(citation, study_type) %>%
    group_by(study_type) %>%
    tally(name = "count") %>%
    mutate(dataset = dataset_name)
}

# Apply to all datasets
mortality_study_type <- prepare_study_type_data(tidied_mortality, "Mortality")
habitat_study_type <- prepare_study_type_data(tidied_habitat, "Habitat")
conservation_study_type <- prepare_study_type_data(tidied_conservation, "Conservation")

# Combine datasets
combined_study_type <- bind_rows(mortality_study_type, habitat_study_type, conservation_study_type)
combined_study_type$dataset <- factor(combined_study_type$dataset, levels = c("Mortality", "Habitat", "Conservation"))

# ─────────────────────────────────────────────────────────────────────────────
# Order Study Types by Frequency
# ─────────────────────────────────────────────────────────────────────────────
study_type_order <- combined_study_type %>%
  group_by(study_type) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  pull(study_type)

combined_study_type$study_type <- factor(combined_study_type$study_type, levels = study_type_order)

# ─────────────────────────────────────────────────────────────────────────────
# Create Final Plot
# ─────────────────────────────────────────────────────────────────────────────
stacked_bar_chart <- ggplot(combined_study_type, aes(x = study_type, y = count, fill = dataset)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_viridis_d(option = "viridis") +
  labs(
    title = "Distribution of Study Types by Research Theme",
    x = "Study Type",
    y = "Number of Studies"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18)
  )

# ─────────────────────────────────────────────────────────────────────────────
# Save Plot
# ─────────────────────────────────────────────────────────────────────────────
ggsave("literature_review/figures/2. study_types.pdf", stacked_bar_chart, width = 10, height = 6, dpi = 300)