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
# Select Relevant Columns from Each Dataset
# ─────────────────────────────────────────────────────────────────────────────
mortality_selected    <- tidied_mortality %>% select(year, study_basis, pos_nev_0_1)
habitat_selected      <- tidied_habitat %>% select(year, study_basis, pos_nev_0_1)
conservation_selected <- tidied_conservation %>% select(year, study_basis, pos_nev_0_1)

# ─────────────────────────────────────────────────────────────────────────────
# Identify Top 3 Study Bases (Excluding 'Roadkill' in Conservation)
# ─────────────────────────────────────────────────────────────────────────────
top_mortality <- mortality_selected %>%
  count(study_basis, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(study_basis)

top_habitat <- habitat_selected %>%
  count(study_basis, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(study_basis)

top_conservation <- conservation_selected %>%
  count(study_basis, sort = TRUE) %>%
  filter(study_basis != "Roadkill") %>%
  slice_head(n = 3) %>%
  pull(study_basis)

# ─────────────────────────────────────────────────────────────────────────────
# Filter Datasets to Include Top 3 Study Bases
# ─────────────────────────────────────────────────────────────────────────────
mortality_filtered    <- mortality_selected %>% filter(study_basis %in% top_mortality)
habitat_filtered      <- habitat_selected %>% filter(study_basis %in% top_habitat)
conservation_filtered <- conservation_selected %>% filter(study_basis %in% top_conservation)

# ─────────────────────────────────────────────────────────────────────────────
# Summarise by Year, Study Basis, and Effect Type
# ─────────────────────────────────────────────────────────────────────────────
summarise_trends <- function(df, dataset_name) {
  df %>%
    group_by(year, study_basis, pos_nev_0_1) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(dataset = dataset_name)
}

mortality_trends    <- summarise_trends(mortality_filtered, "Mortality")
habitat_trends      <- summarise_trends(habitat_filtered, "Habitat")
conservation_trends <- summarise_trends(conservation_filtered, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Convert Binary Effect Code to Descriptive Labels
# ─────────────────────────────────────────────────────────────────────────────
convert_pos_neg_labels <- function(df) {
  df %>%
    mutate(pos_nev_0_1 = factor(pos_nev_0_1, levels = c(0, 1), labels = c("Positive", "Negative")))
}

mortality_trends    <- convert_pos_neg_labels(mortality_trends)
habitat_trends      <- convert_pos_neg_labels(habitat_trends)
conservation_trends <- convert_pos_neg_labels(conservation_trends)

# ─────────────────────────────────────────────────────────────────────────────
# Create Plot Function for Study Basis Trends
# ─────────────────────────────────────────────────────────────────────────────
create_stacked_bar_plot <- function(df, dataset_name) {
  ggplot(df, aes(x = year, y = count, fill = pos_nev_0_1)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.9) +
    scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
    labs(
      title = paste("Study Basis Trends in", dataset_name, "Research"),
      x = "Year",
      y = "Number of Studies",
      fill = "Effect Type"
    ) +
    facet_wrap(~study_basis, scales = "free_y") +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12)
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# Generate and Save Final Plots
# ─────────────────────────────────────────────────────────────────────────────
mortality_plot    <- create_stacked_bar_plot(mortality_trends, "Mortality")
habitat_plot      <- create_stacked_bar_plot(habitat_trends, "Habitat")
conservation_plot <- create_stacked_bar_plot(conservation_trends, "Conservation")

ggsave("literature_review/figures/6. study_basis_II/1. mortality_plot.pdf",    plot = mortality_plot, width = 10, height = 6)
ggsave("literature_review/figures/6. study_basis_II/2. habitat_plot.pdf",      plot = habitat_plot, width = 10, height = 6)
ggsave("literature_review/figures/6. study_basis_II/3. conservation_plot.pdf", plot = conservation_plot, width = 10, height = 6)
