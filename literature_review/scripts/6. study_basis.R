# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(viridis)

# ─────────────────────────────────────────────────────────────────────────────
# Load Cleaned Datasets
# ─────────────────────────────────────────────────────────────────────────────
source("literature_review/scripts/1. data_tidying.R")

# ─────────────────────────────────────────────────────────────────────────────
# Function: Prepare Study Basis Data
# ─────────────────────────────────────────────────────────────────────────────
prepare_study_basis_data <- function(df, dataset_name) {
  df %>%
    select(study_basis) %>%
    count(study_basis, name = "count") %>%
    mutate(
      count = as.integer(count),
      dataset = dataset_name
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# Function: Create Study Basis Bar Plot
# ─────────────────────────────────────────────────────────────────────────────
create_study_basis_plot <- function(df, dataset_name) {
  df$study_basis <- factor(df$study_basis, levels = df$study_basis[order(df$count, decreasing = TRUE)])
  
  ggplot(df, aes(x = study_basis, y = count, fill = study_basis)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    scale_y_continuous(breaks = seq(0, max(df$count), by = 2)) +
    labs(
      title = paste("Study Basis in", dataset_name, "Research"),
      x = "Study Basis",
      y = "Number of Studies"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 18)
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# Prepare Each Dataset
# ─────────────────────────────────────────────────────────────────────────────
mortality_basis    <- prepare_study_basis_data(tidied_mortality, "Mortality")
habitat_basis      <- prepare_study_basis_data(tidied_habitat, "Habitat")
conservation_basis <- prepare_study_basis_data(tidied_conservation, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Generate the Plots
# ─────────────────────────────────────────────────────────────────────────────
mortality_plot    <- create_study_basis_plot(mortality_basis, "Mortality")
habitat_plot      <- create_study_basis_plot(habitat_basis, "Habitat")
conservation_plot <- create_study_basis_plot(conservation_basis, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Save Plots to PDF
# ─────────────────────────────────────────────────────────────────────────────
ggsave("literature_review/figures/5. study_basis/1. mortality_basis.pdf", mortality_plot, width = 8, height = 6, dpi = 300)
ggsave("literature_review/figures/5. study_basis/2. habitat_basis.pdf", habitat_plot, width = 8, height = 6, dpi = 300)
ggsave("literature_review/figures/5. study_basis/3. conservation_basis.pdf", conservation_plot, width = 8, height = 6, dpi = 300)
