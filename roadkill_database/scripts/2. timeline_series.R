# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(viridis)

# ─────────────────────────────────────────────────────────────────────────────
# Load Cleaned Dataset
# ─────────────────────────────────────────────────────────────────────────────
source("roadkill_database/scripts/1. data_tidying.R")

# ─────────────────────────────────────────────────────────────────────────────
# Aggregate Roadkill Incidents by Year
# ─────────────────────────────────────────────────────────────────────────────
roadkill_by_year <- cleaned_roadkill_dataset %>%
  group_by(start_date_year) %>%
  summarise(total_roadkill = n(), .groups = "drop")

# ─────────────────────────────────────────────────────────────────────────────
# Create Timeline Plot
# ─────────────────────────────────────────────────────────────────────────────
timeline_plot <- ggplot(roadkill_by_year, aes(x = start_date_year, y = total_roadkill)) +
  geom_line(color = viridis(1, begin = 0.3), linewidth = 1) +
  geom_point(color = viridis(1, begin = 0.7), size = 2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Annual Roadkill Incidents Involving Otters",
    x = "Year",
    y = "Total Roadkill Incidents"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ─────────────────────────────────────────────────────────────────────────────
# Save Timeline Plot
# ─────────────────────────────────────────────────────────────────────────────
ggsave("roadkill_database/figures/1. timeline_incidents.pdf",
       plot = timeline_plot, width = 10, height = 6)
