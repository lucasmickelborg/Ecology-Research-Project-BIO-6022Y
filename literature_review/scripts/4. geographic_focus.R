# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages
# ─────────────────────────────────────────────────────────────────────────────

library(dpl)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(viridis)

# ─────────────────────────────────────────────────────────────────────────────
# Load Cleaned Datasets from Data Tidying Script
# ─────────────────────────────────────────────────────────────────────────────
source("literature_review/scripts/1. data_tidying.R")

# ─────────────────────────────────────────────────────────────────────────────
# Prepare Tally of Geographic Focus
# ─────────────────────────────────────────────────────────────────────────────
prepare_geographic_tally <- function(df) {
  df %>%
    select(citation, geographic_focus) %>%
    group_by(geographic_focus) %>%
    tally(name = "study_count") %>%
    ungroup()
}

mortality_geo <- prepare_geographic_tally(tidied_mortality)
habitat_geo <- prepare_geographic_tally(tidied_habitat)
conservation_geo <- prepare_geographic_tally(tidied_conservation)

combined_geo <- bind_rows(mortality_geo, habitat_geo, conservation_geo) %>%
  group_by(geographic_focus) %>%
  summarise(study_count = sum(study_count), .groups = "drop")

# ─────────────────────────────────────────────────────────────────────────────
# Prepare World Map & Merge Data
# ─────────────────────────────────────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

world_data <- world %>%
  left_join(combined_geo, by = c("name" = "geographic_focus"))

# ─────────────────────────────────────────────────────────────────────────────
# Calculate Totals for 'Europe' & 'Global' Categories (for caption)
# ─────────────────────────────────────────────────────────────────────────────
europe_total <- combined_geo %>%
  filter(geographic_focus == "Europe") %>%
  summarise(total = sum(study_count)) %>%
  pull(total)

global_total <- combined_geo %>%
  filter(geographic_focus == "Global") %>%
  summarise(total = sum(study_count)) %>%
  pull(total)

europe_total <- ifelse(is.na(europe_total), 0, europe_total)
global_total <- ifelse(is.na(global_total), 0, global_total)

# ─────────────────────────────────────────────────────────────────────────────
# Define Colour Scale Breaks
# ─────────────────────────────────────────────────────────────────────────────
min_val <- min(combined_geo$study_count, na.rm = TRUE)
max_val <- max(combined_geo$study_count, na.rm = TRUE)
mid_val <- round((min_val + max_val) / 2)

breaks_seq <- c(min_val, mid_val, max_val)
labels_seq <- as.character(breaks_seq)

# ─────────────────────────────────────────────────────────────────────────────
# Create Map Plot
# ─────────────────────────────────────────────────────────────────────────────
geo_map <- ggplot() +
  geom_sf(data = world, fill = "grey85", color = "white", size = 0.4) +
  geom_sf(data = world_data, aes(fill = study_count), color = "black", size = 0.4) +
  scale_fill_gradientn(
    colours = viridis::plasma(6),
    trans = "log",
    na.value = "grey85",
    name = "Number of Studies",
    breaks = breaks_seq,
    labels = labels_seq,
    guide = guide_colorbar(barwidth = 1, barheight = 10)
  ) +
  labs(
    title = "Geographic Distribution of Otter Research",
    caption = paste0("Note: 'Europe' = ", europe_total, " studies; 'Global' = ", global_total, " studies.")
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0.5, face = "italic", margin = margin(t = 10))
  )

# ─────────────────────────────────────────────────────────────────────────────
# Save Plot
# ─────────────────────────────────────────────────────────────────────────────
ggsave("literature_review/figures/3. geographic_focus.pdf", geo_map, width = 12, height = 6, dpi = 300)