# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages (Ordered by Use)
# ─────────────────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(osmdata)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# Load Cleaned Dataset
# ─────────────────────────────────────────────────────────────────────────────
source("roadkill_database/scripts/1. data_tidying.R")

# ─────────────────────────────────────────────────────────────────────────────
# Load Norfolk Boundary from Shapefile
# ─────────────────────────────────────────────────────────────────────────────
shapefile_path <- "roadkill_database/data/counties_shapefile/CTYUA_DEC_2024_UK_BFC.shp"
counties <- st_read(shapefile_path)
norfolk <- counties %>% filter(CTYUA24NM == "Norfolk")

# ─────────────────────────────────────────────────────────────────────────────
# Download Major Roads from OpenStreetMap
# ─────────────────────────────────────────────────────────────────────────────
norfolk_bbox <- st_bbox(st_transform(norfolk, 4326))

roads <- opq(bbox = norfolk_bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

roads_lines <- roads$osm_lines

# ─────────────────────────────────────────────────────────────────────────────
# Plot: 2014 Roadkill Incidents
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2014 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2014) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

plot_2014 <- ggplot() +
  geom_sf(data = norfolk, fill = "grey95", color = "black") +
  geom_sf(data = roads_lines, color = "grey40", size = 0.3) +
  geom_sf(data = roadkill_2014, colour = viridis(1, begin = 0.5), shape = 16, size = 2) +
  labs(
    title = "Roadkill Incidents in 2014 — Norfolk (Major Roads)",
    caption = "Data: The Road Lab UK & OpenStreetMap"
  ) +
  theme_minimal()

ggsave("roadkill_database/figures/2. spatial_analysis/1. 2014_norfolk_map.pdf", plot = plot_2014, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: 2016 Roadkill Incidents
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2016 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2016) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

plot_2016 <- ggplot() +
  geom_sf(data = norfolk, fill = "grey95", color = "black") +
  geom_sf(data = roads_lines, color = "grey40", size = 0.3) +
  geom_sf(data = roadkill_2016, colour = viridis(1, begin = 0.5), shape = 16, size = 2) +
  labs(
    title = "Roadkill Incidents in 2016 — Norfolk (Major Roads)",
    caption = "Data: The Road Lab UK & OpenStreetMap"
  ) +
  theme_minimal()

ggsave("roadkill_database/figures/2. spatial_analysis/2. 2016_norfolk_map.pdf", plot = plot_2016, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: 2018 Roadkill Incidents
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2018 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2018) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

plot_2018 <- ggplot() +
  geom_sf(data = norfolk, fill = "grey95", color = "black") +
  geom_sf(data = roads_lines, color = "grey40", size = 0.3) +
  geom_sf(data = roadkill_2018, colour = viridis(1, begin = 0.5), shape = 16, size = 2) +
  labs(
    title = "Roadkill Incidents in 2018 — Norfolk (Major Roads)",
    caption = "Data: The Road Lab UK & OpenStreetMap"
  ) +
  theme_minimal()

ggsave("roadkill_database/figures/2. spatial_analysis/3. 2018_norfolk_map.pdf", plot = plot_2018, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: 2020 Roadkill Incidents
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2020 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2020) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

plot_2020 <- ggplot() +
  geom_sf(data = norfolk, fill = "grey95", color = "black") +
  geom_sf(data = roads_lines, color = "grey40", size = 0.3) +
  geom_sf(data = roadkill_2020, colour = viridis(1, begin = 0.5), shape = 16, size = 2) +
  labs(
    title = "Roadkill Incidents in 2020 — Norfolk (Major Roads)",
    caption = "Data: The Road Lab UK & OpenStreetMap"
  ) +
  theme_minimal()

ggsave("roadkill_database/figures/2. spatial_analysis/4. 2020_norfolk_map.pdf", plot = plot_2020, width = 8, height = 6)
