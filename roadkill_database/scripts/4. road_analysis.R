# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages (Ordered by Use)
# ─────────────────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(osmdata)
library(ggplot2)
library(viridis)

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
# Download and Transform Major Roads from OpenStreetMap
# ─────────────────────────────────────────────────────────────────────────────
norfolk_bbox <- st_bbox(st_transform(norfolk, 4326))

roads <- opq(bbox = norfolk_bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

roads_lines <- st_transform(roads$osm_lines, crs = st_crs(norfolk))

# ─────────────────────────────────────────────────────────────────────────────
# Plot: Roadkill by Road Type in 2014
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2014 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2014) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

nearest_2014 <- st_nearest_feature(roadkill_2014, roads_lines)

roadkill_2014 <- roadkill_2014 %>%
  mutate(road_type = roads_lines$highway[nearest_2014]) %>%
  filter(!is.na(road_type)) %>%
  mutate(road_type = factor(road_type, levels = c("motorway", "trunk", "primary", "secondary")))

plot_2014 <- ggplot(roadkill_2014, aes(x = road_type, fill = road_type)) +
  geom_bar(colour = "black") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) +
  labs(title = "Roadkill by Road Type (Norfolk, 2014)",
       x = "Type of Road", y = "Number of Incidents") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("roadkill_database/figures/3. road_analysis/1. 2014_norfolk_roads.pdf", plot = plot_2014, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: Roadkill by Road Type in 2016
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2016 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2016) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

nearest_2016 <- st_nearest_feature(roadkill_2016, roads_lines)

roadkill_2016 <- roadkill_2016 %>%
  mutate(road_type = roads_lines$highway[nearest_2016]) %>%
  filter(!is.na(road_type)) %>%
  mutate(road_type = factor(road_type, levels = c("motorway", "trunk", "primary", "secondary")))

plot_2016 <- ggplot(roadkill_2016, aes(x = road_type, fill = road_type)) +
  geom_bar(colour = "black") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) +
  labs(title = "Roadkill by Road Type (Norfolk, 2016)",
       x = "Type of Road", y = "Number of Incidents") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("roadkill_database/figures/3. road_analysis/2. 2016_norfolk_roads.pdf", plot = plot_2016, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: Roadkill by Road Type in 2018
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2018 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2018) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

nearest_2018 <- st_nearest_feature(roadkill_2018, roads_lines)

roadkill_2018 <- roadkill_2018 %>%
  mutate(road_type = roads_lines$highway[nearest_2018]) %>%
  filter(!is.na(road_type)) %>%
  mutate(road_type = factor(road_type, levels = c("motorway", "trunk", "primary", "secondary")))

plot_2018 <- ggplot(roadkill_2018, aes(x = road_type, fill = road_type)) +
  geom_bar(colour = "black") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) +
  labs(title = "Roadkill by Road Type (Norfolk, 2018)",
       x = "Type of Road", y = "Number of Incidents") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("roadkill_database/figures/3. road_analysis/3. 2018_norfolk_roads.pdf", plot = plot_2018, width = 8, height = 6)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: Roadkill by Road Type in 2020
# ─────────────────────────────────────────────────────────────────────────────
roadkill_2020 <- cleaned_roadkill_dataset %>%
  filter(start_date_year == 2020) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(norfolk)) %>%
  st_join(norfolk) %>%
  filter(!is.na(CTYUA24NM))

nearest_2020 <- st_nearest_feature(roadkill_2020, roads_lines)

roadkill_2020 <- roadkill_2020 %>%
  mutate(road_type = roads_lines$highway[nearest_2020]) %>%
  filter(!is.na(road_type)) %>%
  mutate(road_type = factor(road_type, levels = c("motorway", "trunk", "primary", "secondary")))

plot_2020 <- ggplot(roadkill_2020, aes(x = road_type, fill = road_type)) +
  geom_bar(colour = "black") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.9) +
  labs(title = "Roadkill by Road Type (Norfolk, 2020)",
       x = "Type of Road", y = "Number of Incidents") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("roadkill_database/figures/3. road_analysis/4. 2020_norfolk_roads.pdf", plot = plot_2020, width = 8, height = 6)
