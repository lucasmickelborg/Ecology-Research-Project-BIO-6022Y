# ─────────────────────────────────────────────────────────────────────────────
# Clear Environment
# ─────────────────────────────────────────────────────────────────────────────
rm(list = ls())

# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages (Ordered by Use)
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(janitor)

# ─────────────────────────────────────────────────────────────────────────────
# Load the Raw Roadkill Dataset
# ─────────────────────────────────────────────────────────────────────────────
raw_roadkill_dataset <- read.csv("roadkill_database/data/roadkill_dataset/records-2025-01-09.csv")

# ─────────────────────────────────────────────────────────────────────────────
# Clean Column Names
# ─────────────────────────────────────────────────────────────────────────────
cleaned_roadkill_dataset <- raw_roadkill_dataset %>%
  janitor::clean_names()

# ─────────────────────────────────────────────────────────────────────────────
# Inspect the Raw Dataset
# ─────────────────────────────────────────────────────────────────────────────
summary(cleaned_roadkill_dataset)
glimpse(cleaned_roadkill_dataset)

# ─────────────────────────────────────────────────────────────────────────────
# Filter for Complete Geographic Records
# ─────────────────────────────────────────────────────────────────────────────
cleaned_roadkill_dataset <- cleaned_roadkill_dataset %>%
  filter(!is.na(latitude_wgs84) & !is.na(longitude_wgs84))

# ─────────────────────────────────────────────────────────────────────────────
# Standardise Data Types
# ─────────────────────────────────────────────────────────────────────────────
cleaned_roadkill_dataset <- cleaned_roadkill_dataset %>%
  mutate(
    record_id = as.character(nbn_atlas_record_id),
    vitality = as.factor(vitality),
    country = as.factor(country),
    latitude = as.numeric(latitude_wgs84),
    longitude = as.numeric(longitude_wgs84)
  )

# ─────────────────────────────────────────────────────────────────────────────
# Remove Duplicate Records
# ─────────────────────────────────────────────────────────────────────────────
cleaned_roadkill_dataset <- cleaned_roadkill_dataset %>%
  distinct()

# ─────────────────────────────────────────────────────────────────────────────
# Subset to Relevant Columns
# ─────────────────────────────────────────────────────────────────────────────
cleaned_roadkill_dataset <- cleaned_roadkill_dataset %>%
  select(record_id, scientific_name, common_name, vitality, osgr_1km,
         latitude, longitude, country, start_date_year)

# ─────────────────────────────────────────────────────────────────────────────
# Check That All Records Are Confirmed as Dead
# ─────────────────────────────────────────────────────────────────────────────
unique_vitality <- unique(cleaned_roadkill_dataset$vitality)
print("Unique values in the 'vitality' column:")
print(unique_vitality)

# ─────────────────────────────────────────────────────────────────────────────
# Final Inspection of Cleaned Dataset
# ─────────────────────────────────────────────────────────────────────────────
summary(cleaned_roadkill_dataset)
head(cleaned_roadkill_dataset)
