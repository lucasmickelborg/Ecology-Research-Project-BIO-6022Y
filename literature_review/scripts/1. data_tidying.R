# ─────────────────────────────────────────────────────────────────────────────
# Clear Environment
# ─────────────────────────────────────────────────────────────────────────────
rm(list = ls())

# ─────────────────────────────────────────────────────────────────────────────
# Load Required Packages (Ordered by Use)
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(janitor)
library(stringr)
library(readr)

# ─────────────────────────────────────────────────────────────────────────────
# Load Raw Datasets (Unmodified)
# ─────────────────────────────────────────────────────────────────────────────
mortality_data <- read.csv("literature_review/data/1. Mortality.csv") %>% clean_names()
habitat_data <- read.csv("literature_review/data/2. Habitat.csv") %>% clean_names()
conservation_data <- read.csv("literature_review/data/3. Conservation.csv") %>% clean_names()

# ─────────────────────────────────────────────────────────────────────────────
# Check Categorical Columns
# ─────────────────────────────────────────────────────────────────────────────
check_discrete_columns <- function(df, dataset_name) {
  cat("\n--- Checking Discrete Columns for", dataset_name, "---\n")
  cat("\nStudy Types:\n"); print(unique(df$study_type))
  cat("\nStudy Basis:\n"); print(unique(df$study_basis))
  cat("\nYear:\n"); print(unique(df$year))
  cat("\nGeographic Focus:\n"); print(unique(df$geographic_focus))
  cat("\nPos/Nev:\n"); print(unique(df$pos_nev))
  cat("\nYear Pos/Nev:\n"); print(unique(df$year_posnev))
}

# Run checks before cleaning
check_discrete_columns(mortality_data, "Mortality")
check_discrete_columns(habitat_data, "Habitat")
check_discrete_columns(conservation_data, "Conservation")

# ─────────────────────────────────────────────────────────────────────────────
# Cleaning: Study Types
# ─────────────────────────────────────────────────────────────────────────────
clean_study_type <- function(df) {
  df %>%
    mutate(study_type = str_trim(study_type),
           study_type = str_to_lower(study_type),
           study_type = recode(study_type,
                               "meta analysis" = "Meta-Analysis",
                               "meta-analysis" = "Meta-Analysis",
                               "review and analysis" = "Review",
                               "post mortem study" = "Postmortem Study",
                               "postmortem study" = "Postmortem Study",
                               "long term monitoring study" = "Long-Term Monitoring",
                               "long-term monitoring" = "Long-Term Monitoring",
                               "long-term monitoring study" = "Long-Term Monitoring",
                               "conference proceeding" = "Conference Proceedings",
                               "conference proceedings" = "Conference Proceedings",
                               "economic study" = "Economic Analysis",
                               "economic analysis" = "Economic Analysis",
                               "population study" = "Population Survey",
                               "population survey" = "Population Survey",
                               "phd dissertation" = "Academic Dissertation",
                               "master’s dissertation" = "Academic Dissertation",
                               "field study" = "Field-Based Study",
                               "genetic study" = "Field-Based Study",
                               "habitat suitability modeling" = "Field-Based Study",
                               "spraint analysis" = "Field-Based Study",
                               "molecular study" = "Field-Based Study",
                               "observational study" = "Observational Study"
           ),
           study_type = str_to_title(study_type))
}

# ─────────────────────────────────────────────────────────────────────────────
# Cleaning: Study Basis
# ─────────────────────────────────────────────────────────────────────────────
standardise_study_basis <- function(df) {
  df %>%
    mutate(study_basis = str_trim(study_basis),
           study_basis = str_to_lower(study_basis),
           study_basis = case_when(
             study_basis %in% c("diet + habitat", "habitat + diet") ~ "Diet",
             study_basis %in% c("prey availability + water quality", "prey diversity + habitat use", 
                                "prey diversity + habitat protection", "prey availability + long-term monitoring",
                                "habitat structure + prey availability", "prey availability + habitat connectivity") ~ "Prey",
             study_basis %in% c("habitat structure", "habitat stability", "habitat conservation", "habitat suitability") ~ "Habitat",
             study_basis %in% c("habitat connectivity", "habitat connectivity + road impact",
                                "habitat connectivity + seasonal habitat use", "habitat connectivity + human disturbance",
                                "habitat connectivity + prey availability") ~ "Connectivity",
             study_basis %in% c("urban habitat", "urban habitat connectivity", "urban habitat use + water quality", "urban habitat management") ~ "Urban",
             study_basis %in% c("riparian habitat conservation", "riparian restoration + pollution control") ~ "Freshwater",
             study_basis %in% c("freshwater access in marine habitats", "freshwater habitat protection", 
                                "coastal vs freshwater habitat") ~ "Freshwater",
             study_basis %in% c("marine habitat protection") ~ "Marine",
             study_basis %in% c("water quality", "water quality + habitat restoration", "habitat structure + water quality") ~ "Water",
             study_basis %in% c("toxicology & pollution reduction", "pollution control", "pollution control + habitat restoration",
                                "pollution control in reintroduction areas") ~ "Pollution",
             study_basis %in% c("roadkill", "roadkill prevention", "roadkill prevention + pollution control", 
                                "roadkill prevention + fishing trap management") ~ "Roadkill",
             study_basis %in% c("reintroduction", "translocation success & habitat suitability") ~ "Reintroduction",
             study_basis %in% c("captive breeding + habitat protection") ~ "Breeding",
             study_basis %in% c("economic analysis of conservation efforts") ~ "Economics",
             study_basis %in% c("population recovery", "population monitoring + habitat protection", "reproduction study") ~ "Population",
             study_basis %in% c("wildlife corridors & genetic diversity") ~ "Corridors",
             study_basis %in% c("invasive species control + habitat restoration", "habitat structure + invasive species") ~ "Invasive",
             study_basis %in% c("camera-trap conservation monitoring", "habitat monitoring with spraint surveys") ~ "Monitoring",
             study_basis %in% c("habitat + prey availability") ~ "Habitat",
             study_basis %in% c("habitat + water quality") ~ "Water",
             study_basis %in% c("habitat + bioindicator analysis") ~ "Monitoring",
             study_basis %in% c("habitat restoration", "restoration", "habitat restoration + water management", 
                                "habitat restoration + pollution control", "habitat restoration + prey protection", 
                                "habitat restoration + hydropower impact", "habitat restoration + population monitoring") ~ "Habitat",
             study_basis %in% c("habitat structure + human disturbance") ~ "Disturbance",
             study_basis %in% c("seasonal habitat use") ~ "Seasonal",
             TRUE ~ study_basis
           ),
           study_basis = str_to_title(study_basis))
}

# ─────────────────────────────────────────────────────────────────────────────
# Cleaning: Geographic Focus
# ─────────────────────────────────────────────────────────────────────────────
standardise_geographic_focus <- function(df) {
  df %>%
    mutate(geographic_focus = tolower(geographic_focus),
           geographic_focus = case_when(
             geographic_focus %in% c("uk", "united kingdom", "britain", "england", "scotland", "wales") ~ "United Kingdom",
             geographic_focus %in% c("usa", "united states", "united states of america") ~ "United States",
             geographic_focus %in% c("russia", "russian federation") ~ "Russia",
             geographic_focus %in% c("south korea", "korea", "republic of korea") ~ "South Korea",
             geographic_focus %in% c("czechia", "czech republic") ~ "Czechia",
             geographic_focus %in% c("iran, islamic republic of", "iran") ~ "Iran",
             geographic_focus %in% c("israel", "state of israel") ~ "Israel",
             TRUE ~ tools::toTitleCase(geographic_focus)))
}

# ─────────────────────────────────────────────────────────────────────────────
# Apply Cleaning Functions to All Datasets
# ─────────────────────────────────────────────────────────────────────────────
tidied_mortality <- mortality_data %>%
  clean_study_type() %>%
  standardise_study_basis() %>%
  standardise_geographic_focus()

tidied_habitat <- habitat_data %>%
  clean_study_type() %>%
  standardise_study_basis() %>%
  standardise_geographic_focus()

tidied_conservation <- conservation_data %>%
  clean_study_type() %>%
  standardise_study_basis() %>%
  standardise_geographic_focus()

# ─────────────────────────────────────────────────────────────────────────────
# Final Checks
# ─────────────────────────────────────────────────────────────────────────────
cat("\n✔ Study Basis, Study Type & Geographic Focus standardisation complete.\n")

check_discrete_columns(tidied_mortality, "Mortality (Cleaned)")
check_discrete_columns(tidied_habitat, "Habitat (Cleaned)")
check_discrete_columns(tidied_conservation, "Conservation (Cleaned)")
