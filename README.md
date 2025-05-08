# README

# Project Metadata
# Project Title: Otters as Bioindicators: Literature Review and Roadkill Data Analysis
# Author: Lucas Finn Mickelborg
# Institution: University of East Anglia
# Supervisor(s): Martin Taylor, Ian Barr
# Date: Jan-May 2025
# R Version: R version 4.4.3 (2025-02-28)
# RStudio Version: 2024.4.2.764.1

# Project Description
# This project investigates the ecology and conservation challenges faced by Eurasian otters (Lutra lutra) by synthesising
# a systematic literature review and analysing roadkill records from Norfolk, UK.
# The aim was to identify major mortality factors, habitat associations, and conservation effectiveness using
# two complementary approaches: published studies and observational mortality records.
# All data analysis and visualisation were conducted in R using reproducible scripts,
# and all figures follow accessibility guidelines for colourblind readers.

# Directory Structure
# /literature_review/
#   ├── /scripts/
#       - 1. data_tidying.R
#       - 2. year.R
#       - 3. study_type.R
#       - 4. geographic_focus.R
#       - 5. study_basis.R
#       - 6. study_basis II.R
#   ├── /figures/
#       - Output figures for literature analysis
#   ├── /data/
#       - 1. Mortality.csv
#       - 2. Habitat.csv
#       - 3. Conservation.csv
#
# /roadkill_database/
#   ├── /scripts/
#       - 1. data_tidying.R
#       - 2. timeline_series.R
#       - 3. spatial_analysis.R
#       - 4. road_analysis.R
#   ├── /figures/
#       - Timeline plots, spatial maps, road type plots
#   ├── /data/
#       - records-2025-01-09.csv (roadkill otter records)
#       - CTYUA_DEC_2024_UK_BFC.shp (county shapefile for Norfolk filtering)

# Software Requirements
# - R (recommended version 4.2 or later)
# - RStudio (latest stable version)

# Key R Packages Used:
# dplyr         - Data manipulation and summarisation
# ggplot2       - Data visualisation (plots, bar charts, line graphs)
# sf            - Handling and plotting spatial (GIS) data
# osmdata       - Downloading and importing OpenStreetMap road network data
# viridis       - Colourblind-friendly colour palettes for plots
# readr         - Fast and efficient data import (CSV files)
# janitor        - Cleaning column names and tidy tabular data
# stringr        - String manipulation and pattern matching
# rnaturalearth  - Country shapefiles for mapping (Natural Earth)
# rnaturalearthdata - Base map data from Natural Earth

# Data Sources
# - Literature Review: Systematic searches across Web of Science, Scopus, PubMed, Google Scholar (Jan–Feb 2025).
# -  Three tables were created: Mortality, Habitat, Conservation.
# - Roadkill Data: Downloaded from NBN Atlas (The Road Lab UK dataset) on 9 January 2025 (records-2025-01-09.csv).
## Required Data

# The shapefile `CTYUA_DEC_2024_UK_BFC.shp` (used for spatial joins) is available here:  
[Download shapefile from Google Drive](https://drive.google.com/file/d/1IBQd8FkjQPuHBb1elxbOO_uvQSXTmFSX/view?usp=sharing)

# > Note: This file is excluded from the GitHub repo due to GitHub's 100MB file limit.
# Download from link and upload into (roadkill_database/data/counties_shapefile) and ensure file name matches `CTYUA_DEC_2024_UK_BFC.shp`.

# How to Reproduce the Analysis

# 1. Literature Review Analysis
# - Navigate to /literature_review/scripts/
# - Run the scripts in order:
#   1. 1. data_tidying.R: Prepare and clean Mortality, Habitat, and Conservation datasets.
#   2. 2. year.R: Summarise number of studies published per year and produce temporal trends plot.
#   3. 3. study_type.R: Summarise and plot study types across themes.
#   4. 4. geographic_focus.R: Create a choropleth map showing study locations.
#   5. 5. study_basis.R and 6. study basis II.R: Analyse effect direction trends (positive/negative, old/recent classification).

# 2. Roadkill Database Analysis
# - Navigate to /roadkill_database/scripts/
# - Run the scripts in order:
#   1. 1. data_tidying.R: Filter otter records, clean data, join points to Norfolk county shapefile.
#   2. 2. timeline_series.R: Summarise and plot the number of roadkill incidents per year (2014–2023).
#   3. 3. spatial_analysis.R: Map spatial distribution of incidents overlaid on major Norfolk road networks.
#   4. 4. road_analysis.R: Create bar plots comparing incident frequency by road class.

# Notes and Best Practices
# - Data sensitivity: All roadkill data used were publicly available and anonymised.
# - Accessibility: All plots use viridis colour palettes to ensure readability for colourblind users.
# - Statistical Analysis: This project focused on descriptive visualisation and trend analysis,
#   not formal hypothesis testing, due to the observational nature of the data.
# - Reproducibility: Scripts are modular and documented for ease of re-running the analysis if data are updated or expanded.
