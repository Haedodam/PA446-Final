# ==========================================================
# STEP 0: LOAD REQUIRED PACKAGES
# ==========================================================
# This script loads all R packages used across Steps 2–5.
# It is recommended to source this file at the beginning of
# each step script and at the top of the Quarto report.
# Example:
#     source("scripts/step0_load_packages.R")
# ==========================================================

# Data access & wrangling
library(tidycensus)
library(dplyr)
library(tidyverse)
library(rvest)
library(stringr)
library(readr)

# Missingness visualization
library(naniar)

# Modeling & tidying model output
library(broom)

# Spatial data & mapping
library(tigris)
library(sf)
library(ggplot2)

# Interactive dashboard
library(shiny)
library(DT)
library(bslib)

# Machine learning (Decision Tree)
library(rpart)
library(rpart.plot)

# Geospatial caching
options(tigris_use_cache = TRUE)

# END STEP 0