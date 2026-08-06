# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02_data_cleaning.R
# Responsible student:
# Name:
# TP Number:

# Purpose:
# This is the master data-cleaning script. It runs the four
# cleaning stages in the correct order.
# ============================================================


# Stage 1: Structure and data types
source("scripts/cleaning/02a_structure_and_types.R")


# Stage 2: Duplicates and genre standardisation
source("scripts/cleaning/02b_duplicates_and_genres.R")


# Stage 3: Invalid-value validation
source("scripts/cleaning/02c_value_validation.R")


# Stage 4: Missing values and cleaned-data export
source("scripts/cleaning/02d_missing_values_and_export.R")