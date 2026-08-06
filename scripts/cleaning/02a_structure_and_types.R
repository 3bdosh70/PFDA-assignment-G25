# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02a_structure_and_types.R
# Responsible student:
# Name:
# TP Number:

# Main responsibility:
# Data structure, column checking and data-type correction.

# Additional cleaning responsibility:
# After all data-cleaning stages have been completed, the same
# student will validate the final cleaned dataset to confirm
# that the cleaning process was completed correctly.
#
# This validation is only for the data-cleaning section.
# It does not include validating the complete assignment,
# analysis objectives, models, report or other project files.
#
# The cleaned-data validation should be written at the end of:
# scripts/02_data_cleaning.R
#
# It must run only after:
# 02a_structure_and_types.R
# 02b_duplicates_and_genres.R
# 02c_value_validation.R
# 02d_missing_values_and_export.R

# Purpose:
# To create a working copy of the original Spotify dataset,
# inspect its structure, remove the unnecessary row-number
# column, confirm that all required variables are available,
# and convert variables into suitable R data types.
#
# This script handles only the structure and data-type stage.
# Missing values, duplicated records, genre standardisation and
# invalid numerical values are handled in later cleaning scripts.
# ============================================================


# Tasks for this script:
# 1. Confirm that spotify_raw exists
# 2. Create a working copy called spotify_clean
# 3. Record the original number of rows and columns
# 4. Identify and remove the unnecessary row-number column
# 5. Confirm that all expected Spotify variables exist
# 6. Standardise column names where necessary
# 7. Inspect the original data type of every variable
# 8. Convert variables into suitable R data types
# 9. Confirm that popularity is the target variable
# 10. Create a structure-and-types audit table
# 11. Export the structure-and-types audit table
# 12. Document all decisions, code and results
# 13. Pass spotify_clean to the next cleaning stage


# Variables that should normally remain character:
# track_id
# artists
# album_name
# track_name
# track_genre


# Variables that should normally remain numeric:
# popularity
# duration_ms
# danceability
# energy
# loudness
# speechiness
# acousticness
# instrumentalness
# liveness
# valence
# tempo


# Variables that should be reviewed as categorical:
# explicit
# key
# mode
# time_signature


# Planned output from this script:
# outputs/tables/structure_and_types_audit.csv


# Documentation file:
# report/cleaning_documentation/
# member1_structure_types_and_cleaning_validation.md


# Cleaned-data validation tasks to complete later:
# These checks must be performed only after every cleaning
# script has finished.

# 1. Confirm that the final cleaned dataset exists
# 2. Record the final number of rows and columns
# 3. Confirm that the unnecessary index column was removed
# 4. Confirm that all required columns still exist
# 5. Confirm that column data types are correct
# 6. Confirm that missing popularity values were handled
# 7. Confirm that exact duplicates were handled
# 8. Confirm that invalid values were handled
# 9. Confirm that genre labels were standardised
# 10. Confirm that the cleaned dataset was exported successfully
# 11. Compare the dataset before and after cleaning
# 12. Create a cleaned-data validation results table
# 13. Document any remaining issues or limitations


# Planned cleaned-data validation output:
# outputs/tables/cleaned_data_validation_results.csv


# Important:
# Do not handle missing values in this script.
# Do not remove duplicated tracks in this script.
# Do not standardise genre labels in this script.
# Do not replace invalid values in this script.
# Do not export the final cleaned dataset in this script.
# Do not validate the entire assignment.