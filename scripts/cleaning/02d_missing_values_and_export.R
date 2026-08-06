# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02d_missing_values_and_export.R
# Responsible student:
# Name:
# TP Number:

# Main responsibility:
# Missing-value treatment, final cleaned-data export and
# preparation of the cleaning summary.

# Purpose:
# To examine all missing values after duplicate treatment,
# genre standardisation and invalid-value validation have been
# completed, apply the approved missing-value rules, and export
# the final cleaned Spotify dataset.
#
# Missing values must be handled at this stage because invalid
# values identified in the previous script may have been changed
# to NA.
#
# This script runs after:
# scripts/cleaning/02c_value_validation.R
#
# It must use the updated spotify_clean dataset.
# ============================================================


# Tasks for this script:
# 1. Confirm that spotify_clean exists
# 2. Record the number of rows and columns before treatment
# 3. Count missing values in every variable
# 4. Calculate the missing-value percentage for every variable
# 5. Separate missing target values from missing predictors
# 6. Identify missing values created during value validation
# 7. Remove records with missing popularity
# 8. Record how many rows were removed because popularity was missing
# 9. Apply the approved rule for missing numeric predictors
# 10. Apply the approved rule for missing categorical predictors
# 11. Avoid replacing missing values without justification
# 12. Count missing values after treatment
# 13. Record the final number of rows and columns
# 14. Confirm that the dataset is ready for cleaned-data validation
# 15. Export the final cleaned dataset
# 16. Create the overall before-and-after cleaning summary
# 17. Export all required missing-value tables
# 18. Document all treatment decisions and results
# 19. Pass the exported cleaned dataset to the final
#     cleaning-validation section in 02_data_cleaning.R


# Target-variable rule:
# Records with missing popularity should normally be removed
# because popularity is the main target variable and cannot be
# used for analysis or prediction when it is missing.


# Numeric-predictor treatment:
# The group must approve whether each analysis will use:
#
# 1. Complete cases
# 2. Median imputation
# 3. Another justified method
#
# Do not automatically apply one method to all variables.


# Categorical-predictor treatment:
# The group must decide whether missing categories should:
#
# 1. Be removed from a specific analysis
# 2. Be recorded as an Unknown category
# 3. Be handled using another justified method


# Important modelling note:
# Any imputation used specifically for prediction modelling
# should later be calculated from the training data only.
# The testing data must not be used to calculate imputation
# values because that would cause data leakage.


# Planned outputs:
# outputs/tables/missing_values_before_treatment.csv
# outputs/tables/missing_values_after_treatment.csv
# outputs/tables/rows_removed_for_missing_target.csv
# outputs/tables/final_cleaning_summary.csv
# data/processed/spotify_tracks_clean.csv


# Documentation file:
# report/cleaning_documentation/
# member4_missing_values_and_export.md


# Documentation must explain:
# 1. Missing counts and percentages before treatment
# 2. Which missing values existed in the original dataset
# 3. Which missing values were created from invalid values
# 4. How missing popularity records were handled
# 5. How missing numeric predictors were handled
# 6. How missing categorical predictors were handled
# 7. Why each treatment method was selected
# 8. The number of rows before and after treatment
# 9. The number of missing values before and after treatment
# 10. The location of the exported cleaned dataset
# 11. The limitations of the selected missing-value strategy


# Important:
# Do not change approved duplicate decisions in this script.
# Do not change approved genre mappings in this script.
# Do not create new value-validation rules in this script.
# Do not validate the full assignment in this script.
#
# This script exports the cleaned dataset.
# The final cleaned-data validation will run after this script
# at the end of scripts/02_data_cleaning.R.