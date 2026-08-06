# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02c_value_validation.R
# Responsible student:
# Name:
# TP Number:

# Main responsibility:
# Numerical and categorical value validation, invalid-value
# treatment and outlier investigation.

# Purpose:
# To check whether the values stored in each Spotify variable
# follow the approved valid ranges or categories, identify
# impossible values, and replace confirmed invalid values with
# NA so they can be handled in the missing-value stage.
#
# This script must distinguish between:
#
# 1. Invalid or impossible values
# 2. Unusual but potentially valid outliers
#
# An unusual value must not be removed only because it is far
# from the average. A value should be treated as invalid only
# when it breaks an approved logical or documented rule.
#
# This script runs after:
# scripts/cleaning/02b_duplicates_and_genres.R
#
# It must use the updated spotify_clean dataset.
# ============================================================


# Tasks for this script:
# 1. Confirm that spotify_clean exists
# 2. Identify the variables requiring value validation
# 3. Research or confirm the valid rule for each variable
# 4. Record the source or justification for every rule
# 5. Validate the popularity target variable
# 6. Validate duration_ms
# 7. Validate danceability
# 8. Validate energy
# 9. Validate loudness
# 10. Validate speechiness
# 11. Validate acousticness
# 12. Validate instrumentalness
# 13. Validate liveness
# 14. Validate valence
# 15. Validate tempo
# 16. Validate key
# 17. Validate mode
# 18. Validate time_signature
# 19. Count invalid values for every checked variable
# 20. Display examples of records containing invalid values
# 21. Replace confirmed invalid values with NA
# 22. Record how many values were changed to NA
# 23. Investigate statistical outliers separately
# 24. Avoid automatically removing valid outliers
# 25. Create and export the validation tables
# 26. Document every validation rule and result
# 27. Pass spotify_clean to the missing-value stage


# Variables requiring validation:
#
# Target and general variables:
# popularity
# duration_ms
#
# Continuous audio variables:
# danceability
# energy
# loudness
# speechiness
# acousticness
# instrumentalness
# liveness
# valence
# tempo
#
# Musical category variables:
# key
# mode
# time_signature


# Required validation-table fields:
# variable
# approved_valid_rule
# justification_or_source
# invalid_count
# action_taken


# Examples of invalid values:
# Negative duration
# Unsupported mode category
# Unsupported key category
# Impossible popularity score
# Negative tempo
# Audio-confidence values outside their approved range


# Examples of values that require investigation but may be valid:
# Very long track duration
# Very high tempo
# Very low loudness
# Extremely high instrumentalness
#
# These should not automatically be removed.


# Planned outputs:
# outputs/tables/value_validation_rules.csv
# outputs/tables/invalid_values_summary.csv
# outputs/tables/invalid_record_examples.csv
# outputs/tables/outlier_investigation_summary.csv


# Documentation file:
# report/cleaning_documentation/
# member3_value_validation_and_outliers.md


# Documentation must explain:
# 1. The validation rule for every variable
# 2. The source or justification supporting the rule
# 3. The number of invalid values found
# 4. Examples of invalid records
# 5. Why each value was considered invalid
# 6. Which invalid values were changed to NA
# 7. The difference between invalid values and outliers
# 8. Which unusual values were retained and why
# 9. The effect of validation on missing-value counts
# 10. Any limitations in the selected validation rules


# Important:
# Do not remove complete records only because one value is invalid.
# Replace confirmed invalid values with NA first.
# Do not handle the resulting missing values in this script.
# Do not automatically remove statistical outliers.
# Do not export the final cleaned dataset in this script.
# Do not perform final cleaned-data validation here.