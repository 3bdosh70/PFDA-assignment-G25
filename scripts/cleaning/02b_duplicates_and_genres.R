# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02b_duplicates_and_genres.R
# Responsible student:
# Name:
# TP Number:

# Main responsibility:
# Exact duplicate investigation, repeated track-ID analysis
# and genre-label standardisation.

# Purpose:
# To identify and handle confirmed exact duplicate records,
# investigate why some track IDs appear more than once, and
# standardise inconsistent genre labels.
#
# Exact duplicates and repeated track IDs must be treated as
# different issues. A repeated track ID must not automatically
# be deleted because the same track may be connected to more
# than one genre.
#
# This script runs after:
# scripts/cleaning/02a_structure_and_types.R
#
# It must use the spotify_clean dataset created during the
# structure-and-types stage.
# ============================================================


# Tasks for this script:
# 1. Confirm that spotify_clean exists
# 2. Record the number of rows before duplicate treatment
# 3. Count exact duplicated records
# 4. Display examples of confirmed exact duplicates
# 5. Remove only confirmed exact duplicate records
# 6. Record the number of exact duplicates removed
# 7. Count the number of unique track IDs
# 8. Identify track IDs appearing more than once
# 9. Calculate how many records exist for each repeated track
# 10. Check whether repeated tracks have different genres
# 11. Check whether repeated tracks contain inconsistent values
# 12. Avoid automatically deleting repeated track IDs
# 13. Record a recommendation for handling repeated tracks
# 14. Inspect the original genre labels
# 15. Remove unnecessary spaces from genre labels
# 16. Standardise genre capitalisation
# 17. Identify punctuation and spelling variants
# 18. Create an approved genre-mapping table
# 19. Apply only the approved genre corrections
# 20. Compare genre counts before and after standardisation
# 21. Create and export all required audit tables
# 22. Document every decision, result and limitation
# 23. Pass spotify_clean to the next cleaning stage


# Important distinction:
#
# Exact duplicates:
# Records that contain the same values across all meaningful
# variables after the unnecessary index column has been removed.
#
# Repeated track IDs:
# Records that share the same track_id but may have different
# genres or other values. These require investigation and must
# not automatically be removed.


# Examples of genre variants that should be investigated:
# hiphop and hip-hop
# kpop and k-pop
# jpop and j-pop
# deephouse and deep-house
# hardrock and hard-rock
#
# The final mapping must be reviewed and approved by the group
# before it is applied.


# Planned outputs:
# outputs/tables/exact_duplicate_summary.csv
# outputs/tables/repeated_track_id_summary.csv
# outputs/tables/repeated_track_investigation.csv
# outputs/tables/genre_counts_before_standardisation.csv
# outputs/tables/genre_mapping_table.csv
# outputs/tables/genre_counts_after_standardisation.csv


# Documentation file:
# report/cleaning_documentation/
# member2_duplicates_tracks_and_genres.md


# Documentation must explain:
# 1. How exact duplicates were defined
# 2. How many exact duplicates were found
# 3. How many exact duplicates were removed
# 4. How repeated track IDs were investigated
# 5. Why repeated track IDs were not automatically deleted
# 6. Which genre inconsistencies were found
# 7. Which genre mappings were applied
# 8. Genre counts before and after standardisation
# 9. The effect of these changes on the dataset
# 10. Any limitations or uncertain cases


# Important:
# Do not handle missing values in this script.
# Do not validate numerical ranges in this script.
# Do not replace invalid numerical values in this script.
# Do not export the final cleaned dataset in this script.
# Do not perform the final cleaned-data validation here.