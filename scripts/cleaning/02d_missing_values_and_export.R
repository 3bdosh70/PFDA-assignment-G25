# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02d_missing_values_and_export.R
# Responsible student:
# Name:
# TP Number:

# Purpose:
# To handle the remaining missing values after data validation
# and export the final cleaned dataset for later analysis.
# ============================================================

library(dplyr)

#-----------------------------------------------------
#Check the dataset before handling missing values

nrow(clean_data)
ncol(clean_data)

colSums(is.na(clean_data))
sum(is.na(clean_data))

#There are 113579 rows, 20 columns and 5220 missing values
#before handling the remaining missing values

#-----------------------------------------------------
#Remove rows with missing popularity

sum(is.na(clean_data$popularity))
#There are 268 missing popularity values

rows_before_popularity = nrow(clean_data)

clean_data =
  clean_data[!is.na(clean_data$popularity), ]

rows_after_popularity = nrow(clean_data)

popularity_rows_removed =
  rows_before_popularity - rows_after_popularity

popularity_rows_removed
#268 rows with missing popularity are removed

nrow(clean_data)
#113311 rows remain

#Popularity is the main target variable for the assignment
#Missing popularity values are not imputed because this would
#create artificial target values for the later analysis and prediction

#-----------------------------------------------------
#Check the remaining missing values

colSums(is.na(clean_data))
sum(is.na(clean_data))

#After removing rows with missing popularity, 4936 missing values remain
#in the predictor and descriptive variables

#-----------------------------------------------------
#Handle missing descriptive text values

clean_data$artists[
  is.na(clean_data$artists)
] = "Unknown"

clean_data$album_name[
  is.na(clean_data$album_name)
] = "Unknown"

clean_data$track_name[
  is.na(clean_data$track_name)
] = "Unknown"

sum(is.na(clean_data$artists))
sum(is.na(clean_data$album_name))
sum(is.na(clean_data$track_name))

#Missing artist, album and track names are replaced with Unknown
#because replacing them with another existing name would create
#incorrect descriptive information

#-----------------------------------------------------
#Handle missing duration values using median

duration_median =
  as.integer(median(clean_data$duration_ms, na.rm = TRUE))

duration_median

clean_data$duration_ms[
  is.na(clean_data$duration_ms)
] = duration_median

sum(is.na(clean_data$duration_ms))

#Median is used because duration is numerical and the median
#is less affected by extreme values

#-----------------------------------------------------
#Handle missing continuous audio values using median

danceability_median =
  median(clean_data$danceability, na.rm = TRUE)

clean_data$danceability[
  is.na(clean_data$danceability)
] = danceability_median


energy_median =
  median(clean_data$energy, na.rm = TRUE)

clean_data$energy[
  is.na(clean_data$energy)
] = energy_median


loudness_median =
  median(clean_data$loudness, na.rm = TRUE)

clean_data$loudness[
  is.na(clean_data$loudness)
] = loudness_median


speechiness_median =
  median(clean_data$speechiness, na.rm = TRUE)

clean_data$speechiness[
  is.na(clean_data$speechiness)
] = speechiness_median


acousticness_median =
  median(clean_data$acousticness, na.rm = TRUE)

clean_data$acousticness[
  is.na(clean_data$acousticness)
] = acousticness_median


instrumentalness_median =
  median(clean_data$instrumentalness, na.rm = TRUE)

clean_data$instrumentalness[
  is.na(clean_data$instrumentalness)
] = instrumentalness_median


liveness_median =
  median(clean_data$liveness, na.rm = TRUE)

clean_data$liveness[
  is.na(clean_data$liveness)
] = liveness_median


valence_median =
  median(clean_data$valence, na.rm = TRUE)

clean_data$valence[
  is.na(clean_data$valence)
] = valence_median


tempo_median =
  median(clean_data$tempo, na.rm = TRUE)

clean_data$tempo[
  is.na(clean_data$tempo)
] = tempo_median

#Median is used for the continuous numerical audio variables
#because it is less affected by extreme values than the mean

#-----------------------------------------------------
#Check the median values used

duration_median
danceability_median
energy_median
loudness_median
speechiness_median
acousticness_median
instrumentalness_median
liveness_median
valence_median
tempo_median

#Expected median values are approximately:
#duration_ms = 213000
#danceability = 0.580
#energy = 0.685
#loudness = -6.999
#speechiness = 0.0489
#acousticness = 0.168
#instrumentalness = 0.0000413
#liveness = 0.132
#valence = 0.464
#tempo = 122.020

#-----------------------------------------------------
#Handle missing key values using mode

key_mode =
  as.integer(names(which.max(table(clean_data$key))))

key_mode

clean_data$key[
  is.na(clean_data$key)
] = key_mode

sum(is.na(clean_data$key))

#Key is a discrete variable so the most frequently occurring
#valid key value is used for missing records

#-----------------------------------------------------
#Handle missing mode values using mode

mode_mode =
  as.integer(names(which.max(table(clean_data$mode))))

mode_mode

clean_data$mode[
  is.na(clean_data$mode)
] = mode_mode

sum(is.na(clean_data$mode))

#Mode is a categorical discrete variable containing 0 and 1
#so its most frequent value is used for missing records

#-----------------------------------------------------
#Handle missing time_signature values using mode

time_signature_mode =
  as.integer(
    names(which.max(table(clean_data$time_signature)))
  )

time_signature_mode

clean_data$time_signature[
  is.na(clean_data$time_signature)
] = time_signature_mode

sum(is.na(clean_data$time_signature))

#Time signature is a discrete variable so its most frequently
#occurring valid value is used for missing records

#-----------------------------------------------------
#Handle missing explicit values using mode

explicit_mode =
  names(which.max(table(clean_data$explicit)))

explicit_mode

clean_data$explicit[
  is.na(clean_data$explicit)
] = as.logical(explicit_mode)

sum(is.na(clean_data$explicit))

#Explicit is a boolean variable so the most frequently occurring
#TRUE or FALSE value is used for the missing records

#-----------------------------------------------------
#Check the mode values used

key_mode
mode_mode
time_signature_mode
explicit_mode

#Expected mode values:
#key = 7
#mode = 1
#time_signature = 4
#explicit = FALSE

#-----------------------------------------------------
#Check missing values after treatment

colSums(is.na(clean_data))

sum(is.na(clean_data))

sum(rowSums(is.na(clean_data)) > 0)

#The result should be 0, confirming that no missing values remain

#-----------------------------------------------------
#Check duplicates after missing value treatment

sum(duplicated(clean_data))

#Missing value replacement may cause previously different records
#to become identical, so exact duplicates are checked again

rows_before_final_duplicates = nrow(clean_data)

clean_data = clean_data %>% distinct()

rows_after_final_duplicates = nrow(clean_data)

final_duplicates_removed =
  rows_before_final_duplicates -
  rows_after_final_duplicates

final_duplicates_removed

#6 exact duplicate records are expected to be created after
#missing value treatment and are removed before export

sum(duplicated(clean_data))
#The result should be 0

#-----------------------------------------------------
#Final dataset check

nrow(clean_data)
ncol(clean_data)

names(clean_data)

str(clean_data)

colSums(is.na(clean_data))

sum(is.na(clean_data))

sum(duplicated(clean_data))

#The final dataset should contain 113305 rows and 20 columns
#with no missing values and no exact duplicate records

#-----------------------------------------------------
#Export the cleaned dataset

write.csv(
  clean_data,
  "data/processed/spotify_tracks_clean.csv",
  row.names = FALSE
)

#The final cleaned dataset is exported without an additional
#row index and will be used for exploration and objective analysis

#-----------------------------------------------------
#Stage 02d Summary

cat("\n--- MISSING VALUE AND EXPORT SUMMARY ---\n")

cat(
  "Popularity rows removed:",
  popularity_rows_removed,
  "\n"
)

cat(
  "Duplicates removed after missing value treatment:",
  final_duplicates_removed,
  "\n"
)

cat(
  "Remaining missing values:",
  sum(is.na(clean_data)),
  "\n"
)

cat(
  "Final duplicate records:",
  sum(duplicated(clean_data)),
  "\n"
)

cat(
  "Final rows:",
  nrow(clean_data),
  "\n"
)

cat(
  "Final columns:",
  ncol(clean_data),
  "\n"
)

cat(
  "\nStage 02d completed: missing values handled and cleaned dataset exported.\n"
)