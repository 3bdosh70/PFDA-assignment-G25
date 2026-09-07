# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 01_import_and_audit.R
# Responsible student:
# Name:
# TP Number:

# Purpose:
# To import and inspect the original Spotify dataset without
# changing or cleaning the original records.
# ============================================================

#import and test the dataset
#compare the names and data types of the columns
getwd()
raw_data = read.csv(
  "data/raw/spotify_tracks_data.csv",
  na.strings = c("", "NA", "N/A", "NULL")
)
#we sort all missing values as null for the program so it's all identifed under the same concept
head(raw_data)
#dataset was imported successfully and tested to be matching the actual csv file comparing with the first 6 rows

names(raw_data)
#current columns for the raw data
#"X"                "track_id"         "artists"          "album_name"      
#"track_name"       "popularity"       "duration_ms"      "explicit"        
#"danceability"     "energy"           "key"              "loudness"        
#"mode"             "speechiness"      "acousticness"     "instrumentalness"
#"liveness"         "valence"          "tempo"            "time_signature"  
#"track_genre"
ncol(raw_data)
nrow(raw_data)
# The dataset contains 114,000 rows and 21 columns.
# Twenty columns correspond to the supplied dataset description.
# X is an additional index-like column and will be investigated
# before any cleaning decision is made.

str(raw_data)
#we have compared the columns data types with the file "dataset_description.txt"
#the decimal audio features are represented as numeric in R, which is appropriate for the documented float variables
#any required structural conversions will be handled in Stage 02a

#-----------------------------------------------------

#Finding the missing values

colSums(is.na(raw_data))
sort(colSums(is.na(raw_data)), decreasing = TRUE)
sum(is.na(raw_data))
View(colSums(is.na(raw_data)))

(colSums(is.na(raw_data)) / nrow(raw_data)) * 100
# Missing percentage for every column

sum(rowSums(is.na(raw_data)) > 0)
# Number of rows containing at least one missing value
# The dataset contains 3,553 missing values across 18 columns.
# A total of 3,485 rows contain at least one missing value.
# artists - 187
# album_name - 220
# track_name - 186
# popularity - 193
# duration_ms - 191
# explicit - 202
# danceability - 190
# energy - 192
# key - 203
# loudness - 214
# mode - 189
# speechiness - 187
# acousticness - 193
# instrumentalness - 216
# liveness - 177
# valence - 202
# tempo - 220
# time_signature - 191
# No missing values are changed during the audit stage.

#-----------------------------------------------------

#Finding the duplicated rows

sum(duplicated(raw_data))
# The result is 0 because the X column contains a unique index for every row.
# Therefore, no two complete rows are identical when X is included.

duplicate_test = raw_data
duplicate_test$X = NULL
# A temporary copy is created so the X index can be removed
# without changing the raw data, as X may hide exact duplicates.q

sum(duplicated(duplicate_test))
#total of duplicates excluding "X" column
#There are 396 redundant duplicate occurrences.

duplicate_test[duplicated(duplicate_test), ]
View(duplicate_test[duplicated(duplicate_test), ])

####################
duplicate_rows = duplicate_test[duplicated(duplicate_test), ]
View(head(duplicate_rows))
View(duplicate_rows)

###################
all_duplicate_rows = duplicate_test[
  duplicated(duplicate_test) |
    duplicated(duplicate_test, fromLast = TRUE), ]

View(head((all_duplicate_rows)))
#-----------------------------------------------------
#Finding repeated track IDs

length(unique(raw_data$track_id))
#count unique track IDs

track_id_number = table(raw_data$track_id)
repeated_track_ids = track_id_number[track_id_number > 1]
repeated_track_ids
View(repeated_track_ids)
length(repeated_track_ids)
#Identify repeated IDs

repeated_track_rows =
  raw_data[raw_data$track_id %in% names(repeated_track_ids), ]

nrow(repeated_track_rows)
#Count all rows belonging to repeated IDs

View(repeated_track_ids)
#Display the record count for each repeated ID

id_genre = unique(
  repeated_track_rows[c("track_id", "track_genre")]
)
#-----------------------------------------------------
#This check determines whether repeated track IDs are redundant records
#or the same track intentionally classified under different genres.
#This prevents meaningful genre records from being treated as exact duplicates.

genre_counts = table(id_genre$track_id)
View(genre_counts)

multiple_genre_ids = genre_counts[genre_counts > 1]

multiple_genre_ids
View(multiple_genre_ids)

#-----------------------------------------------------
#Repeated track IDs are checked to determine whether the same track 
#has multiple popularity values
id_popularity = unique(
  repeated_track_rows[c("track_id", "popularity")]
)

popularity_counts = table(id_popularity$track_id)

different_popularity_ids =
  popularity_counts[popularity_counts > 1]

different_popularity_ids
View(different_popularity_ids)

#-----------------------------------------------------
# Repeated track IDs are checked to determine whether the same track has
# multiple combinations of audio attributes, without assuming the cause.
audio_columns = c(
  "track_id", "duration_ms", "danceability", "energy",
  "key", "loudness", "mode", "speechiness", "acousticness",
  "instrumentalness", "liveness", "valence", "tempo",
  "time_signature"
)

id_audio = unique(repeated_track_rows[audio_columns])

audio_counts = table(id_audio$track_id)

different_audio_ids = audio_counts[audio_counts > 1]

different_audio_ids
View(different_audio_ids)
#-----------------------------------------------------

exact_duplicate_flag =
  duplicated(duplicate_test) |
  duplicated(duplicate_test, fromLast = TRUE)
# Exact duplicates have identical values across all compared columns.


repeated_id_flag =
  duplicate_test$track_id %in% names(repeated_track_ids)
# Identify all rows with repeated track IDs


exact_duplicate_records =
  duplicate_test[exact_duplicate_flag, ]
# Exact duplicated records


repeated_nonduplicate_records =
  duplicate_test[repeated_id_flag & !exact_duplicate_flag, ]

# Repeated-ID records that are not exact duplicates

nrow(exact_duplicate_records)
#786 rows belong to exact duplicate groups.
#These groups contain 396 redundant duplicate occurrences.
nrow(repeated_nonduplicate_records)
#40114 rows contain repeated track IDs but are not exact duplicates.

View(exact_duplicate_records)
View(repeated_nonduplicate_records)
#-----------------------------------------------------
#Categorical Values

unique(raw_data$explicit)
#Whether the track has explicit lyrics

unique(raw_data$key)
#Estimated overall musical key, using standard pitch class notation (0=C, 1=C#/Db, etc.)
#Key contains −5, 12, 15, 20, and 30, which are potentially unsupported because they fall outside the documented range of 0 to 11.
#unique(raw_data$mode)


unique(raw_data$mode)
#Modality of the track (1=major, 0=minor)
#Mode contains −1, 2, and 5, which are potentially unsupported because only 0 (minor) and 1 (major) are documented.


unique(raw_data$time_signature)
#Estimated time signature (number of beats per bar)
#Time signature contains unusual values including −1, 0, 1, 9, 12, and 15.
#Their validity will be checked against an authoritative reference during Stage 02c before any values are changed.


sort(unique(raw_data$track_genre))
#Genre category assigned to the track
#Track genre contains several potential spelling and formatting inconsistencies, mainly caused by missing hyphens or spaces.
#These variations may incorrectly separate the same intended genre into different categories
#-----------------------------------------------------
#frequency counts
#Frequency counts show how often each categorical value occurs.

table(raw_data$explicit)
#Explicit is strongly concentrated in “False” with 104,069 records,
#compared with 9,729 “True” records. The frequency table excludes 202 missing values.

table(raw_data$key)
##Key contains 38 potentially unsupported values outside the accepted range of 0–11: −5 appears 11 times,
#12 appears 8 times, 15 appears 4 times, 20 appears 10 times, and 30 appears 5 times. The table excludes 203 missing values.


table(raw_data$mode)
##Mode is mainly represented by 1, with 72,532 records, followed by 0, with 41,248 records.
#It also contains 31 unsupported values: −1 appears 9 times, 2 appears 14 times, and 5 appears 8 times.
#The table excludes 189 missing values.


table(raw_data$time_signature)
##Time signature is strongly concentrated at 4, with 101,638 records.
#It also contains 1,173 values among the unusual values identified for later verification,
#including −1, 0, 1, 9, 12, and 15. The table excludes 191 missing values.

sort(table(raw_data$track_genre), decreasing = TRUE)
##Genre frequencies are divided by potential spelling and punctuation variants.
#For example, alt-rock has 750 records while altrock has 250.
#These variants are recorded for later review and are not changed during Stage 1.
#-----------------

#Numerical Values-----------------

#summary statistics
summary(raw_data$popularity)
summary(raw_data$duration_ms)

continuous_audio = raw_data[c(
  "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness",
  "valence", "tempo"
)]

summary(continuous_audio)
#----------------------------------------------------
#minimum and maximum values
sapply(continuous_audio, min, na.rm = TRUE)
sapply(continuous_audio, max, na.rm = TRUE)

#------------------------------------------------
#Popularity outside 0–100
sum(
  !is.na(raw_data$popularity) &
    (raw_data$popularity < 0 | raw_data$popularity > 100)
)

#-----------------------------------------------

sum(!is.na(raw_data$popularity) & raw_data$popularity < 0)
sum(!is.na(raw_data$popularity) & raw_data$popularity > 100)

# A total of 75 popularity values fall outside the currently checked range of 0–100:
# 24 are below 0 and 51 are above 100.
# Their validity will be verified using an authoritative reference during Stage 02c.


# -------------------------------------------------------------
# 6. Negative or zero duration values
# -------------------------------------------------------------

sum(
  !is.na(raw_data$duration_ms) &
    raw_data$duration_ms <= 0
)

sum(!is.na(raw_data$duration_ms) & raw_data$duration_ms < 0)
sum(!is.na(raw_data$duration_ms) & raw_data$duration_ms == 0)

# There are 51 non-positive duration values: 38 are negative and
# 13 are zero. They are potentially invalid because a track must
# have a positive duration.


# -------------------------------------------------------------
# 7. Audio-score values outside the expected range of 0–1
# -------------------------------------------------------------

audio_scores = raw_data[c(
  "danceability", "energy", "speechiness", "acousticness",
  "instrumentalness", "liveness", "valence"
)]

sapply(
  audio_scores,
  function(x) sum(!is.na(x) & (x < 0 | x > 1))
)

# Danceability, energy, speechiness, acousticness, instrumentalness,
# liveness and valence each contain 31 values outside the currently checked range of 0–1.
# These potential range violations will be verified before treatment
# during Stage 02c.


# -------------------------------------------------------------
# 8. Non-positive tempo values
# -------------------------------------------------------------

sum(
  !is.na(raw_data$tempo) &
    raw_data$tempo <= 0
)

sum(!is.na(raw_data$tempo) & raw_data$tempo < 0)
sum(!is.na(raw_data$tempo) & raw_data$tempo == 0)

# Tempo contains 194 non-positive values: 37 are negative and
# 157 are zero. These values are recorded for further validation
# during Stage 02c before any treatment.


# -------------------------------------------------------------
# 9. Potentially suspicious loudness values
# -------------------------------------------------------------

summary(raw_data$loudness)

sum(
  !is.na(raw_data$loudness) &
    (raw_data$loudness < -60 | raw_data$loudness > 0)
)

sum(!is.na(raw_data$loudness) & raw_data$loudness < -60)
sum(!is.na(raw_data$loudness) & raw_data$loudness > 0)

# Using the current reference interval of -60 to 0 dB, 140 loudness
# values are flagged for further investigation: 24 are below -60 and
# 116 are above 0. These values are not treated as invalid during Stage 01
# and will be verified during Stage 02c.

#-----------------------------------------------------
#Stage 01 Audit Summary

cat("\n--- DATASET AUDIT SUMMARY ---\n")
cat("Rows:", nrow(raw_data), "\n")
cat("Columns:", ncol(raw_data), "\n")
cat("Total missing cells:", sum(is.na(raw_data)), "\n")
cat(
  "Rows with missing values:",
  sum(rowSums(is.na(raw_data)) > 0),
  "\n"
)
cat(
  "Duplicate occurrences excluding X:",
  sum(duplicated(duplicate_test)),
  "\n"
)
cat(
  "Rows belonging to duplicate groups:",
  sum(exact_duplicate_flag),
  "\n"
)
cat(
  "Repeated track IDs:",
  length(repeated_track_ids),
  "\n"
)

cat("\nStage 01 completed: no data were cleaned or removed.\n")

