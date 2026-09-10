# ============================================================
# GROUP 25 - MAIN WORKFLOW: STAGES 1 TO 3
# ============================================================
# Consolidated from 01_import_and_audit.R, 02_data_preparation.R,
# cleaning/02c_value_validation.R, cleaning/02d_missing_values_and_export.R
# and 03_general_exploration.R. The completed 02c/02d rules supersede
# stale validation and pending-treatment sections in 02_data_preparation.R.
# Run from the project root: source("scripts/Main_stages_1_3.R")
# Or: Rscript --vanilla scripts/Main_stages_1_3.R
# One audit, one preparation stage and one general exploration stage.
# Generated data, tables and plots go to outputs/main_stages_1_3/.
#
# Cleaning follows the existing project rules. Median/mode imputation
# here is for descriptive exploration. Prediction must split the data
# before learning imputation values, using the pre-imputation copy.
# The completed 02c policy treats tempo <= 0 and loudness outside
# [-60, 0] as invalid, then applies the existing 02d missing-value treatment.
# Repeated track IDs across genres remain; rows are not independent songs.

library(dplyr)
library(ggplot2)

# Locate the project when run from its root, scripts folder, or by file path.
script_args = grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path = if (length(script_args)) sub("^--file=", "", script_args[1]) else ""
for (source_frame in sys.frames()) {
  if (!is.null(source_frame$ofile)) script_path = source_frame$ofile
}
project_candidates = c(getwd(), dirname(getwd()))
if (nzchar(script_path)) {
  project_candidates = c(project_candidates,
    dirname(dirname(normalizePath(script_path, mustWork = TRUE))))
}
project_candidates = project_candidates[
  file.exists(file.path(project_candidates, "data", "raw", "spotify_tracks_data.csv"))
]
if (!length(project_candidates)) {
  stop("Cannot find data/raw/spotify_tracks_data.csv. Run from the project root.")
}
project_dir = normalizePath(project_candidates[1], mustWork = TRUE)
output_dir = file.path(project_dir, "outputs", "main_stages_1_3")
dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
if (!all(dir.exists(file.path(output_dir, c("tables", "plots"))))) {
  stop("Cannot create output folders. Check write permission for outputs/main_stages_1_3.")
}

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
  file.path(project_dir, "data", "raw", "spotify_tracks_data.csv"),
  na.strings = c("", "NA", "N/A", "NULL")
)

# Fail before cleaning if the input schema or types are incompatible.
required_columns = c("track_id", "artists", "album_name", "track_name",
  "popularity", "duration_ms", "explicit", "danceability", "energy", "key",
  "loudness", "mode", "speechiness", "acousticness", "instrumentalness",
  "liveness", "valence", "tempo", "time_signature", "track_genre")
if (!identical(names(raw_data), c("X", required_columns)) || nrow(raw_data) == 0) {
  stop("Raw dataset must have the expected 21 columns in order and at least one row.")
}
numeric_columns = setdiff(required_columns,
  c("track_id", "artists", "album_name", "track_name", "explicit", "track_genre"))
for (column in numeric_columns) {
  if (!is.numeric(raw_data[[column]]) ||
      any(!is.finite(raw_data[[column]]) & !is.na(raw_data[[column]]))) {
    stop(paste("Non-numeric or infinite input in", column))
  }
}
integer_columns = c("popularity", "duration_ms", "key", "mode", "time_signature")
for (column in integer_columns) {
  values = raw_data[[column]]
  if (any(!is.na(values) & (values %% 1 != 0 | abs(values) > .Machine$integer.max))) {
    stop(paste("Unsafe integer conversion in", column))
  }
}
explicit_values = tolower(trimws(as.character(raw_data$explicit)))
if (any(!is.na(explicit_values) & !explicit_values %in% c("true", "false"))) {
  stop("Unexpected explicit label: expected True/False or TRUE/FALSE.")
}
if (any(is.na(raw_data$track_id) | trimws(raw_data$track_id) == "") ||
    any(is.na(raw_data$track_genre) | trimws(raw_data$track_genre) == "")) {
  stop("Track IDs and genres must not be missing.")
}

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
if (interactive()) View(colSums(is.na(raw_data)))

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
#View(duplicate_test[duplicated(duplicate_test), ])

####################
duplicate_rows = duplicate_test[duplicated(duplicate_test), ]
#View(head(duplicate_rows))
#View(duplicate_rows)

###################
all_duplicate_rows = duplicate_test[
  duplicated(duplicate_test) |
    duplicated(duplicate_test, fromLast = TRUE), ]

#View(head((all_duplicate_rows)))
#-----------------------------------------------------
#Finding repeated track IDs

length(unique(raw_data$track_id))
#count unique track IDs

track_id_number = table(raw_data$track_id)
repeated_track_ids = track_id_number[track_id_number > 1]
repeated_track_ids
#View(repeated_track_ids)
length(repeated_track_ids)
#Identify repeated IDs

repeated_track_rows =
  raw_data[raw_data$track_id %in% names(repeated_track_ids), ]

nrow(repeated_track_rows)
#Count all rows belonging to repeated IDs

#View(repeated_track_ids)
#Display the record count for each repeated ID

id_genre = unique(
  repeated_track_rows[c("track_id", "track_genre")]
)
#-----------------------------------------------------
#This check determines whether repeated track IDs are redundant records
#or the same track intentionally classified under different genres.
#This prevents meaningful genre records from being treated as exact duplicates.

genre_counts = table(id_genre$track_id)
if (interactive()) View(genre_counts)

multiple_genre_ids = genre_counts[genre_counts > 1]

multiple_genre_ids
#View(multiple_genre_ids)

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
#View(different_popularity_ids)

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
#View(different_audio_ids)
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

#View(exact_duplicate_records)
#View(repeated_nonduplicate_records)
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
# values are flagged for further investigation: 24 are below -60   and
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


# STAGE 2: DATA PREPARATION
original_rows = nrow(raw_data)
original_columns = ncol(raw_data)
original_missing = sum(is.na(raw_data))

#Create a working copy of the raw dataset

clean_data = raw_data


#-----------------------------------------------------
#Check X before removing it

head(clean_data$X)
tail(clean_data$X)
length(unique(clean_data$X))

#X contains a unique sequential value for every row and is not
#included in the supplied dataset description


#-----------------------------------------------------
#Remove the additional X index column

clean_data$X = NULL

#X is removed because it is an additional index column and does not
#represent a Spotify track characteristic used in the analysis


#-----------------------------------------------------
#Check the remaining columns against the dataset description

expected_columns = c(
  "track_id",
  "artists",
  "album_name",
  "track_name",
  "popularity",
  "duration_ms",
  "explicit",
  "danceability",
  "energy",
  "key",
  "loudness",
  "mode",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo",
  "time_signature",
  "track_genre"
)

all(
  names(clean_data) ==
    expected_columns
)

#TRUE confirms that the 20 remaining column names match
#the columns in dataset_description.txt



# ============================================================
# 2.4 DATA TRANSFORMATION
# ============================================================

#The transformation steps are performed here before later cleaning
#and validation because later operations use the corrected data types


#-----------------------------------------------------
#Check that documented integer variables contain whole numbers

sum(
  !is.na(clean_data$popularity) &
    clean_data$popularity %% 1 != 0
)

sum(
  !is.na(clean_data$duration_ms) &
    clean_data$duration_ms %% 1 != 0
)

sum(
  !is.na(clean_data$key) &
    clean_data$key %% 1 != 0
)

sum(
  !is.na(clean_data$mode) &
    clean_data$mode %% 1 != 0
)

sum(
  !is.na(clean_data$time_signature) &
    clean_data$time_signature %% 1 != 0
)

#All five variables contain no decimal values among
#their non-missing records


#-----------------------------------------------------
#Record missing values before type conversion

missing_before_types =
  sum(is.na(clean_data))


#-----------------------------------------------------
#Convert documented integer variables

clean_data$popularity =
  as.integer(clean_data$popularity)

clean_data$duration_ms =
  as.integer(clean_data$duration_ms)

clean_data$key =
  as.integer(clean_data$key)

clean_data$mode =
  as.integer(clean_data$mode)

clean_data$time_signature =
  as.integer(clean_data$time_signature)

#The variables documented as integers are converted from
#numeric to integer after confirming that they contain no decimals


#-----------------------------------------------------
#Convert explicit from character to logical

unique(clean_data$explicit)

clean_data$explicit =
  tolower(trimws(as.character(clean_data$explicit))) == "true"

unique(clean_data$explicit)

#explicit is now represented as FALSE, TRUE and NA


#-----------------------------------------------------
#Check whether type conversions created new missing values

missing_after_types =
  sum(is.na(clean_data))

missing_after_types -
  missing_before_types

#The difference should be 0, confirming that the type
#conversions did not create additional missing values



# ============================================================
# 2.2 DATA CLEANING AND PREPROCESSING
# Exact duplicate treatment
# ============================================================

#-----------------------------------------------------
#Remove exact duplicate records

duplicates_before =
  sum(duplicated(clean_data))

duplicates_before

#There are 396 redundant exact duplicate records


rows_before_duplicates =
  nrow(clean_data)

clean_data =
  clean_data %>%
  distinct()

rows_after_duplicates =
  nrow(clean_data)

rows_removed_duplicates =
  rows_before_duplicates -
  rows_after_duplicates

rows_removed_duplicates

#396 exact duplicate records are removed
#Repeated track IDs are not automatically removed



# ============================================================
# 2.4 DATA TRANSFORMATION
# Genre standardisation
# ============================================================

#-----------------------------------------------------
#Check genres before standardisation

genres_before =
  length(
    unique(clean_data$track_genre)
  )

genres_before

sort(
  unique(clean_data$track_genre)
)

#There are 141 genre categories before standardisation


#-----------------------------------------------------
#Standardise general genre formatting

clean_data$track_genre =
  gsub(
    "\\s+",
    " ",
    tolower(
      trimws(clean_data$track_genre)
    )
  )


#-----------------------------------------------------
#Standardise identified genre variants

clean_data$track_genre[
  clean_data$track_genre == "altrock"
] = "alt-rock"

clean_data$track_genre[
  clean_data$track_genre == "chicagohouse"
] = "chicago-house"

clean_data$track_genre[
  clean_data$track_genre == "deephouse"
] = "deep-house"

clean_data$track_genre[
  clean_data$track_genre == "detroittechno"
] = "detroit-techno"

clean_data$track_genre[
  clean_data$track_genre == "drum and bass"
] = "drum-and-bass"

clean_data$track_genre[
  clean_data$track_genre == "hardrock"
] = "hard-rock"

clean_data$track_genre[
  clean_data$track_genre == "hiphop"
] = "hip-hop"

clean_data$track_genre[
  clean_data$track_genre == "honkytonk"
] = "honky-tonk"

clean_data$track_genre[
  clean_data$track_genre == "indiepop"
] = "indie-pop"

clean_data$track_genre[
  clean_data$track_genre == "jdance"
] = "j-dance"

clean_data$track_genre[
  clean_data$track_genre == "jidol"
] = "j-idol"

clean_data$track_genre[
  clean_data$track_genre == "jpop"
] = "j-pop"

clean_data$track_genre[
  clean_data$track_genre == "jrock"
] = "j-rock"

clean_data$track_genre[
  clean_data$track_genre == "kpop"
] = "k-pop"

clean_data$track_genre[
  clean_data$track_genre == "minimaltechno"
] = "minimal-techno"

clean_data$track_genre[
  clean_data$track_genre == "newage"
] = "new-age"

clean_data$track_genre[
  clean_data$track_genre == "popfilm"
] = "pop-film"

clean_data$track_genre[
  clean_data$track_genre == "powerpop"
] = "power-pop"

clean_data$track_genre[
  clean_data$track_genre == "progressivehouse"
] = "progressive-house"

clean_data$track_genre[
  clean_data$track_genre == "punkrock"
] = "punk-rock"

clean_data$track_genre[
  clean_data$track_genre == "rnb"
] = "r-n-b"

clean_data$track_genre[
  clean_data$track_genre == "rocknroll"
] = "rock-n-roll"

clean_data$track_genre[
  clean_data$track_genre == "showtunes"
] = "show-tunes"

clean_data$track_genre[
  clean_data$track_genre == "singersongwriter"
] = "singer-songwriter"

clean_data$track_genre[
  clean_data$track_genre == "synthpop"
] = "synth-pop"

clean_data$track_genre[
  clean_data$track_genre == "triphop"
] = "trip-hop"

clean_data$track_genre[
  clean_data$track_genre == "worldmusic"
] = "world-music"


genres_after =
  length(
    unique(clean_data$track_genre)
  )

genres_after

#The number of genre categories decreases from 141 to 114



# ============================================================
# 2.2 DATA CLEANING AND PREPROCESSING
# Duplicate check after genre transformation
# ============================================================

#Genre standardisation may make previously different records identical

genre_duplicates =
  sum(duplicated(clean_data))

genre_duplicates

#25 duplicate records are created after genre standardisation


rows_before_genre_duplicates =
  nrow(clean_data)

clean_data =
  clean_data %>%
  distinct()

rows_after_genre_duplicates =
  nrow(clean_data)

genre_duplicates_removed =
  rows_before_genre_duplicates -
  rows_after_genre_duplicates

genre_duplicates_removed


#-----------------------------------------------------
#Confirm meaningful repeated track IDs remain

track_id_number =
  table(clean_data$track_id)

repeated_track_ids =
  track_id_number[
    track_id_number > 1
  ]

length(repeated_track_ids)


repeated_track_rows =
  clean_data[
    clean_data$track_id %in%
      names(repeated_track_ids),
  ]

nrow(repeated_track_rows)

#Repeated track IDs remain because repeated IDs are not
#automatically duplicates and may contain meaningful differences



# ============================================================
# 2.3 DATA VALIDATION
# ============================================================

missing_before_validation =
  sum(is.na(clean_data))


#-----------------------------------------------------
#Validate popularity

invalid_popularity =
  !is.na(clean_data$popularity) &
  (
    clean_data$popularity < 0 |
      clean_data$popularity > 100
  )

sum(invalid_popularity)

#Popularity values outside 0 to 100 are treated as invalid

clean_data[
  invalid_popularity,
  c(
    "track_id",
    "track_name",
    "popularity"
  )
]

clean_data$popularity[
  invalid_popularity
] = NA


#-----------------------------------------------------
#Validate duration_ms

invalid_duration =
  !is.na(clean_data$duration_ms) &
  clean_data$duration_ms <= 0

sum(invalid_duration)

#Zero and negative duration values are treated as invalid

clean_data[
  invalid_duration,
  c(
    "track_id",
    "track_name",
    "duration_ms"
  )
]

clean_data$duration_ms[
  invalid_duration
] = NA


#-----------------------------------------------------
#Validate danceability

invalid_danceability =
  !is.na(clean_data$danceability) &
  (
    clean_data$danceability < 0 |
      clean_data$danceability > 1
  )

sum(invalid_danceability)

clean_data$danceability[
  invalid_danceability
] = NA


#-----------------------------------------------------
#Validate energy

invalid_energy =
  !is.na(clean_data$energy) &
  (
    clean_data$energy < 0 |
      clean_data$energy > 1
  )

sum(invalid_energy)

clean_data$energy[
  invalid_energy
] = NA


#-----------------------------------------------------
#Validate speechiness

invalid_speechiness =
  !is.na(clean_data$speechiness) &
  (
    clean_data$speechiness < 0 |
      clean_data$speechiness > 1
  )

sum(invalid_speechiness)

clean_data$speechiness[
  invalid_speechiness
] = NA


#-----------------------------------------------------
#Validate acousticness

invalid_acousticness =
  !is.na(clean_data$acousticness) &
  (
    clean_data$acousticness < 0 |
      clean_data$acousticness > 1
  )

sum(invalid_acousticness)

clean_data$acousticness[
  invalid_acousticness
] = NA


#-----------------------------------------------------
#Validate instrumentalness

invalid_instrumentalness =
  !is.na(clean_data$instrumentalness) &
  (
    clean_data$instrumentalness < 0 |
      clean_data$instrumentalness > 1
  )

sum(invalid_instrumentalness)

clean_data$instrumentalness[
  invalid_instrumentalness
] = NA


#-----------------------------------------------------
#Validate liveness

invalid_liveness =
  !is.na(clean_data$liveness) &
  (
    clean_data$liveness < 0 |
      clean_data$liveness > 1
  )

sum(invalid_liveness)

clean_data$liveness[
  invalid_liveness
] = NA


#-----------------------------------------------------
#Validate valence

invalid_valence =
  !is.na(clean_data$valence) &
  (
    clean_data$valence < 0 |
      clean_data$valence > 1
  )

sum(invalid_valence)

clean_data$valence[
  invalid_valence
] = NA


#-----------------------------------------------------
#Validate key

invalid_key =
  !is.na(clean_data$key) &
  !(
    clean_data$key %in%
      c(-1, 0:11)
  )

sum(invalid_key)

#Key supports standard pitch classes 0 to 11
#-1 may represent that no musical key was detected

clean_data[
  invalid_key,
  c(
    "track_id",
    "track_name",
    "key"
  )
]

clean_data$key[
  invalid_key
] = NA


#-----------------------------------------------------
#Validate mode

invalid_mode =
  !is.na(clean_data$mode) &
  !(
    clean_data$mode %in%
      c(0, 1)
  )

sum(invalid_mode)

#Mode only supports 0 for minor and 1 for major

clean_data[
  invalid_mode,
  c(
    "track_id",
    "track_name",
    "mode"
  )
]

clean_data$mode[
  invalid_mode
] = NA


#-----------------------------------------------------
#Validate time_signature

invalid_time_signature =
  !is.na(clean_data$time_signature) &
  !(
    clean_data$time_signature %in%
      3:7
  )

sum(invalid_time_signature)

clean_data[
  invalid_time_signature,
  c(
    "track_id",
    "track_name",
    "time_signature"
  )
]

clean_data$time_signature[
  invalid_time_signature
] = NA


#-----------------------------------------------------
#Validate tempo

invalid_tempo =
  !is.na(clean_data$tempo) &
  clean_data$tempo <= 0

sum(invalid_tempo)

#Zero and negative tempo values are treated as invalid, following 02c.

clean_data[
  invalid_tempo,
  c(
    "track_id",
    "track_name",
    "tempo"
  )
]

clean_data$tempo[
  invalid_tempo
] = NA


sum(
  !is.na(clean_data$tempo) &
    clean_data$tempo == 0
)

#No zero tempo values remain after the completed 02c validation rule.


#-----------------------------------------------------
#Investigate loudness

summary(clean_data$loudness)

sum(
  !is.na(clean_data$loudness) &
    (
      clean_data$loudness < -60 |
        clean_data$loudness > 0
    )
)

#Use the completed 02c loudness rule rather than the older preparation rule.
invalid_loudness = !is.na(clean_data$loudness) &
  (clean_data$loudness < -60 | clean_data$loudness > 0)
clean_data$loudness[invalid_loudness] = NA


#-----------------------------------------------------
#Create one validation summary

invalid_values_summary = data.frame(

  variable = c(
    "popularity",
    "duration_ms",
    "danceability",
    "energy",
    "speechiness",
    "acousticness",
    "instrumentalness",
    "liveness",
    "valence",
    "loudness",
    "tempo",
    "key",
    "mode",
    "time_signature"
  ),

  invalid_count = c(
    sum(invalid_popularity),
    sum(invalid_duration),
    sum(invalid_danceability),
    sum(invalid_energy),
    sum(invalid_speechiness),
    sum(invalid_acousticness),
    sum(invalid_instrumentalness),
    sum(invalid_liveness),
    sum(invalid_valence),
    sum(invalid_loudness),
    sum(invalid_tempo),
    sum(invalid_key),
    sum(invalid_mode),
    sum(invalid_time_signature)
  )
)

invalid_values_summary


missing_after_validation =
  sum(is.na(clean_data))

new_missing_from_validation =
  missing_after_validation -
  missing_before_validation

new_missing_from_validation

#Confirmed invalid values have now been changed to NA
#No complete rows are removed during validation




# Preserve observed values for later modelling and for checking imputation.
prepared_before_imputation = clean_data
for (column in c(numeric_columns, "explicit")) {
  if (all(is.na(clean_data[[column]][!is.na(clean_data$popularity)]))) {
    stop(paste("No observed values available for missing-value treatment:", column))
  }
}

#Check the dataset before handling missing values

nrow(clean_data)
ncol(clean_data)

colSums(is.na(clean_data))
sum(is.na(clean_data))

#There are 113579 rows and 20 columns; missing counts are printed above
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

#Remaining missing values in predictors and descriptive variables are
#counted above; comments must not substitute for the current data audit.

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

# Enforce the prepared-data contract before exporting or exploring.
stopifnot(
  identical(names(clean_data), required_columns),
  nrow(clean_data) > 1,
  !anyNA(clean_data),
  !anyDuplicated(clean_data),
  is.logical(clean_data$explicit),
  all(clean_data$popularity >= 0 & clean_data$popularity <= 100),
  all(clean_data$duration_ms > 0),
  all(clean_data$key %in% c(-1, 0:11)),
  all(clean_data$mode %in% c(0, 1)),
  all(clean_data$time_signature %in% 3:7),
  all(clean_data$tempo > 0),
  all(clean_data$loudness >= -60 & clean_data$loudness <= 0)
)
score_columns = c("danceability", "energy", "speechiness", "acousticness",
  "instrumentalness", "liveness", "valence")
for (column in numeric_columns) {
  stopifnot(all(is.finite(clean_data[[column]])))
}
for (column in score_columns) {
  stopifnot(all(clean_data[[column]] >= 0 & clean_data[[column]] <= 1))
}
for (column in integer_columns) stopifnot(is.integer(clean_data[[column]]))
stopifnot(nrow(raw_data) - nrow(clean_data) ==
  rows_removed_duplicates + genre_duplicates_removed +
  popularity_rows_removed + final_duplicates_removed)


write.csv(
  clean_data,
  file.path(output_dir, "spotify_tracks_clean.csv"),
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

# STAGE 3: GENERAL EXPLORATION
# 3.0 GENERAL DATA EXPLORATION
# ============================================================

library(ggplot2)

#Use the prepared dataset for general exploration

exploration_data = clean_data


#-----------------------------------------------------
#Check the dataset before exploration

nrow(exploration_data)
ncol(exploration_data)

names(exploration_data)

str(exploration_data)

head(exploration_data)

sum(is.na(exploration_data))
sum(duplicated(exploration_data))

#The dataset has already completed the cleaning and validation
#steps performed in the Data Preparation section


#-----------------------------------------------------
#General dataset profile

total_records =
  nrow(exploration_data)

unique_tracks =
  length(
    unique(exploration_data$track_id)
  )

unique_artist_entries =
  length(
    unique(exploration_data$artists)
  )

unique_albums =
  length(
    unique(exploration_data$album_name)
  )

unique_genres =
  length(
    unique(exploration_data$track_genre)
  )


total_records
unique_tracks
unique_artist_entries
unique_albums
unique_genres


dataset_profile = data.frame(

  Measurement = c(
    "Total records",
    "Unique track IDs",
    "Unique artist entries",
    "Unique albums",
    "Unique genres"
  ),

  Value = c(
    total_records,
    unique_tracks,
    unique_artist_entries,
    unique_albums,
    unique_genres
  )
)

dataset_profile


#-----------------------------------------------------
#Check repeated track IDs

track_id_counts =
  table(exploration_data$track_id)

repeated_track_ids =
  track_id_counts[
    track_id_counts > 1
  ]

number_repeated_track_ids =
  length(repeated_track_ids)

rows_with_repeated_track_ids =
  sum(
    exploration_data$track_id %in%
      names(repeated_track_ids)
  )


number_repeated_track_ids
rows_with_repeated_track_ids

#Repeated track IDs are recorded as a dataset characteristic.
#They are not removed because repeated IDs do not automatically
#represent exact duplicate records.


#-----------------------------------------------------
#General summary of all variables

summary(exploration_data)


#-----------------------------------------------------
#Numerical variables used in general exploration

numeric_variables = c(
  "popularity",
  "duration_ms",
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo"
)


#-----------------------------------------------------
#Numerical summary statistics

numeric_summary = data.frame(

  Variable =
    numeric_variables,

  Minimum =
    sapply(
      exploration_data[numeric_variables],
      min,
      na.rm = TRUE
    ),

  Q1 =
    sapply(
      exploration_data[numeric_variables],
      function(x)
        quantile(
          x,
          0.25,
          na.rm = TRUE
        )
    ),

  Median =
    sapply(
      exploration_data[numeric_variables],
      median,
      na.rm = TRUE
    ),

  Mean =
    sapply(
      exploration_data[numeric_variables],
      mean,
      na.rm = TRUE
    ),

  Q3 =
    sapply(
      exploration_data[numeric_variables],
      function(x)
        quantile(
          x,
          0.75,
          na.rm = TRUE
        )
    ),

  Maximum =
    sapply(
      exploration_data[numeric_variables],
      max,
      na.rm = TRUE
    ),

  Standard_Deviation =
    sapply(
      exploration_data[numeric_variables],
      sd,
      na.rm = TRUE
    )
)

numeric_summary


#-----------------------------------------------------
#Popularity summary

summary(
  exploration_data$popularity
)

popularity_mean =
  mean(
    exploration_data$popularity,
    na.rm = TRUE
  )

popularity_median =
  median(
    exploration_data$popularity,
    na.rm = TRUE
  )

popularity_sd =
  sd(
    exploration_data$popularity,
    na.rm = TRUE
  )

popularity_min =
  min(
    exploration_data$popularity,
    na.rm = TRUE
  )

popularity_max =
  max(
    exploration_data$popularity,
    na.rm = TRUE
  )


popularity_mean
popularity_median
popularity_sd
popularity_min
popularity_max


#-----------------------------------------------------
#Percentage of valid tracks with popularity = 0

zero_popularity_count =
  sum(
    exploration_data$popularity == 0,
    na.rm = TRUE
  )

valid_popularity_count =
  sum(
    !is.na(
      exploration_data$popularity
    )
  )

zero_popularity_percentage =
  zero_popularity_count /
  valid_popularity_count * 100


zero_popularity_count
zero_popularity_percentage


#-----------------------------------------------------
#Popularity histogram

exploration_plot = ggplot(
  exploration_data,
  aes(x = popularity)
) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Track Popularity",
    x = "Popularity Score",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_01.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Popularity boxplot

exploration_plot = ggplot(
  exploration_data,
  aes(y = popularity)
) +
  geom_boxplot(
    fill = "lightblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Boxplot of Track Popularity",
    y = "Popularity Score"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_02.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Duration distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = duration_ms)
) +
  geom_histogram(
    binwidth = 30000,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Track Duration",
    x = "Duration (Milliseconds)",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_03.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Danceability distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = danceability)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "coral",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Danceability",
    x = "Danceability",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_04.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Energy distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = energy)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "maroon",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Energy",
    x = "Energy",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_05.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Loudness distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = loudness)
) +
  geom_histogram(
    binwidth = 2,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Loudness",
    x = "Loudness (dB)",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_06.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Tempo distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = tempo)
) +
  geom_histogram(
    binwidth = 5,
    fill = "coral",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Track Tempo",
    x = "Tempo (BPM)",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_07.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Acousticness distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = acousticness)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "lightblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Acousticness",
    x = "Acousticness",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_08.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Instrumentalness distribution

exploration_plot = ggplot(
  exploration_data,
  aes(x = instrumentalness)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Instrumentalness",
    x = "Instrumentalness",
    y = "Frequency"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_09.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Explicit track distribution

explicit_counts =
  table(
    exploration_data$explicit,
    useNA = "ifany"
  )

explicit_counts

prop.table(
  explicit_counts
) * 100


exploration_plot = ggplot(
  exploration_data,
  aes(x = factor(explicit))
) +
  geom_bar(
    fill = "coral",
    na.rm = TRUE
  ) +
  labs(
    title = "Explicit and Non-Explicit Tracks",
    x = "Explicit",
    y = "Number of Records"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_10.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Mode distribution

mode_counts =
  table(
    exploration_data$mode,
    useNA = "ifany"
  )

mode_counts

prop.table(
  mode_counts
) * 100

#0 = Minor
#1 = Major


#-----------------------------------------------------
#Key distribution

key_counts =
  table(
    exploration_data$key,
    useNA = "ifany"
  )

key_counts


exploration_plot = ggplot(
  exploration_data,
  aes(x = factor(key))
) +
  geom_bar(
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Musical Keys",
    x = "Key",
    y = "Number of Records"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_11.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Time signature distribution

time_signature_counts =
  table(
    exploration_data$time_signature,
    useNA = "ifany"
  )

time_signature_counts

prop.table(
  time_signature_counts
) * 100


exploration_plot = ggplot(
  exploration_data,
  aes(
    x = factor(time_signature)
  )
) +
  geom_bar(
    fill = "lightblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Time Signatures",
    x = "Time Signature",
    y = "Number of Records"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_12.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Genre distribution

genre_counts =
  sort(
    table(
      exploration_data$track_genre
    ),
    decreasing = TRUE
  )

genre_counts

length(genre_counts)



#-----------------------------------------------------
#Basic popularity comparison by explicit status

popularity_by_explicit =
  exploration_data %>%
  group_by(explicit) %>%
  summarise(

    Count =
      n(),

    Mean_Popularity =
      mean(
        popularity,
        na.rm = TRUE
      ),

    Median_Popularity =
      median(
        popularity,
        na.rm = TRUE
      )
  )

popularity_by_explicit


exploration_plot = ggplot(
  exploration_data,
  aes(
    x = factor(explicit),
    y = popularity
  )
) +
  geom_boxplot(
    fill = "lightblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Popularity by Explicit Status",
    x = "Explicit",
    y = "Popularity Score"
  )

# Save the plot even when this script runs using source() or Rscript.
ggsave(
  file.path(output_dir, "plots", "exploration_13.png"),
  plot = exploration_plot, width = 8, height = 5, dpi = 120
)
if (interactive()) print(exploration_plot)



#-----------------------------------------------------
#Basic popularity comparison by mode

popularity_by_mode =
  exploration_data %>%
  group_by(mode) %>%
  summarise(

    Count =
      n(),

    Mean_Popularity =
      mean(
        popularity,
        na.rm = TRUE
      ),

    Median_Popularity =
      median(
        popularity,
        na.rm = TRUE
      )
  )

popularity_by_mode


#-----------------------------------------------------
#Basic correlation exploration

correlation_variables = c(
  "popularity",
  "duration_ms",
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo"
)


correlation_data =
  exploration_data[
    correlation_variables
  ]


correlation_matrix =
  cor(
    correlation_data,
    use = "pairwise.complete.obs"
  )

correlation_matrix


#-----------------------------------------------------
#Correlation of each numerical variable with popularity

popularity_correlations = data.frame(

  Variable =
    correlation_variables[
      correlation_variables !=
        "popularity"
    ],

  Correlation =
    as.numeric(
      correlation_matrix[
        "popularity",
        correlation_variables !=
          "popularity"
      ]
    )
)


popularity_correlations =
  popularity_correlations[
    order(
      abs(
        popularity_correlations$Correlation
      ),
      decreasing = TRUE
    ),
  ]

popularity_correlations

#The correlations are used only for general exploration.
#Detailed statistical analysis is completed later under
#the individual objectives.


#-----------------------------------------------------
#Output folders were created during setup.


#-----------------------------------------------------
#Export useful exploration tables

write.csv(
  dataset_profile,
  file.path(output_dir, "tables", "general_dataset_profile.csv"),
  row.names = FALSE
)

write.csv(
  numeric_summary,
  file.path(output_dir, "tables", "general_numeric_summary.csv"),
  row.names = FALSE
)


write.csv(
  popularity_by_explicit,
  file.path(output_dir, "tables", "popularity_by_explicit.csv"),
  row.names = FALSE
)

write.csv(
  popularity_by_mode,
  file.path(output_dir, "tables", "popularity_by_mode.csv"),
  row.names = FALSE
)

write.csv(
  popularity_correlations,
  file.path(output_dir, "tables", "popularity_correlations.csv"),
  row.names = FALSE
)


# ============================================================
# GENERAL EXPLORATION SUMMARY
# ============================================================

cat(
  "\n--- GENERAL EXPLORATION SUMMARY ---\n"
)

cat(
  "Total records:",
  total_records,
  "\n"
)

cat(
  "Unique track IDs:",
  unique_tracks,
  "\n"
)

cat(
  "Unique genres:",
  unique_genres,
  "\n"
)

cat(
  "Repeated track IDs:",
  number_repeated_track_ids,
  "\n"
)

cat(
  "Missing values:",
  sum(is.na(exploration_data)),
  "\n"
)

cat(
  "Exact duplicates:",
  sum(duplicated(exploration_data)),
  "\n"
)

cat(
  "Mean popularity:",
  popularity_mean,
  "\n"
)

cat(
  "Median popularity:",
  popularity_median,
  "\n"
)
# Correlation requires observed variation in every included variable.
if (any(!is.finite(correlation_matrix))) {
  stop("Correlation contains undefined values; check constant numeric variables.")
}
stopifnot(identical(exploration_data, clean_data))
write.csv(invalid_values_summary,
  file.path(output_dir, "tables", "invalid_values_summary.csv"), row.names = FALSE)
write.csv(data.frame(Variable = names(prepared_before_imputation),
  Missing_Before_Treatment = colSums(is.na(prepared_before_imputation))),
  file.path(output_dir, "tables", "missing_before_treatment.csv"), row.names = FALSE)
cat("\nStages 1-3 completed and data checks passed. Outputs:", output_dir, "\n")
