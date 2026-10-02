# ================================================================
# GROUP 25 | PROGRAMMING FOR DATA ANALYSIS | ONE-FILE SUBMISSION
# Waleed Siddig Mohamed Siddig, TP078784
# Abdalla Hassan Basher Abdelrahman, TP089555
# Abdulaziz Qaderi, TP091283
# Abdulrahman Hussein Ali Mashrah, TP086182
# ================================================================
# ORDER: Main audit, preparation and exploration; Objectives 1-4.
# Run from the GROUP PROJECT ROOT, where data/raw/spotify_tracks_data.csv exists:
#   source("Group25.R")
# Or, from a terminal with the project root as working directory:
#   Rscript --vanilla Group25.R
# Packages: dplyr, ggplot2, rpart (rpart is used by Objective 2).
# All analyses below are from the uploaded original source files.
# Repairs for integration are documented in Group25_INTEGRATION_README.txt.
# ================================================================



# ======================================================================
# PART 1 | MAIN WORKFLOW: DATA AUDIT, PREPARATION AND EXPLORATION
# ======================================================================
# ============================================================
# GROUP 25 - MAIN WORKFLOW: STAGES 1 TO 3
# ============================================================
# Consolidated from 01_import_and_audit.R, 02_data_preparation.R,
# archive/cleaning/02c_value_validation.R, archive/cleaning/02d_missing_values_and_export.R
# and 03_general_exploration.R. The completed 02c/02d rules supersede
# stale validation and pending-treatment sections in 02_data_preparation.R.
# Run from the project root: source("scripts/Main.R")
# Or: Rscript --vanilla scripts/Main.R
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

explicit_counts / sum(explicit_counts) * 100


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

mode_counts / sum(mode_counts) * 100

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

time_signature_counts / sum(time_signature_counts) * 100


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
    use = "complete.obs"
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
exploration_data$duration_min = exploration_data$duration_ms / 60000
cat("\nStages 1-3 completed and data checks passed. Outputs:", output_dir, "\n")

# ================================================================
# SHARED DATA HAND-OFF TO THE FOUR OBJECTIVES
# Main saves its audit copy under outputs/main_stages_1_3, while the
# original Objective 4 expects the same cleaned dataset at data/processed.
# Export the SAME in-memory clean_data: no additional cleaning or changes.
# All objectives below use the Main-generated data, NOT a stale CSV.
# ================================================================
stopifnot(exists("clean_data"), exists("exploration_data"),
          nrow(clean_data) == 113305L,
          !anyNA(clean_data),
          identical(exploration_data$popularity, clean_data$popularity))
setwd(project_dir)
processed_path <- file.path(project_dir, "data", "processed")
dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)
write.csv(clean_data, file.path(processed_path, "spotify_tracks_clean.csv"), row.names = FALSE)
cat("\nShared hand-off: cleaned data exported to data/processed/spotify_tracks_clean.csv\n")

# In non-interactive runs, collect all four objectives' plots in a PDF.
# Interactive RStudio runs continue to display the plots in the Plots pane.
.group25_plot_device <- FALSE
if (!interactive()) {
  .group25_plot_path <- file.path(project_dir, "outputs", "Group25_All_Objective_Plots.pdf")
  pdf(.group25_plot_path, width = 12, height = 8, onefile = TRUE)
  .group25_plot_device <- TRUE
}

# ======================================================================
# PART 2 | WALEED SIDDIG MOHAMED SIDDIG, TP078784 | OBJECTIVE 1
# ======================================================================
local({
# SCREENSHOT COPY: same calculations as your final script.
# Run the whole file first. Then select each marked output block and Run.
# Capture code in the editor and results in the Console separately.
# For graphs: Plots > Zoom, then screenshot. No extra packages needed.

# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 04_objective1_audio_updated.R
# Responsible student:
# Name: Waleed Siddig Mohamed Siddig
# TP Number: TP078784

# Objective 1:
# To examine how Energy, Loudness, Valence and Tempo relate to
# Spotify popularity and assess their ability to predict its score.
# Analysis types: descriptive and predictive, followed by recommendations.

# ============================================================
# OBJECTIVE 1 DATA
# ============================================================

# Run after the group cleaning script, or place the cleaned CSV
# in your RStudio working directory and run this file directly.
# Packages: dplyr and ggplot2.
library(dplyr)
library(ggplot2)

if (!exists("exploration_data")) {
  exploration_data <- read.csv(
    "spotify_tracks_clean(1).csv",
    stringsAsFactors = FALSE
  )
}

# SCREENSHOT 1 CODE: data selection/filtering through nrow(objective1_data).
objective1_data <- exploration_data %>%
  select(popularity, energy, loudness, valence, tempo) %>%
  filter(
    !is.na(popularity),
    !is.na(energy),
    !is.na(loudness),
    !is.na(valence),
    !is.na(tempo),
    tempo > 0
  )

summary(objective1_data)
nrow(objective1_data)



# ============================================================
# ANALYSIS 1-1
# ENERGY + LOUDNESS AND POPULARITY
# Question: Do energy-loudness groups have different median popularity?
# Technique: Quantile grouping, descriptive statistics and visualisation.
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find Energy and Loudness ranges
# ------------------------------------------------------------

# SCREENSHOT 2 CODE: energy/loudness thresholds and case_when grouping.
energy_breaks <- quantile(
  objective1_data$energy,
  probs = c(0, 1/3, 2/3, 1)
)

energy_breaks


loudness_breaks <- quantile(
  objective1_data$loudness,
  probs = c(0, 1/3, 2/3, 1)
)

loudness_breaks



# ------------------------------------------------------------
# STEP 2
# Create Energy and Loudness groups
# ------------------------------------------------------------

objective1_data <- objective1_data %>%
  mutate(
    
    energy_level = case_when(
      energy <= energy_breaks[2] ~ "Low",
      energy <= energy_breaks[3] ~ "Medium",
      TRUE ~ "High"
    ),
    
    loudness_level = case_when(
      loudness <= loudness_breaks[2] ~ "Low",
      loudness <= loudness_breaks[3] ~ "Medium",
      TRUE ~ "High"
    )
  )


# Put groups in the correct order

objective1_data$energy_level <- factor(
  objective1_data$energy_level,
  levels = c("Low", "Medium", "High")
)

objective1_data$loudness_level <- factor(
  objective1_data$loudness_level,
  levels = c("Low", "Medium", "High")
)


table(objective1_data$energy_level)
table(objective1_data$loudness_level)



# ------------------------------------------------------------
# STEP 3
# Summarise Energy + Loudness profiles
# ------------------------------------------------------------

# SCREENSHOT 3 CODE: this group_by/summarise calculation.
intensity_profile <- objective1_data %>%
  group_by(energy_level, loudness_level) %>%
  summarise(
    record_count = n(),
    mean_popularity = mean(popularity),
    median_popularity = median(popularity),
    .groups = "drop"
  )

intensity_profile



# ------------------------------------------------------------
# STEP 4
# Visualise Energy + Loudness profiles
# ------------------------------------------------------------

intensity_bar_plot <- ggplot(
  intensity_profile,
  aes(
    x = energy_level,
    y = median_popularity,
    fill = loudness_level
  )
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = median_popularity), position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(
    title = "Median Popularity by Energy and Loudness",
    x = "Energy Level",
    y = "Median Popularity",
    fill = "Loudness Level"
  ) +
  theme_minimal()
print(intensity_bar_plot)

intensity_heatmap <- ggplot(intensity_profile, aes(x = energy_level, y = loudness_level, fill = median_popularity)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = median_popularity), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Median Popularity by Energy and Loudness",
       x = "Energy Level", y = "Loudness Level", fill = "Median Popularity") +
  theme_minimal()
print(intensity_heatmap)


# ============================================================
# ANALYSIS 1-2
# VALENCE + TEMPO AND POPULARITY
# Question: Do valence-tempo groups have different median popularity?
# Technique: Quantile grouping, descriptive statistics and visualisation.
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find Valence and Tempo ranges
# ------------------------------------------------------------

# SCREENSHOT 5 CODE: valence/tempo thresholds and case_when grouping.
valence_breaks <- quantile(
  objective1_data$valence,
  probs = c(0, 1/3, 2/3, 1)
)

valence_breaks


tempo_breaks <- quantile(
  objective1_data$tempo,
  probs = c(0, 1/3, 2/3, 1)
)

tempo_breaks



# ------------------------------------------------------------
# STEP 2
# Create Valence and Tempo groups
# ------------------------------------------------------------

objective1_data <- objective1_data %>%
  mutate(
    
    valence_level = case_when(
      valence <= valence_breaks[2] ~ "Low",
      valence <= valence_breaks[3] ~ "Medium",
      TRUE ~ "High"
    ),
    
    tempo_level = case_when(
      tempo <= tempo_breaks[2] ~ "Slow",
      tempo <= tempo_breaks[3] ~ "Medium",
      TRUE ~ "Fast"
    )
  )


# Put groups in correct order

objective1_data$valence_level <- factor(
  objective1_data$valence_level,
  levels = c("Low", "Medium", "High")
)

objective1_data$tempo_level <- factor(
  objective1_data$tempo_level,
  levels = c("Slow", "Medium", "Fast")
)


table(objective1_data$valence_level)
table(objective1_data$tempo_level)



# ------------------------------------------------------------
# STEP 3
# Summarise Valence + Tempo profiles
# ------------------------------------------------------------

# SCREENSHOT 6 CODE: this group_by/summarise calculation.
mood_tempo_profile <- objective1_data %>%
  group_by(valence_level, tempo_level) %>%
  summarise(
    record_count = n(),
    mean_popularity = mean(popularity),
    median_popularity = median(popularity),
    .groups = "drop"
  )

mood_tempo_profile



# ------------------------------------------------------------
# STEP 4
# Visualise Valence + Tempo profiles
# ------------------------------------------------------------

mood_tempo_plot <- ggplot(
  mood_tempo_profile,
  aes(
    x = valence_level,
    y = median_popularity,
    fill = tempo_level
  )
) +
  geom_col(position = "dodge") +
  geom_text(aes(label = median_popularity), position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(
    title = "Median Popularity by Valence and Tempo",
    x = "Valence Level",
    y = "Median Popularity",
    fill = "Tempo Level"
  ) +
  theme_minimal()
print(mood_tempo_plot)



# ============================================================
# ANALYSIS 1-3
# HIGH-POPULARITY AUDIO PROFILES
# Question: Which four-feature profiles have higher high-popularity rates?
# Technique: Grouped proportions with an overall reference rate.
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find high-popularity cutoff
# Use the 75th-percentile score: 50 in this cleaned dataset.
# High popularity means 50 or above, including records equal to 50.
# Ties make the high-popularity share about 25.8%, rather than exactly 25%.
# ------------------------------------------------------------

# SCREENSHOT 8 CODE: cutoff and overall percentage calculation.
popularity_cutoff <- quantile(
  objective1_data$popularity,
  0.75
)

popularity_cutoff

# Percentage of all records reaching the cutoff
overall_high_rate <- mean(objective1_data$popularity >= popularity_cutoff) * 100
overall_high_rate


# ------------------------------------------------------------
# STEP 2
# Combine all four Objective 1 variables
# ------------------------------------------------------------

# SCREENSHOT 9 CODE: four-feature group_by and percentage calculation.
audio_profiles <- objective1_data %>%
  group_by(
    energy_level,
    loudness_level,
    valence_level,
    tempo_level
  ) %>%
  summarise(
    total_records = n(),
    
    high_popularity_records =
      sum(popularity >= popularity_cutoff),
    
    high_popularity_rate =
      (high_popularity_records / total_records) * 100,
    
    median_popularity =
      median(popularity),
    
    .groups = "drop"
  )

audio_profiles



# ------------------------------------------------------------
# STEP 3
# Create shorter profile names
# ------------------------------------------------------------

audio_profiles$profile <- paste(
  audio_profiles$energy_level, "Energy |",
  audio_profiles$loudness_level, "Loudness |",
  audio_profiles$valence_level, "Valence |",
  audio_profiles$tempo_level, "Tempo"
)



# ------------------------------------------------------------
# STEP 4
# Select Top 10 profiles by high-popularity rate
# Profiles with fewer than 500 records are excluded
# ------------------------------------------------------------

# SCREENSHOT 10 CODE: minimum 500 records, ranking by rate and top 10.
top_audio_profiles <- audio_profiles %>%
  filter(total_records >= 500) %>%
  arrange(
    desc(high_popularity_rate),
    desc(total_records)
  ) %>%
  head(10)

top_audio_profiles


# ------------------------------------------------------------
# STEP 5
# Visualise Top High-Popularity profiles
# ------------------------------------------------------------

audio_profile_plot <- ggplot(
  top_audio_profiles,
  aes(
    x = reorder(profile, high_popularity_rate),
    y = high_popularity_rate
  )
) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = overall_high_rate, colour = "red", linetype = "dashed") +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.1f", high_popularity_rate),
        "% (n=", total_records, ")"
      )
    ),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Top Combined Profiles by High-Popularity Rate",
    subtitle = "At least 500 records per profile; red line = overall rate",
    x = "Audio Profile",
    y = "High-Popularity Rate (%)"
  ) +
  theme_minimal()
print(audio_profile_plot)

# ============================================================
# ANALYSIS 1-4: SIMPLE MULTIPLE LINEAR REGRESSION
# Question: Can the four audio features predict popularity?
# Technique: Linear Regression with a 75% / 25% track-ID split.
# ============================================================

# STEP 1: Keep complete records from the same cleaned dataset
# SCREENSHOT 12 CODE: regression preparation, split and stopifnot check.
regression_data <- exploration_data %>%
  select(track_id, popularity, energy, loudness, valence, tempo) %>%
  filter(complete.cases(.), track_id != "", tempo > 0)

# STEP 2: Keep records for each track together when splitting
set.seed(123)
track_ids <- sort(unique(regression_data$track_id), method = "radix")
training_ids <- sample(track_ids, floor(length(track_ids) * 0.75))

training_data <- regression_data %>%
  filter(track_id %in% training_ids)

testing_data <- regression_data %>%
  filter(!track_id %in% training_ids)

# Check that training and testing contain different track IDs
stopifnot(
  nrow(training_data) > 0,
  nrow(testing_data) > 0,
  var(testing_data$popularity) > 0,
  length(intersect(training_data$track_id, testing_data$track_id)) == 0
)

# STEP 3: Fit the model and predict test popularity
# SCREENSHOT 13 CODE: model formula, summary and prediction.
popularity_model <- lm(
  popularity ~ energy + loudness + valence + tempo,
  data = training_data
)

print(summary(popularity_model))

actual <- testing_data$popularity
predicted <- predict(popularity_model, newdata = testing_data)
baseline <- mean(training_data$popularity)

# STEP 4: Compare with predicting the training average
# MAE = average absolute error; RMSE penalises larger errors.
# Lower MAE and RMSE are better. Higher test R-squared is better.
# SCREENSHOT 14 CODE: baseline comparison and error calculations.
regression_results <- data.frame(
  Method = c("Training-average baseline", "Linear Regression"),
  MAE = c(mean(abs(actual - baseline)), mean(abs(actual - predicted))),
  RMSE = c(sqrt(mean((actual - baseline)^2)),
           sqrt(mean((actual - predicted)^2))),
  Test_R_squared = c(
    1 - sum((actual - baseline)^2) / sum((actual - mean(actual))^2),
    1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
)

print(regression_results, row.names = FALSE)

# STEP 5: Show the comparison on the same test records
regression_plot <- ggplot(regression_results, aes(x = Method, y = RMSE)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Can Four Audio Features Predict Popularity?",
    x = "Method", y = "Test RMSE (lower is better)"
  ) +
  theme_minimal()

print(regression_plot)
print(regression_results, row.names = FALSE)

# ============================================================
# FINAL CONCLUSION AND RECOMMENDATIONS
# ============================================================
# Descriptive: combined profiles show different popularity rates.
# Predictive: judge the regression using test errors and the baseline.
# On the supplied cleaned data, test R-squared is about 0.95%,
# indicating limited prediction despite the descriptive differences.
# Use the printed results if the data changes.
# Recommendations are decision support, not prescriptive optimisation.
# Investigate the identified profiles with their intended audiences.
# Do not assume that changing audio features causes higher popularity.
# Avoid relying on this weak model to make production decisions.
# Limits: exploratory analysis, broad categories, repeated track records,
# and any full-dataset imputation already done in the group cleaning script.



# ============================================================
# SCREENSHOT OUTPUT BLOCKS - run these ONE AT A TIME
# These commands display existing results; they do not change them.
# ============================================================

# SCREENSHOT 1 OUTPUT: data summary and number of records
summary(objective1_data[, c("popularity", "energy", "loudness", "valence", "tempo")])
nrow(objective1_data)

# SCREENSHOT 2 OUTPUT: energy and loudness cut points
energy_breaks
loudness_breaks

# SCREENSHOT 3 OUTPUT: nine intensity groups and their sizes
print(intensity_profile, n = Inf, width = Inf)

# SCREENSHOT 4 GRAPH: energy and loudness - use Plots > Zoom
print(intensity_bar_plot)
# Optional: the heatmap shows the same medians; no need for both.
# print(intensity_heatmap)

# SCREENSHOT 5 OUTPUT: valence and tempo cut points
valence_breaks
tempo_breaks

# SCREENSHOT 6 OUTPUT: nine mood/tempo groups and their sizes
print(mood_tempo_profile, n = Inf, width = Inf)

# SCREENSHOT 7 GRAPH: valence and tempo - use Plots > Zoom
print(mood_tempo_plot)

# SCREENSHOT 8 OUTPUT: high-popularity cutoff and overall percentage
popularity_cutoff
overall_high_rate

# SCREENSHOTS 9/10 OUTPUT: top 10 profiles, counts and percentages
# Leave out the long profile-name column so the table fits the Console.
print(
  top_audio_profiles %>%
    select(energy_level, loudness_level, valence_level, tempo_level,
           total_records, high_popularity_records, high_popularity_rate),
  n = Inf, width = Inf
)

# SCREENSHOT 11 GRAPH: your BEST analysis - use Plots > Zoom
print(audio_profile_plot)

# SCREENSHOT 12 OUTPUT: training/testing counts and shared IDs (must be 0)
nrow(training_data)
nrow(testing_data)
length(intersect(training_data$track_id, testing_data$track_id))

# SCREENSHOT 13 OUTPUT: regression coefficients and training R-squared
summary(popularity_model)

# SCREENSHOT 14 OUTPUT: test results - use these to judge prediction
print(regression_results, row.names = FALSE)

# SCREENSHOT 15 GRAPH: test RMSE comparison - use Plots > Zoom
print(regression_plot)

# REPORT ORDER:
# Question + technique > code > output/chart > findings/limitations.
# Write the final conclusion/recommendations as text, not a screenshot.
# The 75%/25% split applies to unique track IDs, not exactly to row counts.
# Training R-squared in summary() differs from Test_R_squared in the table.

# Integration-only assertions: original methods and settings preserved.
stopifnot(nrow(objective1_data) > 0, nrow(regression_results) == 2,
  all(is.finite(regression_results$RMSE)),
  length(intersect(training_data$track_id, testing_data$track_id)) == 0)
})
cat("\n[OBJ1] completed and checks passed.\n")

# ======================================================================
# PART 3 | ABDALLA HASSAN BASHER ABDELRAHMAN, TP089555 | OBJECTIVE 2
# ======================================================================
local({
# ============================================================
# OBJECTIVE 2: SPOTIFY TRACK POPULARITY
# Duration, Explicit Status and Genre
# Descriptive -> Predictive -> Exploratory Prescriptive Analysis
# ============================================================

#ABDALLA HASSAN BASHER ABDELRAHMAN TP089555

library(dplyr)
library(ggplot2)
library(rpart)

required_columns <- c(
  "track_id",
  "popularity",
  "duration_ms",
  "explicit",
  "track_genre"
)

stopifnot(
  all(required_columns %in% names(exploration_data))
)


# ============================================================
# ANALYSIS 2.1: DESCRIPTIVE ANALYSIS
# What happened?
# ============================================================

# Research question:
# How are track duration, explicit status and genre
# associated with observed Spotify popularity?


# ------------------------------------------------------------
# STEP 1: Convert duration and create duration groups
# ------------------------------------------------------------

objective2_data <- exploration_data %>%
  mutate(
    duration_min = as.numeric(duration_ms) / 60000
  )

objective2_data$duration_group <- cut(
  objective2_data$duration_min,

  breaks = c(
    -Inf,
    2,
    3,
    4,
    5,
    6.5,
    Inf
  ),

  labels = c(
    "Very short (<2 min)",
    "Short (2-3 min)",
    "Typical (3-4 min)",
    "Moderately long (4-5 min)",
    "Long (5-6.5 min)",
    "Very long (>6.5 min)"
  ),

  right = FALSE
)

print(
  table(objective2_data$duration_group)
)


# ------------------------------------------------------------
# STEP 2: Summarise popularity by duration
# ------------------------------------------------------------

duration_summary <- objective2_data %>%
  group_by(duration_group) %>%
  summarise(track_count = n(),
  mean_popularity = mean(popularity,na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE ),
    .groups = "drop") %>%arrange(
    desc(median_popularity))

print(duration_summary)


# ------------------------------------------------------------
# STEP 3: Visualise popularity by duration
# ------------------------------------------------------------

plot_duration <- ggplot(
  objective2_data,
  aes(
    x = duration_group,
    y = popularity
  )
) +
  geom_boxplot() +
  labs(
    title = "Popularity by Track Duration",
    x = "Track Duration",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_duration)


# ------------------------------------------------------------
# STEP 4: Compare duration and explicit status
# ------------------------------------------------------------

duration_explicit_summary <- objective2_data %>%
  group_by(duration_group,
    explicit ) %>%
  summarise(track_count = n(),
    mean_popularity = mean( popularity, na.rm = TRUE ),
    median_popularity = median( popularity,na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    explicit_status = ifelse(
      explicit,
      "Explicit",
      "Non-explicit"
    )
  )

print(duration_explicit_summary)


# ------------------------------------------------------------
# STEP 5: Visualise duration and explicit status
# ------------------------------------------------------------

plot_duration_explicit <- ggplot(
  duration_explicit_summary,
  aes(
    x = duration_group,
    y = median_popularity,
    group = explicit_status,
    linetype = explicit_status
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Median Popularity by Duration and Explicit Status",
    x = "Track Duration",
    y = "Median Popularity",
    linetype = "Explicit Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_duration_explicit)


# ------------------------------------------------------------
# STEP 6: Compare genres among 3-5-minute tracks
# ------------------------------------------------------------

genre_3_5 <- objective2_data %>%
  filter(
    duration_group %in% c( "Typical (3-4 min)","Moderately long (4-5 min)")) %>%
  group_by(track_genre) %>%
  summarise(track_count = n(),
    mean_popularity = mean(
      popularity,
      na.rm = TRUE
    ),
    median_popularity = median( popularity, na.rm = TRUE ),
    .groups = "drop"
  ) %>%
  arrange(desc(median_popularity),desc(mean_popularity))
top_10_genres <- genre_3_5 %>%
  slice_head(
    n = 10
  )

print(top_10_genres)


# ------------------------------------------------------------
# STEP 7: Visualise the leading genres
# ------------------------------------------------------------

plot_genres <- ggplot(
  top_10_genres,
  aes(
    x = reorder(
      track_genre,
      median_popularity
    ),
    y = median_popularity
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Genres Among 3-5 Minute Tracks",
    x = "Track Genre",
    y = "Median Popularity"
  ) +
  theme_minimal()

print(plot_genres)


# ------------------------------------------------------------
# STEP 8: Define high popularity for descriptive analysis
# ------------------------------------------------------------

# This cutoff describes the complete historical dataset.
# The predictive analysis below computes its own training cutoff.

popularity_cutoff <- quantile(
  objective2_data$popularity,
  0.75,
  na.rm = TRUE
)

overall_high_rate <- mean(
  objective2_data$popularity >= popularity_cutoff,
  na.rm = TRUE
)

cat("Historical popularity cutoff:", popularity_cutoff, "\n")
cat("Overall high-popularity rate:", overall_high_rate * 100, "%\n")


# ------------------------------------------------------------
# STEP 9: Create historical genre-duration-explicit profiles
# ------------------------------------------------------------

profile_analysis <- objective2_data %>%
  filter(
    !is.na(track_genre),
    !is.na(duration_group),
    !is.na(explicit),
    !is.na(popularity)
  ) %>%
  group_by(track_genre, duration_group, explicit) %>%
  summarise(total_tracks = n(),
    high_popularity_tracks = sum( popularity >= popularity_cutoff ),
    median_popularity = median( popularity),
    .groups = "drop"
  ) %>%
  mutate( high_popularity_rate = high_popularity_tracks / total_tracks * 100,
    expected_high_tracks = total_tracks * overall_high_rate,
    excess_high_tracks =
      high_popularity_tracks - expected_high_tracks,
    lift =
      (high_popularity_tracks / total_tracks) / overall_high_rate,
    explicit_status = ifelse(
      explicit,
      "Explicit",
      "Non-explicit"
    ),
    profile = paste(track_genre, duration_group, explicit_status,
      sep = " | "
    )
  )

cat("Observed historical profiles:", nrow(profile_analysis), "\n")


# ------------------------------------------------------------
# STEP 10: Rank the top 12 historical profiles
# ------------------------------------------------------------

top_profiles <- profile_analysis %>%
  arrange(desc(excess_high_tracks)) %>% 
  slice_head(n = 12)

print(
  top_profiles %>%
    select(
      track_genre,
      duration_group,
      explicit_status,
      total_tracks,
      high_popularity_tracks,
      high_popularity_rate,
      excess_high_tracks,
      lift
    ),
  n = 12
)


# ------------------------------------------------------------
# STEP 11: Visualise historical profiles
# ------------------------------------------------------------

plot_profiles <- ggplot(
  top_profiles,
  aes(
    x = reorder(
      profile,
      excess_high_tracks
    ),
    y = excess_high_tracks,
    fill = duration_group
  )
) +
  geom_col() +
  geom_text(
    aes( label = paste0(
        round(high_popularity_rate, 1),
        "% | ",
        round(lift, 2),
        "x | n=",
        total_tracks
      )
    ),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.38)
    )
  ) +
  labs(
    title = "Leading Historical High-Popularity Profiles",
    subtitle = "Ranked by high-popularity tracks above expectation",
    x = "Genre, Duration and Explicit Status",
    y = "High-Popularity Tracks Above Expectation",
    fill = "Duration"
  ) +
  theme_minimal()

print(plot_profiles)


# ============================================================
# ANALYSIS 2.2: PREDICTIVE ANALYSIS
# What is likely to happen?
# ============================================================

# Research question:
# Can genre, duration and explicit status predict whether
# an unseen track belongs to the high-popularity group?


# ------------------------------------------------------------
# STEP 1: Prepare data and split by unique track ID
# ------------------------------------------------------------

data2 <- objective2_data %>%
  filter(
    !is.na(track_id),
    !is.na(popularity),
    !is.na(track_genre),
    !is.na(duration_group),
    !is.na(explicit)
  )

# The same track ID must not appear in both datasets.
set.seed(123)

ids <- sample(unique(data2$track_id))

test_ids <- ids[ seq_len(floor(length(ids) * 0.20)
  )
]

train <- data2 %>% filter( !(track_id %in% test_ids))
test <- data2 %>% filter( track_id %in% test_ids)
stopifnot( length(intersect( unique(train$track_id),
    unique(test$track_id)
  )) == 0
)

cat("Training records:", nrow(train), "\n")
cat("Testing records:", nrow(test), "\n")


# ------------------------------------------------------------
# STEP 2: Define high popularity from TRAINING data
# ------------------------------------------------------------

cutoff <- quantile(
  train$popularity,
  0.75,
  na.rm = TRUE
)

train$high <- as.integer(
  train$popularity >= cutoff
)

test$high <- as.integer(
  test$popularity >= cutoff
)

cat("Predictive popularity cutoff:", cutoff, "\n")


# ------------------------------------------------------------
# STEP 3: Prepare genre categories for the decision tree
# ------------------------------------------------------------

# Choose 14 focal genres using TRAINING records only.
# Every other genre becomes 'Other'. No tracks are deleted.

focus <- train %>%
  group_by(
    track_genre
  ) %>%
  summarise(
    high_count = sum(high),
    .groups = "drop"
  ) %>%
  arrange(
    desc(high_count)
  ) %>%
  slice_head(
    n = 14
  ) %>%
  pull(
    track_genre
  )

levels_genre <- c(
  as.character(focus),
  "Other"
)

train$genre_group <- factor(
  ifelse(
    train$track_genre %in% focus,
    as.character(train$track_genre),
    "Other"
  ),
  levels = levels_genre
)

test$genre_group <- factor(
  ifelse(
    test$track_genre %in% focus,
    as.character(test$track_genre),
    "Other"
  ),
  levels = levels_genre
)

train$high_label <- factor(
  ifelse(
    train$high == 1,
    "High",
    "Not high"
  ),
  levels = c(
    "Not high",
    "High"
  )
)

print(focus)


# ------------------------------------------------------------
# STEP 4: Train and prune the decision tree
# ------------------------------------------------------------

tree <- rpart(
  high_label ~ genre_group + duration_group + explicit,

  data = train,
  method = "class",

  control = rpart.control(
    minsplit = 150,
    minbucket = 60,
    maxdepth = 6,
    cp = 0.001,
    xval = 5
  )
)

printcp(tree)

best_cp <- tree$cptable[
  which.min(tree$cptable[, "xerror"]),
  "CP"
]

model <- prune(
  tree,
  cp = best_cp
)

# Check which of the proposed predictors the tree actually used.
used_variables <- unique(
  model$frame$var[
    model$frame$var != "<leaf>"
  ]
)

cat("Predictors used by the tree:\n")
print(used_variables)


# ------------------------------------------------------------
# STEP 5: Predict the complete unseen testing dataset
# ------------------------------------------------------------

test$pred <- predict(
  model,
  newdata = test,
  type = "prob"
)[, "High"]

test$class <- as.integer(
  test$pred >= 0.50
)

stopifnot(
  length(test$pred) == nrow(test),
  all(is.finite(test$pred))
)


# ------------------------------------------------------------
# STEP 6: Evaluate the classification results
# ------------------------------------------------------------

confusion <- table(
  Actual = factor(
    test$high,
    levels = c(0, 1)
  ),

  Predicted = factor(
    test$class,
    levels = c(0, 1)
  )
)

print(confusion)

true_positive <- confusion["1", "1"]
false_positive <- confusion["0", "1"]
false_negative <- confusion["1", "0"]

majority_class <- as.integer(
  mean(train$high) >= 0.50
)

accuracy <- mean(
  test$class == test$high
)

baseline_accuracy <- mean(
  test$high == majority_class
)

precision <- if (true_positive + false_positive > 0) {
  true_positive / (true_positive + false_positive)
} else {
  NA_real_
}

recall <- if (true_positive + false_negative > 0) {
  true_positive / (true_positive + false_negative)
} else {
  NA_real_
}

brier_score <- mean(
  (test$pred - test$high)^2
)

baseline_brier <- mean(
  (mean(train$high) - test$high)^2
)

evaluation <- data.frame(
  Metric = c(
    "Accuracy",
    "Baseline accuracy",
    "Precision",
    "Recall",
    "Brier score",
    "Baseline Brier score"
  ),

  Result = c(
    accuracy,
    baseline_accuracy,
    precision,
    recall,
    brier_score,
    baseline_brier
  )
)

print(evaluation)


# ------------------------------------------------------------
# STEP 7: Predictive visualisation - confusion matrix
# ------------------------------------------------------------

# Convert confusion matrix to data frame
confusion_df <- as.data.frame(confusion)
colnames(confusion_df) <- c("Actual", "Predicted", "Freq")

# Add result type: correct or wrong
confusion_df$result_type <- ifelse(
  confusion_df$Actual == confusion_df$Predicted,
  "Correct",
  "Wrong"
)
# Optional: better labels for axes
confusion_df$Actual <- factor(confusion_df$Actual,
                              levels = c(0, 1),
                              labels = c("Not high", "High"))
confusion_df$Predicted <- factor(confusion_df$Predicted,
                                 levels = c(0, 1),
                                 labels = c("Not high", "High"))

# Plot
print(ggplot(confusion_df, aes(x = Predicted, y = Actual, fill = result_type)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = Freq), size = 9, color = "black") +
  scale_fill_manual(
    values = c("Correct" = "#66BB6A", "Wrong" = "#EF5350")
  ) +
  labs(
    title = "Decision Tree: Actual vs Predicted Popularity",
    subtitle = "Results on unseen testing tracks",
    x = "Predicted Category",
    y = "Actual Category",
    fill = "Prediction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ))


# ============================================================
# ANALYSIS 2.3: EXPLORATORY PRESCRIPTIVE ANALYSIS
# What could we do based on the observed patterns?
# ============================================================

# Research question:
# Which observed genre-duration-explicit profiles are
# supported by training evidence and unseen test observations?


# ------------------------------------------------------------
# STEP 1: Identify candidate profiles using TRAINING data
# ------------------------------------------------------------

# Retain profiles with at least 100 training records.
# This is a practical support threshold, not a significance test.

candidates <- train %>%
  filter(
    track_genre %in% focus
  ) %>%
  group_by(
    track_genre,
    genre_group,
    duration_group,
    explicit
  ) %>%
  summarise(
    tracks = n(),
    historical = 100 * mean(high),
    .groups = "drop"
  ) %>%
  filter(
    tracks >= 100
  )


# ------------------------------------------------------------
# STEP 2: Predict candidate profiles and select one per genre
# ------------------------------------------------------------

candidates$predicted <- 100 * predict(
  model,
  newdata = candidates,
  type = "prob"
)[, "High"]

# When the tree gives the same prediction, historical training
# rate breaks the tie. This is NOT a distinct model prediction.

recommendations <- candidates %>%
  group_by(
    track_genre
  ) %>%
  arrange(
    desc(predicted),
    desc(historical),
    desc(tracks),
    .by_group = TRUE
  ) %>%
  slice_head(
    n = 1
  ) %>%
  ungroup() %>%
  arrange(
    desc(predicted),
    desc(historical)
  )

print(recommendations, n = 14)


# ------------------------------------------------------------
# STEP 3: Validate selections on UNSEEN testing observations
# ------------------------------------------------------------

# Do not change the selected profiles after viewing test rates.

validation <- test %>%
  inner_join(
    recommendations %>%
      select(
        track_genre,
        duration_group,
        explicit,
        historical,
        predicted
      ),

    by = c(
      "track_genre",
      "duration_group",
      "explicit"
    )
  ) %>%
  group_by(
    track_genre,
    duration_group,
    explicit,
    historical,
    predicted
  ) %>%
  summarise(
    test_tracks = n(),
    test_rate = 100 * mean(high),
    .groups = "drop"
  ) %>%
  mutate(
    difference = test_rate - historical
  )

print(validation, n = 14)


# ------------------------------------------------------------
# STEP 4: Visualise training versus testing for selections
# ------------------------------------------------------------

validation_plot <- validation %>%
  mutate(
    profile = paste(
      track_genre,
      duration_group,
      ifelse(
        explicit,
        "Explicit",
        "Non-explicit"
      ),
      sep = " | "
    )
  )

plot_validation <- ggplot(
  validation_plot,
  aes(
    y = reorder(profile, test_rate)
  )
) +
  geom_segment(
    aes(
      x = historical,
      xend = test_rate,
      yend = reorder(profile, test_rate)
    ),
    color = "grey65",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      x = historical,
      color = "Training"
    ),
    size = 3
  ) +
  geom_point(
    aes(
      x = test_rate,
      color = "Testing"
    ),
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title = "Validation of Selected Track Profiles",
    subtitle = "Historical training rates vs unseen testing rates",
    x = "High-Popularity Rate (%)",
    y = "Selected Track Profile",
    color = "Dataset"
  ) +
  theme_minimal()

print(plot_validation)


# ------------------------------------------------------------
# STEP 5: Compare durations within three individual genres
# ------------------------------------------------------------

# Analyse non-explicit chill, k-pop and pop-film tracks.
# Include duration groups with at least 100 training records.

selected_genres <- c(
  "pop-film",
  "k-pop",
  "chill"
)

train_compare <- train %>%
  filter(
    track_genre %in% selected_genres,
    explicit == FALSE
  ) %>%
  group_by(
    track_genre,
    duration_group
  ) %>%
  summarise(
    train_n = n(),
    train_rate = 100 * mean(high),
    .groups = "drop"
  ) %>%
  filter(
    train_n >= 100
  )

test_compare <- test %>%
  filter(
    track_genre %in% selected_genres,
    explicit == FALSE
  ) %>%
  group_by(
    track_genre,
    duration_group
  ) %>%
  summarise(
    test_n = n(),
    test_rate = 100 * mean(high),
    .groups = "drop"
  )

duration_comparison <- train_compare %>%
  left_join(
    test_compare,
    by = c("track_genre","duration_group" )) %>%
  arrange(track_genre,duration_group)
print(duration_comparison, n = 30, width = Inf)


# ------------------------------------------------------------
# STEP 6: Final graph - training and testing duration patterns
# ------------------------------------------------------------

# This replaces the earlier graph containing testing bars only.
# Both percentages and sample sizes appear for each duration.

graph_data <- bind_rows(
  duration_comparison %>%
    transmute(
      track_genre,
      duration_group,
      dataset = "Training",
      rate = train_rate,
      tracks = train_n
    ),

  duration_comparison %>%
    transmute(
      track_genre,
      duration_group,
      dataset = "Testing",
      rate = test_rate,
      tracks = test_n
    )
) %>%
  filter(
    !is.na(rate),
    !is.na(tracks)
  )

graph_data$dataset <- factor(
  graph_data$dataset,
  levels = c(
    "Training",
    "Testing"
  )
)

plot_genre_duration <- ggplot(
  graph_data,
  aes(
    x = duration_group,
    y = rate,
    fill = dataset
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        round(rate, 1),
        "%\nn=",
        tracks
      )
    ),
    position = position_dodge(width = 0.7),
    vjust = -0.25,
    size = 2.8,
    lineheight = 0.9
  ) +
  facet_wrap(
    ~ track_genre,
    scales = "free_x"
  ) +
  coord_cartesian(
    ylim = c(0, 115)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title = "Duration Patterns Across Three Genres",
    subtitle = "Non-explicit tracks: training and testing results",
    x = "Track Duration",
    y = "High-Popularity Rate (%)",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_genre_duration)


# ============================================================
# REPORTING NOTES
# ============================================================

# The decision tree can use any of the three predictors offered,
# but may select only genre. Check 'used_variables' above.
# Duration-specific recommendations then depend on historical
# comparisons, not separate model predictions of duration effects.
# Training-versus-testing patterns are observational associations:
# they do not prove that changing a track increases its popularity.

# Integration-only assertions: original methods and settings preserved.
stopifnot(nrow(objective2_data) > 0, nrow(test) > 0,
  length(test$pred) == nrow(test), all(is.finite(test$pred)),
  all(is.finite(evaluation$Result[!is.na(evaluation$Result)])),
  length(intersect(train$track_id, test$track_id)) == 0)
})
cat("\n[OBJ2] completed and checks passed.\n")

# ======================================================================
# PART 4 | ABDULAZIZ QADERI, TP091283 | OBJECTIVE 3
# ======================================================================
local({
# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 06_objective3_categories.R
# Responsible student:
# Name:Abdulaziz Qaderi
# TP Number:TP091283
#2026-10-02

# Objective 3:
# To identify which combinations of audio characteristics are
# associated with higher Spotify track popularity.

# Target variable: popularity
# Predictors: danceability, acousticness, speechiness,
#             instrumentalness, liveness

# Uses the already prepared exploration_data from Main.R.
# ============================================================

objective3_data =
  exploration_data %>%
  select(
    popularity,
    danceability,
    acousticness,
    speechiness,
    instrumentalness,
    liveness
  )

predictor_names = c(
  "danceability",
  "acousticness",
  "speechiness",
  "instrumentalness",
  "liveness"
)

min_group_size = 30
top_n_combinations = 2



# ============================================================
# SECTION 1 - INDIVIDUAL AUDIO CHARACTERISTIC INVESTIGATION
# ============================================================

descriptive_vars = c("popularity", predictor_names)

descriptive_stats =
  data.frame(
    Variable = descriptive_vars,
    Mean = sapply(objective3_data[descriptive_vars], mean),
    Median = sapply(objective3_data[descriptive_vars], median),
    SD = sapply(objective3_data[descriptive_vars], sd)
  )

rownames(descriptive_stats) = NULL
descriptive_stats

correlation_tests =
  lapply(
    predictor_names,
    function(var) cor.test(objective3_data[[var]], objective3_data$popularity)
  )
names(correlation_tests) = predictor_names

preliminary_correlation_summary =
  data.frame(
    Variable = predictor_names,
    Correlation = sapply(correlation_tests, function(x) x$estimate),
    P_Value = sapply(correlation_tests, function(x) x$p.value)
  ) %>%
  mutate(
    Direction = case_when(
      Correlation > 0 ~ "Positive",
      Correlation < 0 ~ "Negative",
      TRUE ~ "None"
    ),
    Strength = case_when(
      abs(Correlation) < 0.10 ~ "Very weak",
      abs(Correlation) < 0.30 ~ "Weak",
      abs(Correlation) < 0.50 ~ "Moderate",
      TRUE ~ "Strong"
    )
  ) %>%
  arrange(desc(abs(Correlation)))

rownames(preliminary_correlation_summary) = NULL
preliminary_correlation_summary

get_decile_trend = function(data, variable_name) {
  data.frame(
    Characteristic = variable_name,
    Decile = ntile(data[[variable_name]], 10),
    Popularity = data$popularity
  ) %>%
    group_by(Characteristic, Decile) %>%
    summarise(
      Mean = mean(Popularity),
      Median = median(Popularity),
      .groups = "drop"
    )
}

decile_trend_data =
  bind_rows(lapply(predictor_names, function(var) get_decile_trend(objective3_data, var)))

decile_trend_long =
  rbind(
    data.frame(
      Characteristic = decile_trend_data$Characteristic,
      Decile = decile_trend_data$Decile,
      Statistic = "Mean",
      Popularity = decile_trend_data$Mean
    ),
    data.frame(
      Characteristic = decile_trend_data$Characteristic,
      Decile = decile_trend_data$Decile,
      Statistic = "Median",
      Popularity = decile_trend_data$Median
    )
  )

# Visualization 1: Average and Median Popularity Trend by Audio Characteristic

print(ggplot(
  decile_trend_long,
  aes(x = Decile, y = Popularity, color = Statistic)
) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Characteristic) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Average and Median Popularity Trend by Audio Characteristic (Decile Groups)",
    x = "Characteristic Value Group (1 = Lowest, 10 = Highest)",
    y = "Popularity",
    color = "Statistic"
  ) +
  theme_minimal())



# ============================================================
# SECTION 2 - COMBINATION ANALYSIS
# ============================================================

make_pair_profile = function(data, var1, var2) {

  values1 = data[[var1]]
  values2 = data[[var2]]

  q1_1 = quantile(values1, 0.25)
  q3_1 = quantile(values1, 0.75)
  q1_2 = quantile(values2, 0.25)
  q3_2 = quantile(values2, 0.75)

  level1 = factor(
    case_when(
      values1 < q1_1 ~ "Low",
      values1 > q3_1 ~ "High",
      TRUE ~ "Moderate"
    ),
    levels = c("Low", "Moderate", "High")
  )

  level2 = factor(
    case_when(
      values2 < q1_2 ~ "Low",
      values2 > q3_2 ~ "High",
      TRUE ~ "Moderate"
    ),
    levels = c("Low", "Moderate", "High")
  )

  labeled_data = data.frame(
    popularity = data$popularity,
    Level_1 = level1,
    Level_2 = level2
  )

  profile_summary =
    labeled_data %>%
    group_by(Level_1, Level_2) %>%
    summarise(
      n = n(),
      Mean_Popularity = mean(popularity),
      Median_Popularity = median(popularity),
      .groups = "drop"
    ) %>%
    mutate(
      Variable_1 = var1,
      Variable_2 = var2,
      Profile = paste0(var1, ": ", Level_1, "  |  ", var2, ": ", Level_2)
    )

  list(labeled_data = labeled_data, summary = profile_summary)
}

predictor_pairs = combn(predictor_names, 2, simplify = FALSE)
pair_keys = sapply(predictor_pairs, function(p) paste(p[1], p[2], sep = "_"))

pair_results = lapply(predictor_pairs, function(p) make_pair_profile(objective3_data, p[1], p[2]))
names(pair_results) = pair_keys

all_pair_profiles = bind_rows(lapply(pair_results, function(x) x$summary))

filtered_profiles =
  all_pair_profiles %>%
  filter(n >= min_group_size)

ranking_table =
  filtered_profiles %>%
  arrange(desc(Median_Popularity)) %>%
  select(Profile, Variable_1, Variable_2, Median_Popularity, Mean_Popularity, n)

head(ranking_table, 15)

pair_spread =
  filtered_profiles %>%
  group_by(Variable_1, Variable_2) %>%
  summarise(
    Spread = max(Median_Popularity) - min(Median_Popularity),
    .groups = "drop"
  ) %>%
  arrange(desc(Spread)) %>%
  mutate(
    Combination = paste(Variable_1, "x", Variable_2),
    Selection = ifelse(row_number() <= top_n_combinations, "Selected", "Not Selected")
  )

pair_spread

top_pairs = pair_spread %>% filter(Selection == "Selected")
top_pairs

# Visualization 2: Popularity Separation Across Audio Characteristic Combinations

print(ggplot(
  pair_spread,
  aes(x = reorder(Combination, Spread), y = Spread, fill = Selection)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Selected" = "steelblue", "Not Selected" = "grey70")) +
  labs(
    title = "Popularity Separation Across Audio Characteristic Combinations",
    x = "Combination",
    y = "Median Popularity Spread (Best Group - Worst Group)",
    fill = ""
  ) +
  theme_minimal())



# ============================================================
# SECTION 3 - DETAILED ANALYSIS OF SELECTED COMBINATIONS
# ============================================================

for (i in seq_len(nrow(top_pairs))) {

  v1 = top_pairs$Variable_1[i]
  v2 = top_pairs$Variable_2[i]
  key = paste(v1, v2, sep = "_")
  plot_data = pair_results[[key]]$labeled_data

  profile_summary =
    pair_results[[key]]$summary %>%
    filter(n >= min_group_size)

  best_profile = profile_summary[which.max(profile_summary$Median_Popularity), ]

  best_label = paste0(
    best_profile$Level_1, " ", v1, " + ", best_profile$Level_2, " ", v2,
    "\nMedian popularity = ", round(best_profile$Median_Popularity, 1)
  )

  print(
    ggplot(plot_data, aes(x = Level_1, y = popularity, fill = Level_2)) +
      geom_boxplot() +
      labs(
        title = paste("Popularity by", v1, "and", v2, "Combination Profile"),
        subtitle = "Popularity distribution across audio characteristic profiles",
        x = paste(v1, "Level"),
        y = "Popularity",
        fill = paste(v2, "Level")
      ) +
      scale_y_continuous(breaks = seq(0, 100, by = 10)) +
      theme_minimal()
  )
}

pair_results[["danceability_acousticness"]]$summary %>%
  filter(n >= min_group_size) %>%
  arrange(desc(Median_Popularity))

# ============================================================
# SECTION 4 - ADVANCED ANALYSIS / EXTRA FEATURE
# ============================================================

get_rmse = function(model) sqrt(mean(residuals(model)^2))

baseline_formula =
  as.formula(paste("popularity ~", paste(predictor_names, collapse = " + ")))
baseline_model = lm(baseline_formula, data = objective3_data)
summary(baseline_model)

interaction_terms = paste(top_pairs$Variable_1, top_pairs$Variable_2, sep = ":")

enhanced_formula =
  as.formula(
    paste(
      "popularity ~",
      paste(predictor_names, collapse = " + "),
      "+",
      paste(interaction_terms, collapse = " + ")
    )
  )
enhanced_model = lm(enhanced_formula, data = objective3_data)
summary(enhanced_model)

enhanced_coefficients = summary(enhanced_model)$coefficients

interaction_term_summary =
  data.frame(
    Term = interaction_terms,
    Coefficient = enhanced_coefficients[interaction_terms, "Estimate"],
    P_Value = enhanced_coefficients[interaction_terms, "Pr(>|t|)"]
  ) %>%
  mutate(
    Direction = case_when(
      Coefficient > 0 ~ "Positive",
      Coefficient < 0 ~ "Negative",
      TRUE ~ "None"
    )
  )

interaction_term_summary

model_comparison_table =
  data.frame(
    Model = c("Baseline (5 predictors)", "Enhanced (+ interactions)"),
    R_Squared = c(summary(baseline_model)$r.squared, summary(enhanced_model)$r.squared),
    Adjusted_R_Squared = c(summary(baseline_model)$adj.r.squared, summary(enhanced_model)$adj.r.squared),
    RMSE = c(get_rmse(baseline_model), get_rmse(enhanced_model))
  )

model_comparison_table

model_comparison_anova = anova(baseline_model, enhanced_model)
model_comparison_anova

model_comparison_long =
  rbind(
    data.frame(Model = model_comparison_table$Model, Metric = "R-Squared", Value = model_comparison_table$R_Squared),
    data.frame(Model = model_comparison_table$Model, Metric = "Adjusted R-Squared", Value = model_comparison_table$Adjusted_R_Squared),
    data.frame(Model = model_comparison_table$Model, Metric = "RMSE", Value = model_comparison_table$RMSE)
  )

# Visualization 5: Baseline Model vs Enhanced Model Performance

print(ggplot(
  model_comparison_long,
  aes(x = Model, y = Value, fill = Model)
) +
  geom_col() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Baseline Model vs Enhanced Model Performance",
    x = "",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))



# ============================================================
# OBJECTIVE 3 SUMMARY OUTPUT
# ============================================================

descriptive_stats
preliminary_correlation_summary

head(ranking_table, 15)
pair_spread
top_pairs

interaction_term_summary
model_comparison_table
model_comparison_anova


# ============================================================
# OBJECTIVE 3 COMPLETE
# ============================================================

# Integration-only assertions: original methods and settings preserved.
stopifnot(nrow(objective3_data) > 0, nrow(top_pairs) == 2,
  nrow(model_comparison_table) == 2,
  all(is.finite(model_comparison_table$RMSE)))
})
cat("\n[OBJ3] completed and checks passed.\n")


# ======================================================================
# PART 5 | ABDULRAHMAN HUSSEIN ALI MASHRAH, TP086182 | OBJECTIVE 4
# ======================================================================
local({
# ================================================================
# GROUP 25 | OBJECTIVE 4: MUSICAL HARMONY, METER AND PREDICTION
# Student: Abdulrahman Hussein Ali Mashrah (TP086182)
# ================================================================
# Research objective:
# Evaluate how musical key, mode and time signature relate to
# Spotify track popularity, and test their predictive usefulness.
#
# Run this script from the repository root in RStudio:
# source("scripts/07_objective4_prediction.R")
# Requirements: dplyr and ggplot2.
# Note: The shared cleaned file was prepared by the group. This
# script independently checks the key/mode/meter coding before use.
#
# Sections:
# 4-1: Descriptive summaries; an independent unique-track mode
#      test; joint key-mode popularity profiles
# 4-2: Time signature profiles with focused and comprehensive plots
# 4-3: Track-ID-separated prediction and baseline comparison,
#      followed by an independent raw-data sensitivity check
# ================================================================

library(dplyr)
library(ggplot2)

# Relative paths make the code work on the group's laptop.
input_file = "data/processed/spotify_tracks_clean.csv"
raw_file = "data/raw/spotify_tracks_data.csv"
table_dir = "outputs/tables/objective4"
figure_dir = "outputs/figures/objective4"

if (!file.exists(input_file)) {
  stop("Cleaned data not found. Run from the repository root after group cleaning.")
}
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

spotify_clean = read.csv(input_file, stringsAsFactors = FALSE)
required_columns = c("track_id", "popularity", "key", "mode", "time_signature")
stopifnot(all(required_columns %in% names(spotify_clean)))

objective4_data = spotify_clean %>%
  select(all_of(required_columns)) %>%
  mutate(
    track_id = as.character(track_id),
    mode_name = case_when(mode == 0 ~ "Minor", mode == 1 ~ "Major"),
    key_name = factor(
      key,
      levels = 0:11,
      labels = c("C", "C#/Db", "D", "D#/Eb", "E", "F",
                 "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
    )
  )

# Verify the shared dataset, rather than silently changing it here.
print(str(objective4_data))
print(colSums(is.na(objective4_data)))
print(summary(objective4_data[, c("popularity", "key", "mode", "time_signature")]))
stopifnot(
  !anyNA(objective4_data),
  all(objective4_data$popularity >= 0 & objective4_data$popularity <= 100),
  all(objective4_data$key %in% 0:11),
  all(objective4_data$mode %in% 0:1),
  all(objective4_data$time_signature %in% c(3, 4, 5)),
  all(nzchar(objective4_data$track_id))
)
cat("\nObjective 4 prepared rows:", nrow(objective4_data), "\n")

# ================================================================
# ANALYSIS 4-1: MUSICAL MODE, THEN COMBINED KEY AND MODE
# ================================================================

mode_summary = objective4_data %>%
  group_by(mode_name) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  )
print(mode_summary)

# Repeated IDs represent the same song across some genre records.
# Summarise the popularity of EACH TRACK once for the hypothesis test.
# A few track IDs have conflicting mode codes, so exclude those ambiguous
# IDs from this track-level test rather than choosing a code arbitrarily.
mode_track_data = objective4_data %>%
  group_by(track_id) %>%
  summarise(
    Mode_Codes = n_distinct(mode),
    mode = first(mode),
    Track_Popularity = mean(popularity),
    .groups = "drop"
  ) %>%
  filter(Mode_Codes == 1)

mode_track_summary = mode_track_data %>%
  mutate(mode_name = ifelse(mode == 0, "Minor", "Major")) %>%
  group_by(mode_name) %>%
  summarise(
    Unique_Tracks = n(),
    Mean_Popularity = mean(Track_Popularity),
    Median_Popularity = median(Track_Popularity),
    .groups = "drop"
  )
print(mode_track_summary)
cat("Ambiguous IDs excluded from the track-level test:",
    n_distinct(objective4_data$track_id) - nrow(mode_track_data), "\n")

# Welch's two-sample test on independent unique-track observations.
mode_test = t.test(Track_Popularity ~ factor(mode),
                   data = mode_track_data)
print(mode_test)

# A tiny difference can be statistically significant with this many rows.
mean_mode_difference = mode_summary$Mean_Popularity[
  mode_summary$mode_name == "Minor"] - mode_summary$Mean_Popularity[
    mode_summary$mode_name == "Major"]
cat("Minor - Major mean popularity difference (all records):",
    round(mean_mode_difference, 3), "points\n")

# Figure 4.1 complements the t-test by showing both groups' spread.
figure_4_1 = ggplot(objective4_data,
                    aes(x = mode_name, y = popularity)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.15) +
  labs(title = "Spotify Popularity by Major and Minor Mode",
       subtitle = "Descriptive distribution across cleaned track records",
       x = "Musical Mode", y = "Popularity Score") +
  theme_minimal(base_size = 12)
print(figure_4_1)
ggsave(file.path(figure_dir, "objective4_figure_4_1_mode.png"),
       figure_4_1, width = 7.2, height = 4.5, dpi = 220)

key_mode_summary = objective4_data %>%
  group_by(key, key_name, mode_name) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    .groups = "drop"
  ) %>%
  arrange(desc(Median_Popularity), desc(Mean_Popularity))
print(head(key_mode_summary, 12))

figure_4_2 = ggplot(
  key_mode_summary,
  aes(x = key_name, y = Median_Popularity, fill = mode_name)
) +
  geom_col(position = "dodge") +
  labs(title = "Median Popularity by Musical Key and Mode",
       x = "Musical Key", y = "Median Popularity", fill = "Mode") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(figure_4_2)
ggsave(file.path(figure_dir, "objective4_figure_4_2_key_mode.png"),
       figure_4_2, width = 8, height = 4.8, dpi = 220)
write.csv(mode_track_summary,
          file.path(table_dir, "unique_track_mode_summary.csv"), row.names = FALSE)
write.csv(data.frame(Statistic = mode_test$statistic,
                     P_Value = mode_test$p.value,
                     Unique_Track_N = nrow(mode_track_data)),
          file.path(table_dir, "unique_track_mode_welch_test.csv"), row.names = FALSE)
write.csv(mode_summary, file.path(table_dir, "mode_summary.csv"), row.names = FALSE)
write.csv(key_mode_summary, file.path(table_dir, "key_mode_summary.csv"), row.names = FALSE)

# ================================================================
# ANALYSIS 4-2: ADDING TIME SIGNATURE
# ================================================================

signature_summary = objective4_data %>%
  count(time_signature, name = "Count") %>%
  mutate(Percentage = round(100 * Count / sum(Count), 2))
print(signature_summary)

key_mode_time_summary = objective4_data %>%
  group_by(key, key_name, mode_name, time_signature) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  ) %>%
  arrange(desc(Median_Popularity), desc(Mean_Popularity))

# Do not highlight profiles based on only a handful of tracks.
reliable_profiles = key_mode_time_summary %>% filter(Count >= 30)
print(head(reliable_profiles, 15))

# E major (Spotify key 4, mode 1) was the prominent key-mode profile.
key4_major = objective4_data %>% filter(key == 4, mode == 1)
key4_major_summary = key4_major %>%
  group_by(time_signature) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  ) %>%
  arrange(time_signature)
print(key4_major_summary)

figure_4_3 = ggplot(key4_major,
                    aes(x = factor(time_signature), y = popularity)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.25) +
  labs(title = "Popularity of E Major Tracks by Time Signature",
       x = "Time Signature", y = "Popularity Score") +
  theme_minimal(base_size = 12)
print(figure_4_3)
ggsave(file.path(figure_dir, "objective4_figure_4_3_e_major.png"),
       figure_4_3, width = 7.2, height = 4.6, dpi = 220)

# Figure 4.4 shows whether the E-major observation is isolated.
# This covers ALL sufficiently populated key/mode/time combinations.
figure_4_4 = ggplot(
  reliable_profiles,
  aes(x = key_name, y = Median_Popularity,
      fill = factor(time_signature))
) +
  geom_col(position = "dodge") +
  facet_wrap(~ mode_name, ncol = 1) +
  labs(title = "Median Popularity Across Key, Mode and Time Signature",
       subtitle = "Profiles with at least 30 records",
       x = "Musical Key", y = "Median Popularity",
       fill = "Time Signature") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(figure_4_4)
ggsave(file.path(figure_dir, "objective4_figure_4_4_full_profiles.png"),
       figure_4_4, width = 10.5, height = 7, dpi = 220)
write.csv(signature_summary, file.path(table_dir, "time_signature_summary.csv"), row.names = FALSE)
write.csv(reliable_profiles, file.path(table_dir, "reliable_profiles.csv"), row.names = FALSE)
write.csv(key4_major_summary, file.path(table_dir, "key4_major_summary.csv"), row.names = FALSE)

# ================================================================
# ANALYSIS 4-3: PREDICTIVE MODELLING AND TESTING
# ================================================================
# Track IDs repeat in this dataset (e.g., tracks occurring in more
# than one genre). A random ROW split could put the same track in
# both sets. Instead, assign each track ID to one set only.
#
# Spotify track IDs are pseudorandom-looking alphanumeric strings.
# This deterministic checksum puts approximately 20% of unique
# track IDs in the test set and is reproducible in base R.
# It is a reproducible partition, not a claim of random sampling.
# IMPORTANT: This uses only the ID, not the target popularity.

track_bucket = function(id) {
  character_codes = utf8ToInt(as.character(id))
  positions = seq_along(character_codes)
  as.integer(sum(character_codes * positions^2) %% 5)
}

prediction_data = objective4_data %>%
  select(track_id, popularity, key, mode, time_signature) %>%
  mutate(
    key = factor(key, levels = 0:11),
    mode = factor(mode, levels = 0:1),
    time_signature = factor(time_signature, levels = c(3, 4, 5))
  )

# Calculate once per unique track to make the split explicit.
track_partition = data.frame(
  track_id = unique(prediction_data$track_id),
  stringsAsFactors = FALSE
)
track_partition$bucket = vapply(track_partition$track_id, track_bucket, integer(1))
prediction_data = prediction_data %>%
  left_join(track_partition, by = "track_id")
training_data = prediction_data %>% filter(bucket != 0)
testing_data = prediction_data %>% filter(bucket == 0)

stopifnot(
  nrow(training_data) > 0,
  nrow(testing_data) > 0,
  length(intersect(unique(training_data$track_id),
                   unique(testing_data$track_id))) == 0
)
cat("\nUnique tracks train/test:",
    n_distinct(training_data$track_id), n_distinct(testing_data$track_id), "\n")
cat("Rows train/test:", nrow(training_data), nrow(testing_data), "\n")

# All 72 key x mode x meter combinations appear in the training
# data, so predictions from the interaction model are estimable.
training_profiles = training_data %>% count(key, mode, time_signature)
testing_profiles = testing_data %>% count(key, mode, time_signature)
stopifnot(nrow(training_profiles) == 72,
          nrow(anti_join(testing_profiles, training_profiles,
                         by = c("key", "mode", "time_signature"))) == 0)

# Constant baseline: predict the mean of TRAINING popularity only.
training_mean = mean(training_data$popularity)
baseline_predictions = rep(training_mean, nrow(testing_data))

# Main-effects model: separate categorical effects of all 3 variables.
model_1 = lm(popularity ~ key + mode + time_signature,
             data = training_data)

# Interaction model: additionally allows the effects to interact.
model_2 = lm(popularity ~ key * mode * time_signature,
             data = training_data)

# Estimate popularity for the same untouched test set.
main_predictions = as.numeric(predict(model_1, newdata = testing_data))
interaction_predictions = as.numeric(predict(model_2, newdata = testing_data))

# One consistent evaluation function for ALL models.
score_predictions = function(actual, predicted) {
  errors = actual - predicted
  test_mean = mean(actual)
  test_total_sum_squares = sum((actual - test_mean)^2)
  data.frame(
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    Test_R2 = 1 - sum(errors^2) / test_total_sum_squares
  )
}

baseline_scores = score_predictions(testing_data$popularity,
                                    baseline_predictions)
main_scores = score_predictions(testing_data$popularity,
                                main_predictions)
interaction_scores = score_predictions(testing_data$popularity,
                                       interaction_predictions)
model_comparison = bind_rows(
  cbind(Model = "Training-mean baseline", baseline_scores),
  cbind(Model = "Main-effects regression", main_scores),
  cbind(Model = "Interaction regression", interaction_scores)
)
print(model_comparison)

prediction_results = testing_data %>%
  select(track_id, popularity, key, mode, time_signature) %>%
  mutate(
    Baseline = baseline_predictions,
    Main_Effects = main_predictions,
    Interaction = interaction_predictions,
    Interaction_Error = popularity - Interaction
  )

# Dense full-data scatterplots hide patterns: use a plotted subset
# ONLY for display. Model fitting/evaluation always uses all rows.
set.seed(25)
plot_rows = sample(seq_len(nrow(prediction_results)),
                   size = min(3500, nrow(prediction_results)))
figure_4_5 = ggplot(prediction_results[plot_rows, ],
                    aes(x = popularity, y = Interaction)) +
  geom_point(alpha = 0.17, size = 0.65) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Actual vs Predicted Popularity: Interaction Regression",
       subtitle = "Display sample only; metrics use the full test set",
       x = "Actual Popularity", y = "Predicted Popularity") +
  theme_minimal(base_size = 12)
print(figure_4_5)
ggsave(file.path(figure_dir, "objective4_figure_4_5_actual_predicted.png"),
       figure_4_5, width = 7.3, height = 5.5, dpi = 220)

figure_4_6 = ggplot(prediction_results,
                    aes(x = Interaction_Error)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Interaction Regression: Prediction Errors",
       x = "Actual Minus Predicted Popularity", y = "Track Records") +
  theme_minimal(base_size = 12)
print(figure_4_6)
ggsave(file.path(figure_dir, "objective4_figure_4_6_errors.png"),
       figure_4_6, width = 7.2, height = 4.5, dpi = 220)

write.csv(model_comparison, file.path(table_dir, "model_comparison.csv"), row.names = FALSE)
write.csv(prediction_results, file.path(table_dir, "test_predictions.csv"), row.names = FALSE)
cat("\nAll Objective 4 analyses and primary prediction outputs completed.\n")

# ================================================================
# ROBUSTNESS CHECK: COMPLETE CASES FROM THE ORIGINAL RAW FILE
# ================================================================
# Limitation: the SHARED cleaned file was prepared before this
# train/test split. If global missing-value imputation used the
# entire dataset, that can introduce minor preprocessing leakage.
# The independent raw complete-case sensitivity check below
# avoids using globally imputed Objective 4 predictor values.
# It tests whether the same weak-prediction conclusion persists;
# it is not a replacement for the group's shared clean dataset.

if (!file.exists(raw_file)) {
  stop("Raw file missing: required for the Objective 4 robustness check.")
}
raw_data = read.csv(raw_file,
                    na.strings = c("", "NA", "N/A", "NULL"),
                    stringsAsFactors = FALSE)
# Drop only the artificial CSV index, not legitimate analysis data.
raw_data = raw_data[, !(names(raw_data) %in%
                          c("X", "Unnamed..0", "Unnamed: 0")), drop = FALSE]
raw_data = distinct(raw_data)

raw_complete = raw_data %>%
  select(all_of(required_columns)) %>%
  filter(
    !is.na(track_id), !is.na(popularity), !is.na(key),
    !is.na(mode), !is.na(time_signature),
    nzchar(track_id),
    popularity >= 0 & popularity <= 100,
    key %in% 0:11,
    mode %in% 0:1,
    time_signature %in% c(3, 4, 5)
  ) %>%
  mutate(
    key = factor(as.integer(key), levels = 0:11),
    mode = factor(as.integer(mode), levels = 0:1),
    time_signature = factor(as.integer(time_signature), levels = c(3, 4, 5))
  ) %>%
  left_join(track_partition, by = "track_id")

raw_training = raw_complete %>% filter(bucket != 0)
raw_testing = raw_complete %>% filter(bucket == 0)
stopifnot(!anyNA(raw_complete$bucket),
          length(intersect(unique(raw_training$track_id),
                           unique(raw_testing$track_id))) == 0)

raw_baseline = rep(mean(raw_training$popularity), nrow(raw_testing))
raw_main = lm(popularity ~ key + mode + time_signature,
              data = raw_training)
raw_interaction = lm(popularity ~ key * mode * time_signature,
                     data = raw_training)
robustness_comparison = bind_rows(
  cbind(Model = "Training-mean baseline",
        score_predictions(raw_testing$popularity, raw_baseline)),
  cbind(Model = "Main-effects regression",
        score_predictions(raw_testing$popularity,
                          predict(raw_main, newdata = raw_testing))),
  cbind(Model = "Interaction regression",
        score_predictions(raw_testing$popularity,
                          predict(raw_interaction, newdata = raw_testing)))
)
cat("\nRaw complete-case sensitivity rows:", nrow(raw_complete), "\n")
print(robustness_comparison)
write.csv(robustness_comparison,
          file.path(table_dir, "robustness_comparison.csv"), row.names = FALSE)
# EXTRA FEATURE PLOT: compare original-data and cleaned-data model errors.
# Run after both model_comparison and robustness_comparison exist.
extra_plot_data = bind_rows(
  mutate(model_comparison, Dataset = "Cleaned data"),
  mutate(robustness_comparison, Dataset = "Original valid data")
) %>%
  mutate(Model = factor(Model, levels = c(
    "Interaction regression", "Main-effects regression", "Training-mean baseline"
  )))

extra_figure = ggplot(extra_plot_data,
                      aes(x = MAE, y = Model, color = Dataset)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_text(aes(label = sprintf("%.3f", MAE)),
            position = position_dodge(width = 0.4),
            hjust = -0.25, size = 3, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.3))) +
  labs(title = "Extra feature: Raw-data robustness check",
       subtitle = "Similar prediction errors on two versions of the data",
       x = "Test MAE (lower is better)", y = NULL, color = "Dataset") +
  theme_minimal(base_size = 12)
print(extra_figure)
ggsave(file.path(figure_dir, "objective4_extra_feature_robustness.png"),
       extra_figure, width = 9, height = 4.6, dpi = 220)

cat("\nOBJECTIVE 4 FINISHED: Rerun from repository root if inputs change.\n")
# Integration-only assertions: the final outputs must exist.
stopifnot(nrow(objective4_data) == 113305,
  nrow(training_profiles) == 72,
  nrow(model_comparison) == 3, nrow(robustness_comparison) == 3,
  all(is.finite(model_comparison$RMSE)),
  all(is.finite(robustness_comparison$RMSE)),
  file.exists(file.path(figure_dir, "objective4_extra_feature_robustness.png")))
})

if (.group25_plot_device) {
  dev.off()
  cat("All objective plots saved:", .group25_plot_path, "\n")
}
cat("\nGROUP 25 COMPLETE. Audited, cleaned and explored data; all four objectives executed.\n")
