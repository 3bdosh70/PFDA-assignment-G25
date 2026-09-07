# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02_data_preparation.R
# Responsible students:
# Name:
# TP Number:

# Purpose:
# To import, inspect, clean, validate and transform the
# Spotify dataset before general exploration and analysis.
# ============================================================


library(dplyr)


# ============================================================
# 2.1 DATA IMPORT AND PRELIMINARY EXPLORATION
# ============================================================

original_rows = nrow(raw_data)
original_columns = ncol(raw_data)
original_missing = sum(is.na(raw_data))


#-----------------------------------------------------
#Check missing values

colSums(is.na(raw_data))

sort(
  colSums(is.na(raw_data)),
  decreasing = TRUE
)

sum(is.na(raw_data))

(colSums(is.na(raw_data)) / nrow(raw_data)) * 100

sum(rowSums(is.na(raw_data)) > 0)

#The dataset contains 3553 missing values across 18 columns
#A total of 3485 rows contain at least one missing value
#No missing values are changed during preliminary exploration


#-----------------------------------------------------
#Check duplicated records

sum(duplicated(raw_data))

#The result is 0 because X contains a unique index for every row
#Therefore X may hide duplicated records


duplicate_test = raw_data
duplicate_test$X = NULL

sum(duplicated(duplicate_test))

#There are 396 redundant duplicate occurrences when X is excluded


duplicate_rows =
  duplicate_test[duplicated(duplicate_test), ]

#View(duplicate_rows)


exact_duplicate_flag =
  duplicated(duplicate_test) |
  duplicated(duplicate_test, fromLast = TRUE)

exact_duplicate_records =
  duplicate_test[exact_duplicate_flag, ]

nrow(exact_duplicate_records)

#786 rows belong to exact duplicate groups
#These groups contain 396 redundant duplicate occurrences


#-----------------------------------------------------
#Check repeated track IDs

length(unique(raw_data$track_id))

track_id_number = table(raw_data$track_id)

repeated_track_ids =
  track_id_number[track_id_number > 1]

length(repeated_track_ids)


repeated_track_rows =
  raw_data[
    raw_data$track_id %in% names(repeated_track_ids),
  ]

nrow(repeated_track_rows)

#Repeated track IDs are investigated separately because a repeated
#track ID does not automatically mean that the record is redundant


#-----------------------------------------------------
#Check repeated IDs by genre

id_genre = unique(
  repeated_track_rows[
    c("track_id", "track_genre")
  ]
)

genre_counts =
  table(id_genre$track_id)

multiple_genre_ids =
  genre_counts[genre_counts > 1]

multiple_genre_ids

#This determines whether repeated track IDs are intentionally
#classified under different genres


#-----------------------------------------------------
#Check repeated IDs by popularity

id_popularity = unique(
  repeated_track_rows[
    c("track_id", "popularity")
  ]
)

popularity_counts =
  table(id_popularity$track_id)

different_popularity_ids =
  popularity_counts[
    popularity_counts > 1
  ]

different_popularity_ids


#-----------------------------------------------------
#Check repeated IDs by audio characteristics

audio_columns = c(
  "track_id",
  "duration_ms",
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
  "time_signature"
)

id_audio =
  unique(repeated_track_rows[audio_columns])

audio_counts =
  table(id_audio$track_id)

different_audio_ids =
  audio_counts[audio_counts > 1]

different_audio_ids


#-----------------------------------------------------
#Check categorical values

unique(raw_data$explicit)
unique(raw_data$key)
unique(raw_data$mode)
unique(raw_data$time_signature)

sort(unique(raw_data$track_genre))


table(raw_data$explicit)
table(raw_data$key)
table(raw_data$mode)
table(raw_data$time_signature)

sort(
  table(raw_data$track_genre),
  decreasing = TRUE
)

#Potentially unsupported values and genre formatting differences
#are identified here but are not changed during preliminary exploration


#-----------------------------------------------------
#Check numerical values

summary(raw_data$popularity)
summary(raw_data$duration_ms)

continuous_audio = raw_data[c(
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo"
)]

summary(continuous_audio)

sapply(
  continuous_audio,
  min,
  na.rm = TRUE
)

sapply(
  continuous_audio,
  max,
  na.rm = TRUE
)


#-----------------------------------------------------
#Check suspicious popularity values

sum(
  !is.na(raw_data$popularity) &
    (
      raw_data$popularity < 0 |
        raw_data$popularity > 100
    )
)

sum(
  !is.na(raw_data$popularity) &
    raw_data$popularity < 0
)

sum(
  !is.na(raw_data$popularity) &
    raw_data$popularity > 100
)

#75 popularity values fall outside 0 to 100
#These values are only identified at this point


#-----------------------------------------------------
#Check non-positive duration values

sum(
  !is.na(raw_data$duration_ms) &
    raw_data$duration_ms <= 0
)

sum(
  !is.na(raw_data$duration_ms) &
    raw_data$duration_ms < 0
)

sum(
  !is.na(raw_data$duration_ms) &
    raw_data$duration_ms == 0
)


#-----------------------------------------------------
#Check audio-score ranges

audio_scores = raw_data[c(
  "danceability",
  "energy",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence"
)]

sapply(
  audio_scores,
  function(x)
    sum(
      !is.na(x) &
        (x < 0 | x > 1)
    )
)


#-----------------------------------------------------
#Check tempo

sum(
  !is.na(raw_data$tempo) &
    raw_data$tempo <= 0
)

sum(
  !is.na(raw_data$tempo) &
    raw_data$tempo < 0
)

sum(
  !is.na(raw_data$tempo) &
    raw_data$tempo == 0
)


#-----------------------------------------------------
#Investigate loudness

summary(raw_data$loudness)

sum(
  !is.na(raw_data$loudness) &
    (
      raw_data$loudness < -60 |
        raw_data$loudness > 0
    )
)

#No values are changed during preliminary exploration



# ============================================================
# 2.2 DATA CLEANING AND PREPROCESSING
# ============================================================

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
  clean_data$explicit == "True"

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
  clean_data$tempo < 0

sum(invalid_tempo)

#Negative tempo values are treated as invalid

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

#Zero tempo values are recorded for investigation
#but are not automatically changed to NA


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

#Loudness values outside -60 to 0 dB are recorded for
#investigation but are not automatically treated as invalid


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



# ============================================================
# 2.2 DATA CLEANING AND PREPROCESSING
# MISSING-VALUE TREATMENT
# ============================================================

#The missing-value treatment has not yet been implemented
#in the existing project files.

#02d_missing_values_and_export.R currently contains the planned
#rules only, so no new treatment is invented here.


#-----------------------------------------------------
#Check missing values after validation

missing_before_treatment =
  colSums(is.na(clean_data))

missing_before_treatment

missing_percentage_before_treatment =
  (
    colSums(is.na(clean_data)) /
      nrow(clean_data)
  ) * 100

missing_percentage_before_treatment


#-----------------------------------------------------
#Target-variable rule

#Records with missing popularity should normally be removed
#because popularity is the main target variable and cannot be
#used for analysis or prediction when it is missing.


#-----------------------------------------------------
#Numeric predictor rule

#The group must approve whether each analysis will use:
#
#1. Complete cases
#2. Median imputation
#3. Another justified method
#
#Do not automatically apply one method to all variables.


#-----------------------------------------------------
#Categorical predictor rule

#The group must decide whether missing categories should:
#
#1. Be removed from a specific analysis
#2. Be recorded as an Unknown category
#3. Be handled using another justified method


#-----------------------------------------------------
#Important modelling note

#Any imputation used specifically for prediction modelling
#should later be calculated from the training data only.
#The testing data must not be used to calculate imputation
#values because that would cause data leakage.



# ============================================================
# FINAL DATA PREPARATION VERIFICATION
# ============================================================

#This is the current final state before the missing-value
#treatment decisions in the section above are implemented.

nrow(clean_data)
ncol(clean_data)
str(clean_data)

sum(is.na(clean_data))
sum(duplicated(clean_data))

length(
  unique(clean_data$track_genre)
)


# ============================================================
# FINAL DATA PREPARATION SUMMARY
# ============================================================

cat("\n--- DATA PREPARATION SUMMARY ---\n")

cat(
  "Original rows:",
  original_rows,
  "\n"
)

cat(
  "Current rows:",
  nrow(clean_data),
  "\n"
)

cat(
  "Rows removed so far:",
  original_rows - nrow(clean_data),
  "\n"
)

cat(
  "Original columns:",
  original_columns,
  "\n"
)

cat(
  "Current columns:",
  ncol(clean_data),
  "\n"
)

cat(
  "Original missing values:",
  original_missing,
  "\n"
)

cat(
  "Missing values after validation:",
  sum(is.na(clean_data)),
  "\n"
)

cat(
  "Exact duplicates remaining:",
  sum(duplicated(clean_data)),
  "\n"
)

cat(
  "Genre categories:",
  length(unique(clean_data$track_genre)),
  "\n"
)

cat(
  "\nMissing-value treatment and final export are still pending.\n"
)

sapply(
  clean_data[c(
    "popularity",
    "duration_ms",
    "key",
    "mode",
    "time_signature",
    "explicit"
  )],
  class
)

missing_after_types - missing_before_types
# ============================================================
# FINAL EXPORT
# ============================================================

#Do not export the final cleaned dataset yet.
#The missing-value treatment must be approved and implemented first.

#Planned output:
#data/processed/spotify_tracks_clean.csv