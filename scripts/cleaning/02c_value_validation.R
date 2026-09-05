# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02c_value_validation.R
# Responsible student:
# Name:
# TP Number:

# Purpose:
# To validate numerical and categorical values, identify
# confirmed invalid values and replace them with NA without
# removing complete records.
# ============================================================


#-----------------------------------------------------
#Check the dataset before value validation

nrow(clean_data)
ncol(clean_data)
sum(is.na(clean_data))

missing_before_validation = sum(is.na(clean_data))

#The dataset is passed from Stage 02b
#Existing missing values are recorded before invalid values are changed


#-----------------------------------------------------
#Validate popularity

sum(
  !is.na(clean_data$popularity) &
    (clean_data$popularity < 0 |
       clean_data$popularity > 100)
)

invalid_popularity =
  !is.na(clean_data$popularity) &
  (clean_data$popularity < 0 |
     clean_data$popularity > 100)

#Popularity values must remain between 0 and 100
#Values outside this range are treated as invalid

clean_data[
  invalid_popularity,
  c("track_id", "track_name", "popularity")
]

clean_data$popularity[invalid_popularity] = NA


#-----------------------------------------------------
#Validate duration_ms

sum(
  !is.na(clean_data$duration_ms) &
    clean_data$duration_ms <= 0
)

invalid_duration =
  !is.na(clean_data$duration_ms) &
  clean_data$duration_ms <= 0

#duration_ms represents the length of a track in milliseconds
#Zero and negative duration values are treated as invalid

clean_data[
  invalid_duration,
  c("track_id", "track_name", "duration_ms")
]

clean_data$duration_ms[invalid_duration] = NA


#-----------------------------------------------------
#Validate danceability

sum(
  !is.na(clean_data$danceability) &
    (clean_data$danceability < 0 |
       clean_data$danceability > 1)
)

invalid_danceability =
  !is.na(clean_data$danceability) &
  (clean_data$danceability < 0 |
     clean_data$danceability > 1)

#Danceability values are expected to remain between 0 and 1

clean_data$danceability[invalid_danceability] = NA


#-----------------------------------------------------
#Validate energy

sum(
  !is.na(clean_data$energy) &
    (clean_data$energy < 0 |
       clean_data$energy > 1)
)

invalid_energy =
  !is.na(clean_data$energy) &
  (clean_data$energy < 0 |
     clean_data$energy > 1)

#Energy values are expected to remain between 0 and 1

clean_data$energy[invalid_energy] = NA


#-----------------------------------------------------
#Validate speechiness

sum(
  !is.na(clean_data$speechiness) &
    (clean_data$speechiness < 0 |
       clean_data$speechiness > 1)
)

invalid_speechiness =
  !is.na(clean_data$speechiness) &
  (clean_data$speechiness < 0 |
     clean_data$speechiness > 1)

#Speechiness values are expected to remain between 0 and 1

clean_data$speechiness[invalid_speechiness] = NA


#-----------------------------------------------------
#Validate acousticness

sum(
  !is.na(clean_data$acousticness) &
    (clean_data$acousticness < 0 |
       clean_data$acousticness > 1)
)

invalid_acousticness =
  !is.na(clean_data$acousticness) &
  (clean_data$acousticness < 0 |
     clean_data$acousticness > 1)

#Acousticness values are expected to remain between 0 and 1

clean_data$acousticness[invalid_acousticness] = NA


#-----------------------------------------------------
#Validate instrumentalness

sum(
  !is.na(clean_data$instrumentalness) &
    (clean_data$instrumentalness < 0 |
       clean_data$instrumentalness > 1)
)

invalid_instrumentalness =
  !is.na(clean_data$instrumentalness) &
  (clean_data$instrumentalness < 0 |
     clean_data$instrumentalness > 1)

#Instrumentalness values are expected to remain between 0 and 1

clean_data$instrumentalness[invalid_instrumentalness] = NA


#-----------------------------------------------------
#Validate liveness

sum(
  !is.na(clean_data$liveness) &
    (clean_data$liveness < 0 |
       clean_data$liveness > 1)
)

invalid_liveness =
  !is.na(clean_data$liveness) &
  (clean_data$liveness < 0 |
     clean_data$liveness > 1)

#Liveness values are expected to remain between 0 and 1

clean_data$liveness[invalid_liveness] = NA


#-----------------------------------------------------
#Validate valence

sum(
  !is.na(clean_data$valence) &
    (clean_data$valence < 0 |
       clean_data$valence > 1)
)

invalid_valence =
  !is.na(clean_data$valence) &
  (clean_data$valence < 0 |
     clean_data$valence > 1)

#Valence values are expected to remain between 0 and 1

clean_data$valence[invalid_valence] = NA


#-----------------------------------------------------
#Validate key

sort(unique(clean_data$key))

sum(
  !is.na(clean_data$key) &
    !(clean_data$key %in% c(-1, 0:11))
)

invalid_key =
  !is.na(clean_data$key) &
  !(clean_data$key %in% c(-1, 0:11))

#Key uses standard pitch class notation from 0 to 11
#-1 may represent that no musical key was detected
#Other values are treated as invalid

clean_data[
  invalid_key,
  c("track_id", "track_name", "key")
]

clean_data$key[invalid_key] = NA


#-----------------------------------------------------
#Validate mode

sort(unique(clean_data$mode))

sum(
  !is.na(clean_data$mode) &
    !(clean_data$mode %in% c(0, 1))
)

invalid_mode =
  !is.na(clean_data$mode) &
  !(clean_data$mode %in% c(0, 1))

#Mode only supports 0 for minor and 1 for major
#Other values are treated as invalid

clean_data[
  invalid_mode,
  c("track_id", "track_name", "mode")
]

clean_data$mode[invalid_mode] = NA


#-----------------------------------------------------
#Validate time_signature

sort(unique(clean_data$time_signature))

sum(
  !is.na(clean_data$time_signature) &
    !(clean_data$time_signature %in% 3:7)
)

invalid_time_signature =
  !is.na(clean_data$time_signature) &
  !(clean_data$time_signature %in% 3:7)

#Time signature values outside 3 to 7 are treated as invalid

clean_data[
  invalid_time_signature,
  c("track_id", "track_name", "time_signature")
]

clean_data$time_signature[invalid_time_signature] = NA


#-----------------------------------------------------
#Validate tempo

sum(
  !is.na(clean_data$tempo) &
    clean_data$tempo < 0
)

invalid_tempo =
  !is.na(clean_data$tempo) &
  clean_data$tempo < 0

#Negative tempo values are invalid because tempo represents
#beats per minute

clean_data[
  invalid_tempo,
  c("track_id", "track_name", "tempo")
]

clean_data$tempo[invalid_tempo] = NA


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
    (clean_data$loudness < -60 |
       clean_data$loudness > 0)
)

#Loudness values outside -60 to 0 dB are recorded for investigation
#They are not automatically treated as invalid because this is
#a typical range rather than a strict valid range


#-----------------------------------------------------
#Create invalid value summary

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


#-----------------------------------------------------
#Check missing values after value validation

missing_after_validation = sum(is.na(clean_data))

missing_before_validation
missing_after_validation

new_missing_from_validation =
  missing_after_validation -
  missing_before_validation

new_missing_from_validation

#The difference represents the number of confirmed invalid
#values that were changed to NA during Stage 02c


#-----------------------------------------------------
#Final check

nrow(clean_data)
ncol(clean_data)
colSums(is.na(clean_data))

#No complete records are removed during Stage 02c
#The resulting missing values will be handled during Stage 02d