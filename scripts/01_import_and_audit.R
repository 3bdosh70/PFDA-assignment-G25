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
raw_data = read.csv("data/raw/spotify_tracks_data.csv")
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
#the columns appear to be 21, 20 appears to be matching the columns names provided by the file "dataset_description.txt" while column "X" is additional.

str(raw_data)
#we have compared the columns data types with the file "dataset_description.txt" and found the bloew columns data types to be in a wrong format
#explicit - danceability - energy - loudness - speechiness - acousticness - instrumentalness - liveness - valence - tempo

#-----------------------------------------------------

#Finding the missing values

colSums(is.na(raw_data))
sort(colSums(is.na(raw_data)), decreasing = TRUE)
sum(is.na(raw_data))
#View(colSums(is.na(raw_data)))

raw_data = read.csv(
  "data/raw/spotify_tracks_data.csv",
  na.strings = c("", "NA", "N/A", "NULL")
)
#we sort all missing values is null for the program so it's all identifed under the same concept

(colSums(is.na(raw_data)) / nrow(raw_data)) * 100
# Missing percentage for every column

sum(rowSums(is.na(raw_data)) > 0)
# Number of rows containing at least one missing value
# The dataset contains 2,758 missing values across 14 columns.
# popularity - 193
# duration_ms - 191
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

#-----------------------------------------------------

#Finding the duplicated columns

sum(duplicated(raw_data))
# The result is 0 because the X column contains a unique index for every row.
# Therefore, no two complete rows are identical when X is included.

duplicate_test = raw_data
duplicate_test$X = NULL
# A temporary copy is created so the X index can be removed
# without changing the raw data, as X may hide exact duplicates.

sum(duplicated(duplicate_test))
#total of duuplicates excluding "X" column

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
#View(repeated_track_ids)
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
