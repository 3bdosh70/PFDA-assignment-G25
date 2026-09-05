# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 02b_duplicates_and_genres.R
# Responsible student:
# Name:
# TP Number:

# Purpose:
# To remove exact duplicate records and standardize
# inconsistent genre formatting without removing meaningful
# repeated track records.
# ============================================================

library(dplyr)

#-----------------------------------------------------
#Finding exact duplicated records

sum(duplicated(clean_data))
#There are 396 redundant duplicate records before removing duplicates.

nrow(clean_data)
#There are 113,? records before removing duplicates.


duplicate_rows = clean_data[duplicated(clean_data), ]
#View(duplicate_rows)


rows_before_duplicates = nrow(clean_data)

clean_data = clean_data %>% distinct()

rows_after_duplicates = nrow(clean_data)

rows_removed =
  rows_before_duplicates - rows_after_duplicates

rows_removed
#396 exact duplicate records were removed.

nrow(clean_data)
#113604 records remain after removing the exact duplicates.

sum(duplicated(clean_data))
#The result is 0, confirming that no exact duplicate records remain.

#-----------------------------------------------------
#Checking genre values before standardization

length(unique(clean_data$track_genre))
#There are 141 genre categories before standardization.

sort(unique(clean_data$track_genre))
#View(sort(unique(clean_data$track_genre)))

sort(table(clean_data$track_genre), decreasing = TRUE)
#Frequency count of each genre before standardization.

#Several genres represent the same category but use different
#hyphen or spacing formats, for example alt-rock and altrock.

#-----------------------------------------------------
#Standardizing genre formatting

clean_data$track_genre =
  gsub("\\s+", " ", tolower(trimws(clean_data$track_genre)))

#The genre values are converted to lowercase and unnecessary
#spaces are removed to keep the formatting consistent.


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

#-----------------------------------------------------
#Checking genre values after standardization

length(unique(clean_data$track_genre))
#The number of genre categories decreased from 141 to 114
#after the formatting variants were combined.

sort(unique(clean_data$track_genre))
#View(sort(unique(clean_data$track_genre)))

sort(table(clean_data$track_genre), decreasing = TRUE)
#Frequency count of each genre after standardization.

#-----------------------------------------------------
#Checking duplicates created after genre standardization

sum(duplicated(clean_data))
#25 duplicate records are found after genre standardization.
#These records became identical after different formatting
#variants of the same genre were combined.

genre_duplicate_rows =
  clean_data[duplicated(clean_data), ]

#View(genre_duplicate_rows)


rows_before_genre_duplicates = nrow(clean_data)

clean_data = clean_data %>% distinct()

rows_after_genre_duplicates = nrow(clean_data)

genre_duplicates_removed =
  rows_before_genre_duplicates -
  rows_after_genre_duplicates

genre_duplicates_removed
#25 additional duplicate records were removed.


sum(duplicated(clean_data))
#The result is 0, confirming that no exact duplicates remain.

nrow(clean_data)
#113579 records remain after duplicate and genre cleaning.

#-----------------------------------------------------
#Checking repeated track IDs after duplicate cleaning

track_id_number = table(clean_data$track_id)

repeated_track_ids =
  track_id_number[track_id_number > 1]

length(repeated_track_ids)
#Repeated track IDs still remain because repeated IDs are not
#automatically duplicates and may contain meaningful different records.


repeated_track_rows =
  clean_data[
    clean_data$track_id %in% names(repeated_track_ids),
  ]

nrow(repeated_track_rows)

#The remaining repeated track IDs are preserved because only
#records that became completely identical were removed.

#-----------------------------------------------------
#Stage 02b Summary

cat("\n--- DUPLICATE AND GENRE CLEANING SUMMARY ---\n")
cat("Exact duplicates removed:", rows_removed, "\n")
cat(
  "Genre categories after standardization:",
  length(unique(clean_data$track_genre)),
  "\n"
)
cat(
  "Duplicates removed after genre standardization:",
  genre_duplicates_removed,
  "\n"
)
cat("Final rows after Stage 02b:", nrow(clean_data), "\n")

cat("\nStage 02b completed: duplicates and genre formatting cleaned.\n")