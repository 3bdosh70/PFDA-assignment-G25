
library(dplyr)
library(ggplot2)


#-----------------------------------------------------
# Load the cleaned dataset

exploration_data = read.csv(
  "data/processed/spotify_tracks_clean.csv",
  stringsAsFactors = FALSE
)


#-----------------------------------------------------
# Check the cleaned dataset

nrow(exploration_data)
ncol(exploration_data)

names(exploration_data)

str(exploration_data)

head(exploration_data)

sum(is.na(exploration_data))
sum(duplicated(exploration_data))

# Expected:
# 113305 rows
# 20 columns
# 0 missing values
# 0 exact duplicate records


#-----------------------------------------------------
# General dataset profile

total_records = nrow(exploration_data)

unique_tracks =
  length(unique(exploration_data$track_id))

unique_artist_entries =
  length(unique(exploration_data$artists))

unique_albums =
  length(unique(exploration_data$album_name))

unique_genres =
  length(unique(exploration_data$track_genre))


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
# Check repeated track IDs

track_id_counts =
  table(exploration_data$track_id)

repeated_track_ids =
  track_id_counts[track_id_counts > 1]

number_repeated_track_ids =
  length(repeated_track_ids)

rows_with_repeated_track_ids =
  sum(
    exploration_data$track_id %in%
      names(repeated_track_ids)
  )


number_repeated_track_ids
rows_with_repeated_track_ids

# Repeated track IDs are recorded as a dataset characteristic.
# They are not removed during general exploration because the
# same track may appear in different genre records.


#-----------------------------------------------------
# General summary of all variables

summary(exploration_data)


#-----------------------------------------------------
# Numerical variables used in general exploration

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
# Numerical summary statistics

numeric_summary = data.frame(
  
  Variable = numeric_variables,
  
  Minimum = sapply(
    exploration_data[numeric_variables],
    min
  ),
  
  Q1 = sapply(
    exploration_data[numeric_variables],
    function(x) quantile(x, 0.25)
  ),
  
  Median = sapply(
    exploration_data[numeric_variables],
    median
  ),
  
  Mean = sapply(
    exploration_data[numeric_variables],
    mean
  ),
  
  Q3 = sapply(
    exploration_data[numeric_variables],
    function(x) quantile(x, 0.75)
  ),
  
  Maximum = sapply(
    exploration_data[numeric_variables],
    max
  ),
  
  Standard_Deviation = sapply(
    exploration_data[numeric_variables],
    sd
  )
)


numeric_summary


#-----------------------------------------------------
# Popularity summary

summary(exploration_data$popularity)

popularity_mean =
  mean(exploration_data$popularity)

popularity_median =
  median(exploration_data$popularity)

popularity_sd =
  sd(exploration_data$popularity)

popularity_min =
  min(exploration_data$popularity)

popularity_max =
  max(exploration_data$popularity)


popularity_mean
popularity_median
popularity_sd
popularity_min
popularity_max


# Percentage of tracks with popularity = 0

zero_popularity_count =
  sum(exploration_data$popularity == 0)

zero_popularity_percentage =
  zero_popularity_count /
  nrow(exploration_data) * 100


zero_popularity_count
zero_popularity_percentage


#-----------------------------------------------------
# Popularity histogram

ggplot(
  exploration_data,
  aes(x = popularity)
) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Track Popularity",
    x = "Popularity Score",
    y = "Frequency"
  )


#-----------------------------------------------------
# Popularity boxplot

ggplot(
  exploration_data,
  aes(y = popularity)
) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  labs(
    title = "Boxplot of Track Popularity",
    y = "Popularity Score"
  )


#-----------------------------------------------------
# Duration distribution

ggplot(
  exploration_data,
  aes(x = duration_ms)
) +
  geom_histogram(
    binwidth = 30000,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Track Duration",
    x = "Duration (Milliseconds)",
    y = "Frequency"
  )


#-----------------------------------------------------
# Danceability distribution

ggplot(
  exploration_data,
  aes(x = danceability)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "coral",
    color = "white"
  ) +
  labs(
    title = "Distribution of Danceability",
    x = "Danceability",
    y = "Frequency"
  )


#-----------------------------------------------------
# Energy distribution

ggplot(
  exploration_data,
  aes(x = energy)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "maroon",
    color = "white"
  ) +
  labs(
    title = "Distribution of Energy",
    x = "Energy",
    y = "Frequency"
  )


#-----------------------------------------------------
# Loudness distribution

ggplot(
  exploration_data,
  aes(x = loudness)
) +
  geom_histogram(
    binwidth = 2,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Loudness",
    x = "Loudness (dB)",
    y = "Frequency"
  )


#-----------------------------------------------------
# Tempo distribution

ggplot(
  exploration_data,
  aes(x = tempo)
) +
  geom_histogram(
    binwidth = 5,
    fill = "coral",
    color = "white"
  ) +
  labs(
    title = "Distribution of Track Tempo",
    x = "Tempo (BPM)",
    y = "Frequency"
  )


#-----------------------------------------------------
# Acousticness distribution

ggplot(
  exploration_data,
  aes(x = acousticness)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "lightblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Acousticness",
    x = "Acousticness",
    y = "Frequency"
  )


#-----------------------------------------------------
# Instrumentalness distribution

ggplot(
  exploration_data,
  aes(x = instrumentalness)
) +
  geom_histogram(
    binwidth = 0.05,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Instrumentalness",
    x = "Instrumentalness",
    y = "Frequency"
  )


#-----------------------------------------------------
# Explicit track distribution

explicit_counts =
  table(exploration_data$explicit)

explicit_counts

prop.table(explicit_counts) * 100


ggplot(
  exploration_data,
  aes(x = explicit)
) +
  geom_bar(
    fill = "coral"
  ) +
  labs(
    title = "Explicit and Non-Explicit Tracks",
    x = "Explicit",
    y = "Number of Records"
  )


#-----------------------------------------------------
# Mode distribution

mode_counts =
  table(exploration_data$mode)

mode_counts

prop.table(mode_counts) * 100


# 0 = Minor
# 1 = Major


#-----------------------------------------------------
# Key distribution

key_counts =
  table(exploration_data$key)

key_counts


ggplot(
  exploration_data,
  aes(x = factor(key))
) +
  geom_bar(
    fill = "steelblue"
  ) +
  labs(
    title = "Distribution of Musical Keys",
    x = "Key",
    y = "Number of Records"
  )


#-----------------------------------------------------
# Time signature distribution

time_signature_counts =
  table(exploration_data$time_signature)

time_signature_counts

prop.table(time_signature_counts) * 100


ggplot(
  exploration_data,
  aes(x = factor(time_signature))
) +
  geom_bar(
    fill = "lightblue"
  ) +
  labs(
    title = "Distribution of Time Signatures",
    x = "Time Signature",
    y = "Number of Records"
  )


#-----------------------------------------------------
# Genre distribution

genre_counts =
  sort(
    table(exploration_data$track_genre),
    decreasing = TRUE
  )

genre_counts

length(genre_counts)
#-----------------------------------------------------
# Basic popularity comparison by explicit status

popularity_by_explicit =
  exploration_data %>%
  group_by(explicit) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity)
  )


popularity_by_explicit


ggplot(
  exploration_data,
  aes(
    x = factor(explicit),
    y = popularity
  )
) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  labs(
    title = "Popularity by Explicit Status",
    x = "Explicit",
    y = "Popularity Score"
  )


#-----------------------------------------------------
# Basic popularity comparison by mode

popularity_by_mode =
  exploration_data %>%
  group_by(mode) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity)
  )


popularity_by_mode


#-----------------------------------------------------
# Basic correlation exploration

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
  exploration_data[correlation_variables]


correlation_matrix =
  cor(correlation_data)


correlation_matrix


#-----------------------------------------------------
# Correlation of each numerical variable with popularity

popularity_correlations = data.frame(
  
  Variable =
    correlation_variables[
      correlation_variables != "popularity"
    ],
  
  Correlation =
    as.numeric(
      correlation_matrix[
        "popularity",
        correlation_variables != "popularity"
      ]
    )
)


popularity_correlations =
  popularity_correlations[
    order(
      abs(popularity_correlations$Correlation),
      decreasing = TRUE
    ),
  ]


popularity_correlations


# The correlation values are used only for general exploration.
# Statistical significance and detailed relationships are handled
# later in the individual objective analyses.


#-----------------------------------------------------
# Create output folders if they do not already exist

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables")
}

if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots")
}


#-----------------------------------------------------
# Export useful exploration tables

write.csv(
  dataset_profile,
  "outputs/tables/general_dataset_profile.csv",
  row.names = FALSE
)


write.csv(
  numeric_summary,
  "outputs/tables/general_numeric_summary.csv",
  row.names = FALSE
)


write.csv(
  top_10_genres_df,
  "outputs/tables/top_10_genres.csv",
  row.names = FALSE
)


write.csv(
  popularity_by_explicit,
  "outputs/tables/popularity_by_explicit.csv",
  row.names = FALSE
)


write.csv(
  popularity_by_mode,
  "outputs/tables/popularity_by_mode.csv",
  row.names = FALSE
)


write.csv(
  popularity_correlations,
  "outputs/tables/popularity_correlations.csv",
  row.names = FALSE
)

#-----------------------------------------------------
# ============================================================
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

ggplot(
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


#-----------------------------------------------------
#Popularity boxplot

ggplot(
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


#-----------------------------------------------------
#Duration distribution

ggplot(
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


#-----------------------------------------------------
#Danceability distribution

ggplot(
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


#-----------------------------------------------------
#Energy distribution

ggplot(
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


#-----------------------------------------------------
#Loudness distribution

ggplot(
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


#-----------------------------------------------------
#Tempo distribution

ggplot(
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


#-----------------------------------------------------
#Acousticness distribution

ggplot(
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


#-----------------------------------------------------
#Instrumentalness distribution

ggplot(
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


ggplot(
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


ggplot(
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


ggplot(
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
#Top 10 genres

top_10_genres =
  head(
    genre_counts,
    10
  )

top_10_genres


top_10_genres_df = data.frame(

  Genre =
    names(top_10_genres),

  Count =
    as.numeric(top_10_genres)
)

top_10_genres_df


ggplot(
  top_10_genres_df,
  aes(
    x = reorder(
      Genre,
      Count
    ),
    y = Count
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Genres by Number of Records",
    x = "Genre",
    y = "Number of Records"
  )


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


ggplot(
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
#Create output folders

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables")
}

if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots")
}


#-----------------------------------------------------
#Export useful exploration tables

write.csv(
  dataset_profile,
  "outputs/tables/general_dataset_profile.csv",
  row.names = FALSE
)

write.csv(
  numeric_summary,
  "outputs/tables/general_numeric_summary.csv",
  row.names = FALSE
)

write.csv(
  top_10_genres_df,
  "outputs/tables/top_10_genres.csv",
  row.names = FALSE
)

write.csv(
  popularity_by_explicit,
  "outputs/tables/popularity_by_explicit.csv",
  row.names = FALSE
)

write.csv(
  popularity_by_mode,
  "outputs/tables/popularity_by_mode.csv",
  row.names = FALSE
)

write.csv(
  popularity_correlations,
  "outputs/tables/popularity_correlations.csv",
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