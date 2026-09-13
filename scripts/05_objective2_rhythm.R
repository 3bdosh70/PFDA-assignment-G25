library(dplyr)
library(ggplot2)


#=====================================================
# OBJECTIVE 2
# Investigate the Relationship Between
# Track Duration, Genre, Explicit Status and Popularity
#=====================================================



#=====================================================
# ANALYSIS 2-1
# Track Duration, Explicit Status and Popularity
#=====================================================


#-----------------------------------------------------
# STEP 1
# Create Track Duration Groups
#-----------------------------------------------------

exploration_data$duration_group <- cut(
  exploration_data$duration_min,
  breaks = c(-Inf, 2, 3, 4, 5, 6.5, Inf),
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


# Check distribution of duration groups

table(exploration_data$duration_group)



#-----------------------------------------------------
# STEP 2
# Compare Popularity Between Duration Groups
#-----------------------------------------------------

duration_popularity <- exploration_data %>%
  group_by(duration_group) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_popularity))


duration_popularity



#-----------------------------------------------------
# STEP 3
# Visualise Popularity Distribution by Duration
#-----------------------------------------------------

ggplot(
  exploration_data,
  aes(
    x = duration_group,
    y = popularity
  )
) +
  geom_boxplot() +
  labs(
    title = "Popularity Distribution by Track Duration Range",
    x = "Track Duration Range",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



#-----------------------------------------------------
# STEP 4
# Compare Duration and Explicit Status
#-----------------------------------------------------

duration_explicit_popularity <- exploration_data %>%
  group_by(
    duration_group,
    explicit
  ) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  )


duration_explicit_popularity



#-----------------------------------------------------
# STEP 5
# Visualise Explicit Status Difference
#-----------------------------------------------------

ggplot(
  duration_explicit_popularity,
  aes(
    x = duration_group,
    y = median_popularity,
    group = explicit,
    linetype = explicit
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Median Popularity by Track Duration and Explicit Status",
    x = "Track Duration Range",
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






#=====================================================
# ANALYSIS 2-2
# Genre and Duration Relationship with Popularity
#=====================================================


#-----------------------------------------------------
# STEP 1
# Filter Tracks Within Common Commercial Duration
# (3-5 Minutes)
#-----------------------------------------------------

genre_duration_data <- exploration_data %>%
  filter(
    duration_group %in% c(
      "Typical (3-4 min)",
      "Moderately long (4-5 min)"
    )
  )


# Number of tracks used after filtering

nrow(genre_duration_data)



#-----------------------------------------------------
# STEP 2
# Calculate Popularity by Genre
#-----------------------------------------------------

genre_3_5_popularity <- genre_duration_data %>%
  group_by(track_genre) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(median_popularity)
  )


genre_3_5_popularity



#-----------------------------------------------------
# STEP 3
# Select Top 10 Genres
#-----------------------------------------------------

top_10_genres <- genre_3_5_popularity %>%
  slice_head(n = 10)


top_10_genres



#-----------------------------------------------------
# STEP 4
# Compare Genre and Duration
#-----------------------------------------------------

genre_duration_summary <- genre_duration_data %>%
  filter(
    track_genre %in% top_10_genres$track_genre
  ) %>%
  group_by(
    track_genre,
    duration_group
  ) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  )


genre_duration_summary



#-----------------------------------------------------
# STEP 5
# Heatmap: Genre + Duration + Popularity
#-----------------------------------------------------

ggplot(
  genre_duration_summary,
  aes(
    x = duration_group,
    y = reorder(track_genre, median_popularity),
    fill = median_popularity
  )
) +
  geom_tile() +
  labs(
    title = "Median Popularity by Genre and Track Duration",
    subtitle = "Analysis limited to 3-5 minute tracks",
    x = "Track Duration Range",
    y = "Track Genre",
    fill = "Median Popularity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )







#=====================================================
# ANALYSIS 2-3
# Profile of High-Popularity Tracks
#=====================================================


#-----------------------------------------------------
# STEP 1
# Define High Popularity Tracks
# Using Top 25% of Popularity
#-----------------------------------------------------

popularity_cutoff <- quantile(
  exploration_data$popularity,
  0.75,
  na.rm = TRUE
)


popularity_cutoff



# Select high popularity tracks

high_popularity_tracks <- exploration_data %>%
  filter(
    popularity >= popularity_cutoff
  )


nrow(high_popularity_tracks)



#-----------------------------------------------------
# STEP 2
# Create Combined Profiles
# Genre + Duration + Explicit Status
#-----------------------------------------------------

profile_popularity <- exploration_data %>%
  group_by(
    track_genre,
    duration_group,
    explicit
  ) %>%
  summarise(
    total_tracks = n(),
    
    high_popularity_tracks =
      sum(
        popularity >= popularity_cutoff,
        na.rm = TRUE
      ),
    
    high_popularity_rate =
      (high_popularity_tracks / total_tracks) * 100,
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(high_popularity_rate)
  )


profile_popularity



#-----------------------------------------------------
# STEP 3
# Select Leading Profiles
#-----------------------------------------------------

top_profiles <- profile_popularity %>%
  slice_head(n = 10) %>%
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


top_profiles



#-----------------------------------------------------
# STEP 4
# Visualise High Popularity Profiles
#-----------------------------------------------------

ggplot(
  top_profiles,
  aes(
    x = reorder(profile, high_popularity_rate),
    y = high_popularity_rate
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "High-Popularity Rate of Leading Track Profiles",
    x = "Genre, Duration and Explicit Status",
    y = "High-Popularity Tracks (%)"
  ) +
  theme_minimal()