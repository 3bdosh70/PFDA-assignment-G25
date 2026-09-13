library(dplyr)
library(ggplot2)


#=====================================================
# ANALYSIS 2-1
# Track Duration, Explicit Status and Popularity
#=====================================================


#-----------------------------------------------------
# STEP 1
# Create Duration Groups
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

# Check number of tracks in each duration group
table(exploration_data$duration_group)



#-----------------------------------------------------
# STEP 2
# Summarise Popularity by Duration Group
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
# Visualise Popularity by Duration
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
# Add Explicit Status
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
  ) %>%
  arrange(
    desc(median_popularity),
    desc(mean_popularity)
  )

duration_explicit_popularity



#-----------------------------------------------------
# STEP 5
# Visualise Duration and Explicit Status
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
# Genre and Popularity Within the 3-5 Minute Range
#=====================================================


#-----------------------------------------------------
# STEP 1
# Overall Popularity by Genre
#-----------------------------------------------------

genre_popularity <- exploration_data %>%
  group_by(track_genre) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_popularity))

# View top 10 genres overall
head(genre_popularity, 10)



#-----------------------------------------------------
# STEP 2
# Focus on 3-5 Minute Tracks
# Based on the Finding from Analysis 2-1
#-----------------------------------------------------

genre_3_5_popularity <- exploration_data %>%
  filter(
    duration_group %in% c(
      "Typical (3-4 min)",
      "Moderately long (4-5 min)"
    )
  ) %>%
  group_by(track_genre) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity, na.rm = TRUE),
    median_popularity = median(popularity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(median_popularity),
    desc(mean_popularity)
  )

# View highest-popularity genres within 3-5 minutes
head(genre_3_5_popularity, 10)



#-----------------------------------------------------
# STEP 3
# Select Top 10 Genres Within 3-5 Minutes
#-----------------------------------------------------

top_10_genre_3_5 <- genre_3_5_popularity %>%
  slice_head(n = 10)

top_10_genre_3_5



#-----------------------------------------------------
# STEP 4
# Visualise Top Genres Within 3-5 Minutes
#-----------------------------------------------------

ggplot(
  top_10_genre_3_5,
  aes(
    x = reorder(track_genre, median_popularity),
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



#-----------------------------------------------------
# STEP 5
# Compare 3-4 and 4-5 Minute Tracks
# Within the Top Genres
#-----------------------------------------------------

genre_duration_3_5 <- exploration_data %>%
  filter(
    track_genre %in% top_10_genre_3_5$track_genre,
    duration_group %in% c(
      "Typical (3-4 min)",
      "Moderately long (4-5 min)"
    )
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

# Display exact comparison
genre_duration_3_5 %>%
  arrange(
    track_genre,
    duration_group
  ) %>%
  print(
    n = 20,
    width = Inf
  )



#-----------------------------------------------------
# STEP 6
# Visualise Genre and Duration Comparison
#-----------------------------------------------------

ggplot(
  genre_duration_3_5,
  aes(
    x = reorder(track_genre, median_popularity),
    y = median_popularity,
    fill = duration_group
  )
) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  labs(
    title = "Popularity of Top Genres by Track Duration",
    x = "Track Genre",
    y = "Median Popularity",
    fill = "Duration"
  ) +
  theme_minimal()





#=====================================================
# ANALYSIS 2-3
# Profile of High-Popularity Tracks
#=====================================================


#-----------------------------------------------------
# STEP 1
# Identify the High-Popularity Cutoff
#-----------------------------------------------------

popularity_cutoff <- quantile(
  exploration_data$popularity,
  0.75,
  na.rm = TRUE
)

popularity_cutoff


# Tracks with popularity at or above the 75th percentile
high_popularity_tracks <- exploration_data %>%
  filter(
    popularity >= popularity_cutoff
  )

# Check number of high-popularity tracks
nrow(high_popularity_tracks)



#-----------------------------------------------------
# STEP 2
# Calculate Genre + Duration + Explicit Profiles
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
    desc(high_popularity_tracks)
  )

profile_popularity



#-----------------------------------------------------
# STEP 3
# Select Top 10 Profiles by Number of
# High-Popularity Tracks
#-----------------------------------------------------

top_high_popularity_profiles <- profile_popularity %>%
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

top_high_popularity_profiles



#-----------------------------------------------------
# STEP 4
# Visualise Most Common High-Popularity Profiles
#-----------------------------------------------------

ggplot(
  top_high_popularity_profiles,
  aes(
    x = reorder(
      profile,
      high_popularity_tracks
    ),
    y = high_popularity_tracks
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Common Profiles Among High-Popularity Tracks",
    x = "Genre, Duration and Explicit Status",
    y = "Number of High-Popularity Tracks"
  ) +
  theme_minimal()



#-----------------------------------------------------
# STEP 5
# Rank the Same Profiles by High-Popularity Rate
#-----------------------------------------------------

top_profile_rates <- top_high_popularity_profiles %>%
  arrange(
    desc(high_popularity_rate)
  )

top_profile_rates



#-----------------------------------------------------
# STEP 6
# Visualise High-Popularity Rate
#-----------------------------------------------------

ggplot(
  top_profile_rates,
  aes(
    x = reorder(
      profile,
      high_popularity_rate
    ),
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
