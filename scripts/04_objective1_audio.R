# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 04_objective1_audio.R
# Responsible student:
# Name:
# TP Number:

# Objective 1:
# To investigate whether audio intensity and acoustic
# characteristics are associated with song popularity.

# Main variables:
# popularity, energy, loudness, acousticness,
# instrumentalness

# Purpose:
# To analyse the distributions, correlations and combined
# influence of audio intensity and acoustic characteristics
# on Spotify track popularity.
# ============================================================
# ============================================================
# 3.1 OBJECTIVE 1
# AUDIO INTENSITY, EMOTION AND TEMPO
# ============================================================

# Objective:
# To examine how energy, loudness, valence and tempo
# are associated with Spotify track popularity.


# ============================================================
# OBJECTIVE 1 DATA
# ============================================================

objective1_data = exploration_data %>%
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
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find Energy and Loudness ranges
# ------------------------------------------------------------

energy_breaks = quantile(
  objective1_data$energy,
  probs = c(0, 1/3, 2/3, 1)
)

energy_breaks


loudness_breaks = quantile(
  objective1_data$loudness,
  probs = c(0, 1/3, 2/3, 1)
)

loudness_breaks



# ------------------------------------------------------------
# STEP 2
# Create Energy and Loudness groups
# ------------------------------------------------------------

objective1_data = objective1_data %>%
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

objective1_data$energy_level = factor(
  objective1_data$energy_level,
  levels = c("Low", "Medium", "High")
)

objective1_data$loudness_level = factor(
  objective1_data$loudness_level,
  levels = c("Low", "Medium", "High")
)


table(objective1_data$energy_level)
table(objective1_data$loudness_level)



# ------------------------------------------------------------
# STEP 3
# Summarise Energy + Loudness profiles
# ------------------------------------------------------------

intensity_profile = objective1_data %>%
  group_by(energy_level, loudness_level) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity),
    median_popularity = median(popularity),
    .groups = "drop"
  )

intensity_profile



# ------------------------------------------------------------
# STEP 4
# Visualise Energy + Loudness profiles
# ------------------------------------------------------------

ggplot(
  intensity_profile,
  aes(
    x = energy_level,
    y = median_popularity,
    fill = loudness_level
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Median Popularity by Energy and Loudness",
    x = "Energy Level",
    y = "Median Popularity",
    fill = "Loudness Level"
  ) +
  theme_minimal()



# ============================================================
# ANALYSIS 1-2
# HIGHEST-MEDIAN INTENSITY PROFILE
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find profile with highest median popularity
# ------------------------------------------------------------

highest_intensity_median = max(
  intensity_profile$median_popularity
)

highest_intensity_median


best_intensity = intensity_profile[
  intensity_profile$median_popularity ==
    highest_intensity_median,
]

best_intensity



# ------------------------------------------------------------
# STEP 2
# Save the profile
# ------------------------------------------------------------

best_energy = best_intensity$energy_level[1]
best_loudness = best_intensity$loudness_level[1]

best_energy
best_loudness



# ------------------------------------------------------------
# STEP 3
# Compare highest-median profile with other profiles
# ------------------------------------------------------------

objective1_data = objective1_data %>%
  mutate(
    intensity_group = case_when(
      
      energy_level == best_energy &
        loudness_level == best_loudness
      ~ "Highest-Median Profile",
      
      TRUE
      ~ "Other Profiles"
    )
  )


intensity_comparison = objective1_data %>%
  group_by(intensity_group) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity),
    median_popularity = median(popularity),
    .groups = "drop"
  )

intensity_comparison



# ------------------------------------------------------------
# STEP 4
# Visualise profile comparison
# ------------------------------------------------------------

ggplot(
  objective1_data,
  aes(
    x = intensity_group,
    y = popularity
  )
) +
  geom_boxplot() +
  labs(
    title = "Popularity of the Highest-Median Intensity Profile",
    x = "Intensity Profile",
    y = "Popularity"
  ) +
  theme_minimal()



# ============================================================
# ANALYSIS 1-3
# VALENCE + TEMPO AND POPULARITY
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find Valence and Tempo ranges
# ------------------------------------------------------------

valence_breaks = quantile(
  objective1_data$valence,
  probs = c(0, 1/3, 2/3, 1)
)

valence_breaks


tempo_breaks = quantile(
  objective1_data$tempo,
  probs = c(0, 1/3, 2/3, 1)
)

tempo_breaks



# ------------------------------------------------------------
# STEP 2
# Create Valence and Tempo groups
# ------------------------------------------------------------

objective1_data = objective1_data %>%
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

objective1_data$valence_level = factor(
  objective1_data$valence_level,
  levels = c("Low", "Medium", "High")
)

objective1_data$tempo_level = factor(
  objective1_data$tempo_level,
  levels = c("Slow", "Medium", "Fast")
)


table(objective1_data$valence_level)
table(objective1_data$tempo_level)



# ------------------------------------------------------------
# STEP 3
# Summarise Valence + Tempo profiles
# ------------------------------------------------------------

mood_tempo_profile = objective1_data %>%
  group_by(valence_level, tempo_level) %>%
  summarise(
    track_count = n(),
    mean_popularity = mean(popularity),
    median_popularity = median(popularity),
    .groups = "drop"
  )

mood_tempo_profile



# ------------------------------------------------------------
# STEP 4
# Visualise Valence + Tempo profiles
# ------------------------------------------------------------

ggplot(
  mood_tempo_profile,
  aes(
    x = valence_level,
    y = median_popularity,
    fill = tempo_level
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Median Popularity by Valence and Tempo",
    x = "Valence Level",
    y = "Median Popularity",
    fill = "Tempo Level"
  ) +
  theme_minimal()



# ============================================================
# ANALYSIS 1-4
# HIGH-POPULARITY AUDIO PROFILES
# ============================================================


# ------------------------------------------------------------
# STEP 1
# Find high-popularity cutoff
# ------------------------------------------------------------

popularity_cutoff = quantile(
  objective1_data$popularity,
  0.75
)

popularity_cutoff



# ------------------------------------------------------------
# STEP 2
# Combine all four Objective 1 variables
# ------------------------------------------------------------

audio_profiles = objective1_data %>%
  group_by(
    energy_level,
    loudness_level,
    valence_level,
    tempo_level
  ) %>%
  summarise(
    total_tracks = n(),
    
    high_popularity_tracks =
      sum(popularity >= popularity_cutoff),
    
    high_popularity_rate =
      (high_popularity_tracks / total_tracks) * 100,
    
    median_popularity =
      median(popularity),
    
    .groups = "drop"
  )

audio_profiles



# ------------------------------------------------------------
# STEP 3
# Create shorter profile names
# ------------------------------------------------------------

audio_profiles$profile = paste(
  audio_profiles$energy_level, "Energy |",
  audio_profiles$loudness_level, "Loudness |",
  audio_profiles$valence_level, "Valence |",
  audio_profiles$tempo_level, "Tempo"
)



# ------------------------------------------------------------
# STEP 4
# Select Top 10 profiles by number of
# high-popularity tracks
# ------------------------------------------------------------

audio_profiles = audio_profiles[
  order(
    audio_profiles$high_popularity_tracks,
    decreasing = TRUE
  ),
]

top_audio_profiles = head(
  audio_profiles,
  10
)

top_audio_profiles



# ------------------------------------------------------------
# STEP 5
# Visualise Top High-Popularity profiles
# ------------------------------------------------------------

ggplot(
  top_audio_profiles,
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
    title = "Top Audio Profiles Among High-Popularity Tracks",
    x = "Audio Profile",
    y = "Number of High-Popularity Tracks"
  ) +
  theme_minimal()



# ============================================================
# OBJECTIVE 1 SUMMARY OUTPUT
# ============================================================

intensity_profile

best_intensity

intensity_comparison

mood_tempo_profile

top_audio_profiles


# ============================================================
# OBJECTIVE 1 COMPLETE
# ============================================================

