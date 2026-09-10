# ============================================================
# OBJECTIVE 3
# Abdulaziz Qaderi TP091283
# ============================================================

# To investigate how danceability, acousticness,
# speechiness, instrumentalness and liveness
# are associated with popularity.


# ============================================================
# ANALYSIS 3-1
# Danceability + Acousticness
# ============================================================

analysis_3_1 = exploration_data %>%
  select(popularity, danceability, acousticness) %>%
  filter(
    !is.na(popularity),
    !is.na(danceability),
    !is.na(acousticness)
  )

nrow(analysis_3_1)
summary(analysis_3_1)


# put both variables into ranges

analysis_3_1 = analysis_3_1 %>%
  mutate(
    
    danceability_group =
      case_when(
        danceability < 0.2 ~ "0.0-0.2",
        danceability >= 0.2 & danceability < 0.4 ~ "0.2-0.4",
        danceability >= 0.4 & danceability < 0.6 ~ "0.4-0.6",
        danceability >= 0.6 & danceability < 0.8 ~ "0.6-0.8",
        danceability >= 0.8 ~ "0.8-1.0"
      ),
    
    acousticness_group =
      case_when(
        acousticness < 0.2 ~ "0.0-0.2",
        acousticness >= 0.2 & acousticness < 0.4 ~ "0.2-0.4",
        acousticness >= 0.4 & acousticness < 0.6 ~ "0.4-0.6",
        acousticness >= 0.6 & acousticness < 0.8 ~ "0.6-0.8",
        acousticness >= 0.8 ~ "0.8-1.0"
      )
  )


# keep the ranges in the correct order

analysis_3_1$danceability_group =
  factor(
    analysis_3_1$danceability_group,
    levels = c(
      "0.0-0.2",
      "0.2-0.4",
      "0.4-0.6",
      "0.6-0.8",
      "0.8-1.0"
    )
  )

analysis_3_1$acousticness_group =
  factor(
    analysis_3_1$acousticness_group,
    levels = c(
      "0.0-0.2",
      "0.2-0.4",
      "0.4-0.6",
      "0.6-0.8",
      "0.8-1.0"
    )
  )


# check how many tracks are in each combination

table(
  analysis_3_1$danceability_group,
  analysis_3_1$acousticness_group
)


# median popularity for each combination

heatmap_3_1 =
  aggregate(
    popularity ~ danceability_group + acousticness_group,
    data = analysis_3_1,
    FUN = median
  )

names(heatmap_3_1)[3] = "Median_Popularity"

heatmap_3_1


# heatmap makes the 25 combinations easier to compare

ggplot(
  heatmap_3_1,
  aes(
    x = acousticness_group,
    y = danceability_group,
    fill = Median_Popularity
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = round(Median_Popularity, 1)
    ),
    size = 4
  ) +
  scale_fill_gradientn(
    colors = c(
      "#0b7d3b",
      "#5cb85c",
      "#f0e68c",
      "#f47c43",
      "#b10026"
    )
  ) +
  labs(
    title = "Median Popularity by Danceability and Acousticness Profile",
    x = "Acousticness Range",
    y = "Danceability Range",
    fill = "Median Popularity"
  ) +
  theme_minimal()


# check a few combinations that stand out

profile_3_1_a = analysis_3_1 %>%
  filter(
    danceability_group == "0.8-1.0",
    acousticness_group == "0.4-0.6"
  )

nrow(profile_3_1_a)
mean(profile_3_1_a$popularity)
median(profile_3_1_a$popularity)


profile_3_1_b = analysis_3_1 %>%
  filter(
    danceability_group == "0.8-1.0",
    acousticness_group == "0.6-0.8"
  )

nrow(profile_3_1_b)
mean(profile_3_1_b$popularity)
median(profile_3_1_b$popularity)


# compare with high danceability and very low acousticness

profile_3_1_c = analysis_3_1 %>%
  filter(
    danceability_group == "0.8-1.0",
    acousticness_group == "0.0-0.2"
  )

nrow(profile_3_1_c)
mean(profile_3_1_c$popularity)
median(profile_3_1_c$popularity)



# ============================================================
# ANALYSIS 3-2
# Speechiness + Instrumentalness
# ============================================================

analysis_3_2 = exploration_data %>%
  select(popularity, speechiness, instrumentalness) %>%
  filter(
    !is.na(popularity),
    !is.na(speechiness),
    !is.na(instrumentalness)
  )

nrow(analysis_3_2)
summary(analysis_3_2)


# make the vocal content groups

analysis_3_2 = analysis_3_2 %>%
  mutate(
    
    speechiness_group =
      case_when(
        speechiness < 0.33 ~ "Music / Low Speech",
        speechiness >= 0.33 & speechiness < 0.66 ~ "Music + Speech",
        speechiness >= 0.66 ~ "Mostly Spoken"
      ),
    
    instrumentalness_group =
      case_when(
        instrumentalness > 0.5 ~ "Likely Instrumental",
        instrumentalness <= 0.5 ~ "Likely Vocal"
      )
  )


analysis_3_2$speechiness_group =
  factor(
    analysis_3_2$speechiness_group,
    levels = c(
      "Music / Low Speech",
      "Music + Speech",
      "Mostly Spoken"
    )
  )


# check the number of tracks in each profile

table(
  analysis_3_2$speechiness_group,
  analysis_3_2$instrumentalness_group
)


# grouped boxplot

ggplot(
  analysis_3_2,
  aes(
    x = speechiness_group,
    y = popularity,
    fill = instrumentalness_group
  )
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Likely Instrumental" = "#F8766D",
      "Likely Vocal" = "#00BFC4"
    )
  ) +
  labs(
    title = "Popularity Distribution by Vocal-Content Profile",
    x = "Speechiness Profile",
    y = "Popularity Score",
    fill = "Instrumentalness Profile"
  ) +
  theme_minimal()


# check the main groups from the graph

low_speech_vocal = analysis_3_2 %>%
  filter(
    speechiness_group == "Music / Low Speech",
    instrumentalness_group == "Likely Vocal"
  )

nrow(low_speech_vocal)
mean(low_speech_vocal$popularity)
median(low_speech_vocal$popularity)


music_speech_vocal = analysis_3_2 %>%
  filter(
    speechiness_group == "Music + Speech",
    instrumentalness_group == "Likely Vocal"
  )

nrow(music_speech_vocal)
mean(music_speech_vocal$popularity)
median(music_speech_vocal$popularity)


mostly_spoken_vocal = analysis_3_2 %>%
  filter(
    speechiness_group == "Mostly Spoken",
    instrumentalness_group == "Likely Vocal"
  )

nrow(mostly_spoken_vocal)
mean(mostly_spoken_vocal$popularity)
median(mostly_spoken_vocal$popularity)


low_speech_instrumental = analysis_3_2 %>%
  filter(
    speechiness_group == "Music / Low Speech",
    instrumentalness_group == "Likely Instrumental"
  )

nrow(low_speech_instrumental)
mean(low_speech_instrumental$popularity)
median(low_speech_instrumental$popularity)


# this one looked unusual, so check the sample size

spoken_instrumental = analysis_3_2 %>%
  filter(
    speechiness_group == "Mostly Spoken",
    instrumentalness_group == "Likely Instrumental"
  )

nrow(spoken_instrumental)
mean(spoken_instrumental$popularity)
median(spoken_instrumental$popularity)



# ============================================================
# ANALYSIS 3-3
# Liveness
# ============================================================

analysis_3_3 = exploration_data %>%
  select(
    popularity,
    danceability,
    acousticness,
    speechiness,
    instrumentalness,
    liveness
  ) %>%
  filter(
    !is.na(popularity),
    !is.na(danceability),
    !is.na(acousticness),
    !is.na(speechiness),
    !is.na(instrumentalness),
    !is.na(liveness)
  )

nrow(analysis_3_3)
summary(analysis_3_3)


# use quartiles for the liveness levels

Q1 = quantile(
  analysis_3_3$liveness,
  0.25
)

Q2 = quantile(
  analysis_3_3$liveness,
  0.50
)

Q3 = quantile(
  analysis_3_3$liveness,
  0.75
)

Q1
Q2
Q3


analysis_3_3 = analysis_3_3 %>%
  mutate(
    
    liveness_group =
      case_when(
        liveness <= Q1 ~ "Low",
        liveness > Q1 & liveness <= Q2 ~ "Moderate-Low",
        liveness > Q2 & liveness <= Q3 ~ "Moderate-High",
        liveness > Q3 ~ "High"
      )
  )


analysis_3_3$liveness_group =
  factor(
    analysis_3_3$liveness_group,
    levels = c(
      "Low",
      "Moderate-Low",
      "Moderate-High",
      "High"
    )
  )


table(
  analysis_3_3$liveness_group
)


# violin plot shows where the popularity values
# are concentrated inside each liveness group

ggplot(
  analysis_3_3,
  aes(
    x = liveness_group,
    y = popularity,
    fill = liveness_group
  )
) +
  geom_violin(
    trim = FALSE
  ) +
  geom_boxplot(
    width = 0.12,
    fill = "white",
    outlier.size = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#377eb8",
      "Moderate-Low" = "#ff7f00",
      "Moderate-High" = "#4daf4a",
      "High" = "#e41a1c"
    )
  ) +
  labs(
    title = "Popularity Distribution by Liveness Level",
    x = "Liveness Level",
    y = "Popularity Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# check the medians too

liveness_low = analysis_3_3 %>%
  filter(
    liveness_group == "Low"
  )

liveness_moderate_low = analysis_3_3 %>%
  filter(
    liveness_group == "Moderate-Low"
  )

liveness_moderate_high = analysis_3_3 %>%
  filter(
    liveness_group == "Moderate-High"
  )

liveness_high = analysis_3_3 %>%
  filter(
    liveness_group == "High"
  )


nrow(liveness_low)
median(liveness_low$popularity)

nrow(liveness_moderate_low)
median(liveness_moderate_low$popularity)

nrow(liveness_moderate_high)
median(liveness_moderate_high$popularity)

nrow(liveness_high)
median(liveness_high$popularity)



# ============================================================
# EXTRA FEATURE
# Combined sonic profile
# ============================================================

# combine some of the useful patterns from 3-1 and 3-2

analysis_3_3 = analysis_3_3 %>%
  mutate(
    
    sonic_profile =
      case_when(
        
        danceability >= 0.4 &
          acousticness >= 0.2 &
          acousticness < 0.8 &
          speechiness < 0.33 &
          instrumentalness <= 0.5
        ~ "Selected Sonic Profile",
        
        TRUE ~ "Other Sonic Profiles"
      )
  )


analysis_3_3$sonic_profile =
  factor(
    analysis_3_3$sonic_profile,
    levels = c(
      "Selected Sonic Profile",
      "Other Sonic Profiles"
    )
  )


table(
  analysis_3_3$sonic_profile
)


# overall comparison before splitting by liveness

selected_profile = analysis_3_3 %>%
  filter(
    sonic_profile == "Selected Sonic Profile"
  )

other_profiles = analysis_3_3 %>%
  filter(
    sonic_profile == "Other Sonic Profiles"
  )


nrow(selected_profile)
mean(selected_profile$popularity)
median(selected_profile$popularity)

nrow(other_profiles)
mean(other_profiles$popularity)
median(other_profiles$popularity)


# split the two profiles across the four liveness levels

selected_low = analysis_3_3 %>%
  filter(
    sonic_profile == "Selected Sonic Profile",
    liveness_group == "Low"
  )

selected_moderate_low = analysis_3_3 %>%
  filter(
    sonic_profile == "Selected Sonic Profile",
    liveness_group == "Moderate-Low"
  )

selected_moderate_high = analysis_3_3 %>%
  filter(
    sonic_profile == "Selected Sonic Profile",
    liveness_group == "Moderate-High"
  )

selected_high = analysis_3_3 %>%
  filter(
    sonic_profile == "Selected Sonic Profile",
    liveness_group == "High"
  )


other_low = analysis_3_3 %>%
  filter(
    sonic_profile == "Other Sonic Profiles",
    liveness_group == "Low"
  )

other_moderate_low = analysis_3_3 %>%
  filter(
    sonic_profile == "Other Sonic Profiles",
    liveness_group == "Moderate-Low"
  )

other_moderate_high = analysis_3_3 %>%
  filter(
    sonic_profile == "Other Sonic Profiles",
    liveness_group == "Moderate-High"
  )

other_high = analysis_3_3 %>%
  filter(
    sonic_profile == "Other Sonic Profiles",
    liveness_group == "High"
  )


# small table for the line plot

interaction_data = data.frame(
  
  liveness_group = c(
    "Low",
    "Moderate-Low",
    "Moderate-High",
    "High",
    "Low",
    "Moderate-Low",
    "Moderate-High",
    "High"
  ),
  
  sonic_profile = c(
    "Selected Sonic Profile",
    "Selected Sonic Profile",
    "Selected Sonic Profile",
    "Selected Sonic Profile",
    "Other Sonic Profiles",
    "Other Sonic Profiles",
    "Other Sonic Profiles",
    "Other Sonic Profiles"
  ),
  
  Median_Popularity = c(
    median(selected_low$popularity),
    median(selected_moderate_low$popularity),
    median(selected_moderate_high$popularity),
    median(selected_high$popularity),
    
    median(other_low$popularity),
    median(other_moderate_low$popularity),
    median(other_moderate_high$popularity),
    median(other_high$popularity)
  )
)


interaction_data$liveness_group =
  factor(
    interaction_data$liveness_group,
    levels = c(
      "Low",
      "Moderate-Low",
      "Moderate-High",
      "High"
    )
  )


interaction_data


# line plot makes the two profile patterns easier to compare

ggplot(
  interaction_data,
  aes(
    x = liveness_group,
    y = Median_Popularity,
    group = sonic_profile,
    color = sonic_profile
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Selected Sonic Profile" = "#e41a1c",
      "Other Sonic Profiles" = "#377eb8"
    )
  ) +
  labs(
    title = "Median Popularity by Liveness and Sonic Profile",
    x = "Liveness Level",
    y = "Median Popularity",
    color = "Sonic Profile"
  ) +
  theme_minimal()
