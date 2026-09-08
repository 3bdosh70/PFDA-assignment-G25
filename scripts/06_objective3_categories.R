# ============================================================
# PROGRAMMING FOR DATA ANALYSIS
# GROUP 25
# SONG POPULARITY PREDICTION
# ============================================================

# Script: 06_objective3_categories.R
# Responsible student:
# Name:
# TP Number:

# Objective 3:
# To determine whether song popularity differs according to
# genre, explicit status and musical categories.

# Main variables:
# popularity, explicit, mode, key, time_signature, track_genre

# Purpose:
# To compare popularity distributions across categorical
# variables and identify whether meaningful differences exist
# between groups.
# ============================================================
# ============================================================
# 3.1 OBJECTIVE 3
# Rhythm, Acoustic and Vocal Characteristics
# Abdulaziz Qaderi TP091283
# ============================================================

# Objective:
# To investigate how danceability, acoustic characteristics,
# vocal content and liveness are associated with Spotify
# track popularity.


# ============================================================
# 3.1.1 ANALYSIS 3-1
# DANCEABILITY, ACOUSTICNESS AND POPULARITY
# ============================================================

# Analysis question:
# How does the combination of danceability and acousticness
# relate to track popularity, and which characteristic shows
# the stronger association?


#-----------------------------------------------------
#Create the dataset required for Analysis 3-1

analysis_3_1 =
  exploration_data %>%
  select(
    popularity,
    danceability,
    acousticness
  ) %>%
  filter(
    !is.na(popularity),
    !is.na(danceability),
    !is.na(acousticness)
  )


#-----------------------------------------------------
#Check the analysis dataset

nrow(analysis_3_1)

colSums(
  is.na(analysis_3_1)
)

summary(analysis_3_1)

#Only records containing valid values for all three
#variables are used so both relationships are compared
#using the same observations.


#-----------------------------------------------------
#Correlation between danceability and popularity

danceability_correlation =
  cor(
    analysis_3_1$danceability,
    analysis_3_1$popularity
  )

danceability_correlation


danceability_test =
  cor.test(
    analysis_3_1$danceability,
    analysis_3_1$popularity
  )

danceability_test


#-----------------------------------------------------
#Correlation between acousticness and popularity

acousticness_correlation =
  cor(
    analysis_3_1$acousticness,
    analysis_3_1$popularity
  )

acousticness_correlation


acousticness_test =
  cor.test(
    analysis_3_1$acousticness,
    analysis_3_1$popularity
  )

acousticness_test


#-----------------------------------------------------
#Compare the strength of the two relationships

correlation_comparison = data.frame(
  
  Variable = c(
    "Danceability",
    "Acousticness"
  ),
  
  Correlation = c(
    danceability_correlation,
    acousticness_correlation
  ),
  
  Absolute_Correlation = c(
    abs(danceability_correlation),
    abs(acousticness_correlation)
  )
)

correlation_comparison

#The absolute correlation is used to compare relationship
#strength because positive and negative correlations can
#both represent meaningful associations.


# ============================================================
# COMBINED DANCEABILITY-ACOUSTICNESS PROFILE
# ============================================================

#-----------------------------------------------------
#Divide both characteristics into five ranges

analysis_3_1$danceability_group =
  cut(
    analysis_3_1$danceability,
    breaks = c(
      0,
      0.2,
      0.4,
      0.6,
      0.8,
      1
    ),
    include.lowest = TRUE,
    labels = c(
      "0.0-0.2",
      "0.2-0.4",
      "0.4-0.6",
      "0.6-0.8",
      "0.8-1.0"
    )
  )


analysis_3_1$acousticness_group =
  cut(
    analysis_3_1$acousticness,
    breaks = c(
      0,
      0.2,
      0.4,
      0.6,
      0.8,
      1
    ),
    include.lowest = TRUE,
    labels = c(
      "0.0-0.2",
      "0.2-0.4",
      "0.4-0.6",
      "0.6-0.8",
      "0.8-1.0"
    )
  )


#-----------------------------------------------------
#Calculate popularity for every combined profile

dance_acoustic_profile =
  analysis_3_1 %>%
  group_by(
    danceability_group,
    acousticness_group
  ) %>%
  summarise(
    
    Count =
      n(),
    
    Mean_Popularity =
      mean(popularity),
    
    Median_Popularity =
      median(popularity),
    
    Standard_Deviation =
      sd(popularity),
    
    .groups = "drop"
  )


dance_acoustic_profile


#-----------------------------------------------------
#Find the profiles with the highest median popularity

dance_acoustic_profile =
  dance_acoustic_profile %>%
  arrange(
    desc(Median_Popularity)
  )

dance_acoustic_profile


head(
  dance_acoustic_profile,
  10
)


#-----------------------------------------------------
#Heatmap of combined characteristics and popularity

ggplot(
  dance_acoustic_profile,
  aes(
    x = acousticness_group,
    y = danceability_group,
    fill = Median_Popularity
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label =
        round(
          Median_Popularity,
          1
        )
    )
  ) +
  labs(
    title =
      "Median Popularity by Danceability and Acousticness Profile",
    
    x =
      "Acousticness Range",
    
    y =
      "Danceability Range",
    
    fill =
      "Median Popularity"
  )


# ============================================================
# SUPPORTING REGRESSION ANALYSIS
# ============================================================

#-----------------------------------------------------
#Examine both predictors together

model_3_1 =
  lm(
    popularity ~
      danceability +
      acousticness,
    data = analysis_3_1
  )


summary(model_3_1)


#-----------------------------------------------------
#Examine whether the relationship between danceability
#and popularity changes depending on acousticness

interaction_model_3_1 =
  lm(
    popularity ~
      danceability *
      acousticness,
    data = analysis_3_1
  )


summary(interaction_model_3_1)


# ============================================================
# ANALYSIS 3-1 OUTPUT SUMMARY
# ============================================================

cat(
  "\n--- ANALYSIS 3-1 SUMMARY ---\n"
)

cat(
  "Records used:",
  nrow(analysis_3_1),
  "\n"
)

cat(
  "Danceability correlation:",
  danceability_correlation,
  "\n"
)

cat(
  "Acousticness correlation:",
  acousticness_correlation,
  "\n"
)

cat(
  "Strongest absolute correlation:",
  correlation_comparison$Variable[
    which.max(
      correlation_comparison$Absolute_Correlation
    )
  ],
  "\n"
)

cat(
  "Highest median-popularity profile:\n"
)

print(
  head(
    dance_acoustic_profile,
    1
  )
)

# ============================================================
# 3.1.2 ANALYSIS 3-2
# VOCAL CONTENT PROFILE AND POPULARITY
# ============================================================

# Analysis question:
# Do speechiness and instrumentalness add useful information
# about popularity after accounting for the danceability-
# acousticness relationship identified in Analysis 3-1,
# and which vocal-content profiles are associated with
# higher popularity?


#-----------------------------------------------------
#Create the dataset required for Analysis 3-2

analysis_3_2 =
  exploration_data %>%
  select(
    popularity,
    danceability,
    acousticness,
    speechiness,
    instrumentalness
  ) %>%
  filter(
    !is.na(popularity),
    !is.na(danceability),
    !is.na(acousticness),
    !is.na(speechiness),
    !is.na(instrumentalness)
  )


#-----------------------------------------------------
#Check the analysis dataset

nrow(analysis_3_2)

colSums(
  is.na(analysis_3_2)
)

summary(analysis_3_2)


# ============================================================
# INDIVIDUAL VOCAL CHARACTERISTIC ASSOCIATIONS
# ============================================================

#-----------------------------------------------------
#Speechiness and popularity

speechiness_correlation =
  cor(
    analysis_3_2$speechiness,
    analysis_3_2$popularity
  )

speechiness_correlation


speechiness_test =
  cor.test(
    analysis_3_2$speechiness,
    analysis_3_2$popularity
  )

speechiness_test


#-----------------------------------------------------
#Instrumentalness and popularity

instrumentalness_correlation =
  cor(
    analysis_3_2$instrumentalness,
    analysis_3_2$popularity
  )

instrumentalness_correlation


instrumentalness_test =
  cor.test(
    analysis_3_2$instrumentalness,
    analysis_3_2$popularity
  )

instrumentalness_test


#-----------------------------------------------------
#Compare the two associations

vocal_correlation_comparison =
  data.frame(
    
    Variable = c(
      "Speechiness",
      "Instrumentalness"
    ),
    
    Correlation = c(
      speechiness_correlation,
      instrumentalness_correlation
    ),
    
    Absolute_Correlation = c(
      abs(speechiness_correlation),
      abs(instrumentalness_correlation)
    )
  )

vocal_correlation_comparison


# ============================================================
# CREATE VOCAL-CONTENT PROFILES
# ============================================================

#-----------------------------------------------------
#Group speechiness using documented Spotify-style ranges

analysis_3_2$speechiness_group =
  cut(
    analysis_3_2$speechiness,
    breaks = c(
      -Inf,
      0.33,
      0.66,
      Inf
    ),
    right = FALSE,
    labels = c(
      "Music / Low Speech",
      "Music + Speech",
      "Mostly Spoken"
    )
  )


#-----------------------------------------------------
#Separate likely vocal and likely instrumental tracks

analysis_3_2$instrumentalness_group =
  ifelse(
    analysis_3_2$instrumentalness > 0.5,
    "Likely Instrumental",
    "Likely Vocal"
  )


#-----------------------------------------------------
#Check group sizes

table(
  analysis_3_2$speechiness_group
)

table(
  analysis_3_2$instrumentalness_group
)


# ============================================================
# COMBINED VOCAL PROFILE
# ============================================================

vocal_profile =
  analysis_3_2 %>%
  group_by(
    speechiness_group,
    instrumentalness_group
  ) %>%
  summarise(
    
    Count =
      n(),
    
    Mean_Popularity =
      mean(popularity),
    
    Median_Popularity =
      median(popularity),
    
    Standard_Deviation =
      sd(popularity),
    
    .groups = "drop"
  )


vocal_profile


#-----------------------------------------------------
#Rank the profiles by median popularity

vocal_profile =
  vocal_profile %>%
  arrange(
    desc(Median_Popularity)
  )

vocal_profile



#-----------------------------------------------------
#Grouped boxplot of vocal-content profiles

ggplot(
  analysis_3_2,
  aes(
    x = speechiness_group,
    y = popularity,
    fill = instrumentalness_group
  )
) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Popularity Distribution by Vocal-Content Profile",
    x = "Speechiness Profile",
    y = "Popularity Score",
    fill = "Instrumentalness Profile"
  )


# ============================================================
# MODEL COMPARISON
# ============================================================

#-----------------------------------------------------
#Baseline model:
#Only the danceability-acousticness relationship
#discovered in Analysis 3-1

baseline_model_3_2 =
  lm(
    popularity ~
      danceability *
      acousticness,
    data = analysis_3_2
  )


#-----------------------------------------------------
#Add speechiness and instrumentalness

extended_model_3_2 =
  lm(
    popularity ~
      danceability *
      acousticness +
      speechiness +
      instrumentalness,
    data = analysis_3_2
  )


#-----------------------------------------------------
#Add interaction between speechiness and instrumentalness

interaction_model_3_2 =
  lm(
    popularity ~
      danceability *
      acousticness +
      speechiness *
      instrumentalness,
    data = analysis_3_2
  )


#-----------------------------------------------------
#Model summaries

summary(
  baseline_model_3_2
)

summary(
  extended_model_3_2
)

summary(
  interaction_model_3_2
)


#-----------------------------------------------------
#Compare whether adding vocal characteristics
#improves the model

anova(
  baseline_model_3_2,
  extended_model_3_2,
  interaction_model_3_2
)


#-----------------------------------------------------
#Compare model explanatory power

model_comparison_3_2 =
  data.frame(
    
    Model = c(
      "Danceability + Acousticness",
      "Add Vocal Characteristics",
      "Add Vocal Interaction"
    ),
    
    R_Squared = c(
      summary(
        baseline_model_3_2
      )$r.squared,
      
      summary(
        extended_model_3_2
      )$r.squared,
      
      summary(
        interaction_model_3_2
      )$r.squared
    ),
    
    Adjusted_R_Squared = c(
      summary(
        baseline_model_3_2
      )$adj.r.squared,
      
      summary(
        extended_model_3_2
      )$adj.r.squared,
      
      summary(
        interaction_model_3_2
      )$adj.r.squared
    )
  )


model_comparison_3_2


# ============================================================
# ANALYSIS 3-2 OUTPUT SUMMARY
# ============================================================

cat(
  "\n--- ANALYSIS 3-2 SUMMARY ---\n"
)

cat(
  "Records used:",
  nrow(analysis_3_2),
  "\n"
)

cat(
  "Speechiness correlation:",
  speechiness_correlation,
  "\n"
)

cat(
  "Instrumentalness correlation:",
  instrumentalness_correlation,
  "\n"
)

cat(
  "Strongest vocal characteristic:",
  vocal_correlation_comparison$Variable[
    which.max(
      vocal_correlation_comparison$Absolute_Correlation
    )
  ],
  "\n"
)

cat(
  "Highest median-popularity vocal profile:\n"
)

print(
  head(
    vocal_profile,
    1
  )
)

cat(
  "\nModel comparison:\n"
)

print(
  model_comparison_3_2
)

# ============================================================
# 3.1.3 ANALYSIS 3-3
# LIVENESS AND THE OVERALL SONIC PROFILE
# ============================================================

# Analysis question:
# Does liveness change the popularity pattern of the
# higher-performing sonic profile identified in Analyses
# 3-1 and 3-2, and does liveness add useful information
# when all five characteristics are considered together?


#-----------------------------------------------------
#Create the Analysis 3-3 dataset

analysis_3_3 =
  exploration_data %>%
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


#-----------------------------------------------------
#Check the dataset

nrow(analysis_3_3)

colSums(
  is.na(analysis_3_3)
)

summary(analysis_3_3)


# ============================================================
# BASIC LIVENESS ASSOCIATION
# ============================================================

#-----------------------------------------------------
#Correlation between liveness and popularity

liveness_correlation =
  cor(
    analysis_3_3$liveness,
    analysis_3_3$popularity
  )

liveness_correlation


liveness_test =
  cor.test(
    analysis_3_3$liveness,
    analysis_3_3$popularity
  )

liveness_test


# ============================================================
# CREATE THE COMBINED SONIC PROFILE
# ============================================================

#The profile below is based on the patterns discovered
#in Analyses 3-1 and 3-2.
#
#Analysis 3-1 showed stronger popularity in tracks with
#moderate-to-high danceability and moderate acousticness.
#
#Analysis 3-2 showed the strongest reliable vocal profile
#among likely-vocal tracks with low speechiness.


analysis_3_3$sonic_profile =
  ifelse(
    
    analysis_3_3$danceability >= 0.4 &
      analysis_3_3$acousticness >= 0.2 &
      analysis_3_3$acousticness < 0.8 &
      analysis_3_3$speechiness < 0.33 &
      analysis_3_3$instrumentalness <= 0.5,
    
    "Higher-Performing Sonic Profile",
    
    "Other Sonic Profiles"
  )


#-----------------------------------------------------
#Check the number of records in each profile

table(
  analysis_3_3$sonic_profile
)


#-----------------------------------------------------
#Compare popularity between the two overall profiles

sonic_profile_summary =
  analysis_3_3 %>%
  group_by(
    sonic_profile
  ) %>%
  summarise(
    
    Count =
      n(),
    
    Mean_Popularity =
      mean(popularity),
    
    Median_Popularity =
      median(popularity),
    
    Standard_Deviation =
      sd(popularity),
    
    .groups = "drop"
  )


sonic_profile_summary


# ============================================================
# GROUP LIVENESS INTO FOUR DATA-BASED LEVELS
# ============================================================

#Quartiles are used so that the liveness groups contain
#approximately similar numbers of observations.

liveness_breaks =
  quantile(
    analysis_3_3$liveness,
    probs = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    na.rm = TRUE
  )

liveness_breaks


analysis_3_3$liveness_group =
  cut(
    analysis_3_3$liveness,
    
    breaks =
      unique(
        liveness_breaks
      ),
    
    include.lowest = TRUE,
    
    labels = c(
      "Low",
      "Moderate-Low",
      "Moderate-High",
      "High"
    )
  )


#-----------------------------------------------------
#Check the liveness groups

table(
  analysis_3_3$liveness_group
)


# ============================================================
# LIVENESS WITHIN THE SONIC PROFILES
# ============================================================

liveness_profile_summary =
  analysis_3_3 %>%
  group_by(
    sonic_profile,
    liveness_group
  ) %>%
  summarise(
    
    Count =
      n(),
    
    Mean_Popularity =
      mean(popularity),
    
    Median_Popularity =
      median(popularity),
    
    Standard_Deviation =
      sd(popularity),
    
    .groups = "drop"
  )


liveness_profile_summary


#-----------------------------------------------------
#Rank the combinations by median popularity

liveness_profile_summary =
  liveness_profile_summary %>%
  arrange(
    desc(Median_Popularity)
  )


liveness_profile_summary


# ============================================================
# VISUALISATION
# ============================================================

#Grouped boxplot showing whether popularity changes
#across liveness levels for the two sonic profiles.

ggplot(
  analysis_3_3,
  aes(
    x = liveness_group,
    y = popularity,
    fill = sonic_profile
  )
) +
  geom_boxplot(
    outlier.alpha = 0.2
  ) +
  labs(
    title =
      "Popularity Distribution by Liveness and Sonic Profile",
    
    x =
      "Liveness Level",
    
    y =
      "Popularity Score",
    
    fill =
      "Sonic Profile"
  )


# ============================================================
# FULL MODEL COMPARISON
# ============================================================

#-----------------------------------------------------
#Model without liveness

model_without_liveness =
  lm(
    popularity ~
      danceability *
      acousticness +
      speechiness *
      instrumentalness,
    
    data =
      analysis_3_3
  )


#-----------------------------------------------------
#Add liveness

model_with_liveness =
  lm(
    popularity ~
      danceability *
      acousticness +
      speechiness *
      instrumentalness +
      liveness,
    
    data =
      analysis_3_3
  )


#-----------------------------------------------------
#Model liveness together with the discovered sonic profile

model_liveness_profile =
  lm(
    popularity ~
      danceability *
      acousticness +
      speechiness *
      instrumentalness +
      liveness *
      sonic_profile,
    
    data =
      analysis_3_3
  )


#-----------------------------------------------------
#Display model results

summary(
  model_without_liveness
)

summary(
  model_with_liveness
)

summary(
  model_liveness_profile
)


#-----------------------------------------------------
#Test whether adding liveness improves the analysis

anova(
  model_without_liveness,
  model_with_liveness,
  model_liveness_profile
)


#-----------------------------------------------------
#Compare explanatory power

model_comparison_3_3 =
  data.frame(
    
    Model = c(
      "Previous Four Characteristics",
      "Add Liveness",
      "Liveness + Sonic Profile Interaction"
    ),
    
    R_Squared = c(
      
      summary(
        model_without_liveness
      )$r.squared,
      
      summary(
        model_with_liveness
      )$r.squared,
      
      summary(
        model_liveness_profile
      )$r.squared
    ),
    
    Adjusted_R_Squared = c(
      
      summary(
        model_without_liveness
      )$adj.r.squared,
      
      summary(
        model_with_liveness
      )$adj.r.squared,
      
      summary(
        model_liveness_profile
      )$adj.r.squared
    )
  )


model_comparison_3_3


# ============================================================
# ANALYSIS 3-3 OUTPUT SUMMARY
# ============================================================

cat(
  "\n--- ANALYSIS 3-3 SUMMARY ---\n"
)

cat(
  "Records used:",
  nrow(analysis_3_3),
  "\n"
)

cat(
  "Liveness correlation:",
  liveness_correlation,
  "\n"
)

cat(
  "\nOverall sonic profile comparison:\n"
)

print(
  sonic_profile_summary
)

cat(
  "\nLiveness and sonic profile combinations:\n"
)

print(
  liveness_profile_summary
)

cat(
  "\nModel comparison:\n"
)

print(
  model_comparison_3_3
)