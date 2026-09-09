# ============================================================
# OBJECTIVE 4 - MUSICAL HARMONY, METER AND PREDICTION
# ============================================================

# Objective 4:
# To evaluate how musical key, modality, and time signature
# relate to track popularity and assess their usefulness
# for predicting popularity scores.

library(dplyr)
library(ggplot2)


#-----------------------------------------------------
# Import cleaned dataset

objective4_data =
  read.csv(
    "data/processed/spotify_tracks_clean.csv",
    stringsAsFactors = FALSE
  )


#-----------------------------------------------------
# Select Objective 4 variables

objective4_data =
  objective4_data %>%
  select(
    popularity,
    key,
    mode,
    time_signature
  )


#-----------------------------------------------------
# Check data

str(objective4_data)

summary(objective4_data)

colSums(is.na(objective4_data))

nrow(objective4_data)


# ============================================================
# ANALYSIS 4-1
# MUSICAL KEY + MODE AND POPULARITY
# ============================================================

# Analysis 4-1:
# How do combinations of musical key and mode correspond
# with differences in track popularity?


#-----------------------------------------------------
# Create readable mode labels

objective4_data =
  objective4_data %>%
  mutate(
    mode_name =
      case_when(
        mode == 0 ~ "Minor",
        mode == 1 ~ "Major"
      )
  )
#-----------------------------------------------------
# Summary by mode

mode_summary =
  objective4_data %>%
  group_by(mode_name) %>%
  summarise(
    Count = n(),
    Mean_Popularity =
      mean(popularity, na.rm = TRUE),
    Median_Popularity =
      median(popularity, na.rm = TRUE),
    SD_Popularity =
      sd(popularity, na.rm = TRUE)
  )

mode_summary


#-----------------------------------------------------
# Popularity by mode

ggplot(
  objective4_data,
  aes(
    x = mode_name,
    y = popularity
  )
) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  labs(
    title = "Track Popularity by Musical Mode",
    x = "Musical Mode",
    y = "Popularity Score"
  )


#-----------------------------------------------------
# Test difference between major and minor mode

major_popularity =
  objective4_data$popularity[
    objective4_data$mode == 1
  ]

minor_popularity =
  objective4_data$popularity[
    objective4_data$mode == 0
  ]


mode_test =
  t.test(
    major_popularity,
    minor_popularity
  )

mode_test


#-----------------------------------------------------
# Combined key and mode analysis

key_mode_summary =
  objective4_data %>%
  group_by(
    key,
    mode_name
  ) %>%
  summarise(
    Count = n(),
    Mean_Popularity =
      mean(popularity, na.rm = TRUE),
    Median_Popularity =
      median(popularity, na.rm = TRUE)
  )

key_mode_summary


#-----------------------------------------------------
# Sort highest median popularity

key_mode_summary =
  key_mode_summary %>%
  arrange(
    desc(Median_Popularity)
  )

key_mode_summary


#-----------------------------------------------------
# Highest key-mode combinations

head(
  key_mode_summary,
  10
)


#-----------------------------------------------------
# Key and mode visualization

ggplot(
  key_mode_summary,
  aes(
    x = factor(key),
    y = Median_Popularity,
    fill = mode_name
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Median Popularity by Musical Key and Mode",
    x = "Musical Key",
    y = "Median Popularity",
    fill = "Mode"
  )


# ============================================================
# ANALYSIS 4-2
# KEY + MODE + TIME SIGNATURE
# ============================================================

# Analysis 4-2:
# Within the key-mode patterns identified in Analysis 4-1,
# how does time signature further distinguish popularity?


#-----------------------------------------------------
# Check time signature frequencies

time_signature_counts =
  table(
    objective4_data$time_signature
  )

time_signature_counts


#-----------------------------------------------------
# Create combined profile

key_mode_time_summary =
  objective4_data %>%
  group_by(
    key,
    mode_name,
    time_signature
  ) %>%
  summarise(
    Count = n(),
    Mean_Popularity =
      mean(popularity, na.rm = TRUE),
    Median_Popularity =
      median(popularity, na.rm = TRUE),
    SD_Popularity =
      sd(popularity, na.rm = TRUE)
  )

key_mode_time_summary


#-----------------------------------------------------
# Sort profiles by median popularity

key_mode_time_summary =
  key_mode_time_summary %>%
  arrange(
    desc(Median_Popularity)
  )

key_mode_time_summary


#-----------------------------------------------------
# Show highest profiles

head(
  key_mode_time_summary,
  15
)


#-----------------------------------------------------
# Remove very small groups for reliable comparison

reliable_profiles =
  key_mode_time_summary %>%
  filter(
    Count >= 30
  )

reliable_profiles


#-----------------------------------------------------
# Highest reliable profiles

head(
  reliable_profiles,
  15
)


#-----------------------------------------------------
# Time signature and key visualization

ggplot(
  objective4_data,
  aes(
    x = factor(key),
    y = popularity,
    fill = factor(time_signature)
  )
) +
  geom_boxplot() +
  labs(
    title = "Track Popularity by Key and Time Signature",
    x = "Musical Key",
    y = "Popularity Score",
    fill = "Time Signature"
  )


#-----------------------------------------------------
# Median popularity for combined profiles

ggplot(
  reliable_profiles,
  aes(
    x = factor(key),
    y = Median_Popularity,
    fill = factor(time_signature)
  )
) +
  geom_col(
    position = "dodge"
  ) +
  facet_wrap(
    ~ mode_name
  ) +
  labs(
    title = "Median Popularity by Key, Mode and Time Signature",
    x = "Musical Key",
    y = "Median Popularity",
    fill = "Time Signature"
  )


# ============================================================
# ANALYSIS 4-3
# PREDICTION MODEL
# ============================================================

# Analysis 4-3:
# How accurately can key, mode and time_signature collectively
# predict track popularity?


#-----------------------------------------------------
# Prepare prediction data

prediction_data =
  objective4_data %>%
  select(
    popularity,
    key,
    mode,
    time_signature
  )


#-----------------------------------------------------
# Remove remaining missing values

prediction_data =
  na.omit(
    prediction_data
  )


nrow(prediction_data)

colSums(
  is.na(prediction_data)
)


#-----------------------------------------------------
# Convert predictors into categorical variables

prediction_data$key =
  factor(
    prediction_data$key
  )

prediction_data$mode =
  factor(
    prediction_data$mode
  )

prediction_data$time_signature =
  factor(
    prediction_data$time_signature
  )


str(prediction_data)


#-----------------------------------------------------
# Split data into training and testing sets

set.seed(25)

training_rows =
  sample(
    1:nrow(prediction_data),
    size =
      0.80 *
      nrow(prediction_data)
  )


training_data =
  prediction_data[
    training_rows,
  ]


testing_data =
  prediction_data[
    -training_rows,
  ]


nrow(training_data)

nrow(testing_data)


#-----------------------------------------------------
# Model 1
# Basic combined model

model_1 =
  lm(
    popularity ~
      key +
      mode +
      time_signature,
    data = training_data
  )


summary(
  model_1
)


#-----------------------------------------------------
# Model 2
# Include interaction between key, mode and time signature

model_2 =
  lm(
    popularity ~
      key *
      mode *
      time_signature,
    data = training_data
  )


summary(
  model_2
)


#-----------------------------------------------------
# Predictions from Model 1

prediction_model_1 =
  predict(
    model_1,
    newdata = testing_data
  )


#-----------------------------------------------------
# Predictions from Model 2

prediction_model_2 =
  predict(
    model_2,
    newdata = testing_data
  )


#-----------------------------------------------------
# Model 1 results

results_model_1 =
  data.frame(
    Actual =
      testing_data$popularity,

    Predicted =
      prediction_model_1
  )


results_model_1 =
  results_model_1 %>%
  mutate(
    Error =
      Actual - Predicted,

    Absolute_Error =
      abs(Error),

    Squared_Error =
      Error ^ 2
  )


head(
  results_model_1
)


#-----------------------------------------------------
# Model 2 results

results_model_2 =
  data.frame(
    Actual =
      testing_data$popularity,

    Predicted =
      prediction_model_2
  )


results_model_2 =
  results_model_2 %>%
  mutate(
    Error =
      Actual - Predicted,

    Absolute_Error =
      abs(Error),

    Squared_Error =
      Error ^ 2
  )


head(
  results_model_2
)


#-----------------------------------------------------
# Model 1 MAE

MAE_model_1 =
  mean(
    results_model_1$Absolute_Error
  )

MAE_model_1


#-----------------------------------------------------
# Model 1 RMSE

RMSE_model_1 =
  sqrt(
    mean(
      results_model_1$Squared_Error
    )
  )

RMSE_model_1


#-----------------------------------------------------
# Model 2 MAE

MAE_model_2 =
  mean(
    results_model_2$Absolute_Error
  )

MAE_model_2


#-----------------------------------------------------
# Model 2 RMSE

RMSE_model_2 =
  sqrt(
    mean(
      results_model_2$Squared_Error
    )
  )

RMSE_model_2


#-----------------------------------------------------
# Model 1 testing R-squared

SSE_model_1 =
  sum(
    (
      results_model_1$Actual -
        results_model_1$Predicted
    ) ^ 2
  )


SST_model_1 =
  sum(
    (
      results_model_1$Actual -
        mean(results_model_1$Actual)
    ) ^ 2
  )


R2_model_1 =
  1 -
  (
    SSE_model_1 /
      SST_model_1
  )

R2_model_1


#-----------------------------------------------------
# Model 2 testing R-squared

SSE_model_2 =
  sum(
    (
      results_model_2$Actual -
        results_model_2$Predicted
    ) ^ 2
  )


SST_model_2 =
  sum(
    (
      results_model_2$Actual -
        mean(results_model_2$Actual)
    ) ^ 2
  )


R2_model_2 =
  1 -
  (
    SSE_model_2 /
      SST_model_2
  )

R2_model_2


#-----------------------------------------------------
# Compare prediction models

model_comparison =
  data.frame(
    Model =
      c(
        "Basic Model",
        "Interaction Model"
      ),

    MAE =
      c(
        MAE_model_1,
        MAE_model_2
      ),

    RMSE =
      c(
        RMSE_model_1,
        RMSE_model_2
      ),

    R_Squared =
      c(
        R2_model_1,
        R2_model_2
      )
  )
model_comparison


#-----------------------------------------------------
# Actual vs predicted - Model 1

ggplot(
  results_model_1,
  aes(
    x = Actual,
    y = Predicted
  )
) +
  geom_point(
    alpha = 0.4
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  labs(
    title = "Actual vs Predicted Popularity - Basic Model",
    x = "Actual Popularity",
    y = "Predicted Popularity"
  )


#-----------------------------------------------------
# Actual vs predicted - Model 2

ggplot(
  results_model_2,
  aes(
    x = Actual,
    y = Predicted
  )
) +
  geom_point(
    alpha = 0.4
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  labs(
    title = "Actual vs Predicted Popularity - Interaction Model",
    x = "Actual Popularity",
    y = "Predicted Popularity"
  )


#-----------------------------------------------------
# Prediction error distribution

ggplot(
  results_model_2,
  aes(
    x = Error
  )
) +
  geom_histogram(
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Prediction Errors",
    x = "Prediction Error",
    y = "Frequency"
  )
