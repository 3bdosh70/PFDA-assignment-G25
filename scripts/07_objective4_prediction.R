# ================================================================
# GROUP 25 | OBJECTIVE 4: MUSICAL HARMONY, METER AND PREDICTION
# Student: Abdulrahman Hussein Ali Mashrah (TP086182)
# ================================================================
# Research objective:
# Evaluate how musical key, mode and time signature relate to
# Spotify track popularity, and test their predictive usefulness.
#
# Run this script from the repository root in RStudio:
# source("scripts/07_objective4_prediction.R")
# Requirements: dplyr and ggplot2.
# Note: The shared cleaned file was prepared by the group. This
# script independently checks the key/mode/meter coding before use.
#
# Sections:
# 4-1: Descriptive summaries; an independent unique-track mode
#      test; joint key-mode popularity profiles
# 4-2: Time signature profiles with focused and comprehensive plots
# 4-3: Track-ID-separated prediction and baseline comparison,
#      followed by an independent raw-data sensitivity check
# ================================================================

library(dplyr)
library(ggplot2)

# Relative paths make the code work on the group's laptop.
input_file = "data/processed/spotify_tracks_clean.csv"
raw_file = "data/raw/spotify_tracks_data.csv"
table_dir = "outputs/tables/objective4"
figure_dir = "outputs/figures/objective4"

if (!file.exists(input_file)) {
  stop("Cleaned data not found. Run from the repository root after group cleaning.")
}
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

spotify_clean = read.csv(input_file, stringsAsFactors = FALSE)
required_columns = c("track_id", "popularity", "key", "mode", "time_signature")
stopifnot(all(required_columns %in% names(spotify_clean)))

objective4_data = spotify_clean %>%
  select(all_of(required_columns)) %>%
  mutate(
    track_id = as.character(track_id),
    mode_name = case_when(mode == 0 ~ "Minor", mode == 1 ~ "Major"),
    key_name = factor(
      key,
      levels = 0:11,
      labels = c("C", "C#/Db", "D", "D#/Eb", "E", "F",
                 "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")
    )
  )

# Verify the shared dataset, rather than silently changing it here.
print(str(objective4_data))
print(colSums(is.na(objective4_data)))
print(summary(objective4_data[, c("popularity", "key", "mode", "time_signature")]))
stopifnot(
  !anyNA(objective4_data),
  all(objective4_data$popularity >= 0 & objective4_data$popularity <= 100),
  all(objective4_data$key %in% 0:11),
  all(objective4_data$mode %in% 0:1),
  all(objective4_data$time_signature %in% c(3, 4, 5)),
  all(nzchar(objective4_data$track_id))
)
cat("\nObjective 4 prepared rows:", nrow(objective4_data), "\n")

# ================================================================
# ANALYSIS 4-1: MUSICAL MODE, THEN COMBINED KEY AND MODE
# ================================================================

mode_summary = objective4_data %>%
  group_by(mode_name) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  )
print(mode_summary)

# Repeated IDs represent the same song across some genre records.
# Summarise the popularity of EACH TRACK once for the hypothesis test.
# A few track IDs have conflicting mode codes, so exclude those ambiguous
# IDs from this track-level test rather than choosing a code arbitrarily.
mode_track_data = objective4_data %>%
  group_by(track_id) %>%
  summarise(
    Mode_Codes = n_distinct(mode),
    mode = first(mode),
    Track_Popularity = mean(popularity),
    .groups = "drop"
  ) %>%
  filter(Mode_Codes == 1)

mode_track_summary = mode_track_data %>%
  mutate(mode_name = ifelse(mode == 0, "Minor", "Major")) %>%
  group_by(mode_name) %>%
  summarise(
    Unique_Tracks = n(),
    Mean_Popularity = mean(Track_Popularity),
    Median_Popularity = median(Track_Popularity),
    .groups = "drop"
  )
print(mode_track_summary)
cat("Ambiguous IDs excluded from the track-level test:",
    n_distinct(objective4_data$track_id) - nrow(mode_track_data), "\n")

# Welch's two-sample test on independent unique-track observations.
mode_test = t.test(Track_Popularity ~ factor(mode),
                   data = mode_track_data)
print(mode_test)

# A tiny difference can be statistically significant with this many rows.
mean_mode_difference = mode_summary$Mean_Popularity[
  mode_summary$mode_name == "Minor"] - mode_summary$Mean_Popularity[
    mode_summary$mode_name == "Major"]
cat("Minor - Major mean popularity difference (all records):",
    round(mean_mode_difference, 3), "points\n")

# Figure 4.1 complements the t-test by showing both groups' spread.
figure_4_1 = ggplot(objective4_data,
                    aes(x = mode_name, y = popularity)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.15) +
  labs(title = "Spotify Popularity by Major and Minor Mode",
       subtitle = "Descriptive distribution across cleaned track records",
       x = "Musical Mode", y = "Popularity Score") +
  theme_minimal(base_size = 12)
print(figure_4_1)
ggsave(file.path(figure_dir, "objective4_figure_4_1_mode.png"),
       figure_4_1, width = 7.2, height = 4.5, dpi = 220)

key_mode_summary = objective4_data %>%
  group_by(key, key_name, mode_name) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    .groups = "drop"
  ) %>%
  arrange(desc(Median_Popularity), desc(Mean_Popularity))
print(head(key_mode_summary, 12))

figure_4_2 = ggplot(
  key_mode_summary,
  aes(x = key_name, y = Median_Popularity, fill = mode_name)
) +
  geom_col(position = "dodge") +
  labs(title = "Median Popularity by Musical Key and Mode",
       x = "Musical Key", y = "Median Popularity", fill = "Mode") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(figure_4_2)
ggsave(file.path(figure_dir, "objective4_figure_4_2_key_mode.png"),
       figure_4_2, width = 8, height = 4.8, dpi = 220)
write.csv(mode_track_summary,
          file.path(table_dir, "unique_track_mode_summary.csv"), row.names = FALSE)
write.csv(data.frame(Statistic = mode_test$statistic,
                     P_Value = mode_test$p.value,
                     Unique_Track_N = nrow(mode_track_data)),
          file.path(table_dir, "unique_track_mode_welch_test.csv"), row.names = FALSE)
write.csv(mode_summary, file.path(table_dir, "mode_summary.csv"), row.names = FALSE)
write.csv(key_mode_summary, file.path(table_dir, "key_mode_summary.csv"), row.names = FALSE)

# ================================================================
# ANALYSIS 4-2: ADDING TIME SIGNATURE
# ================================================================

signature_summary = objective4_data %>%
  count(time_signature, name = "Count") %>%
  mutate(Percentage = round(100 * Count / sum(Count), 2))
print(signature_summary)

key_mode_time_summary = objective4_data %>%
  group_by(key, key_name, mode_name, time_signature) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  ) %>%
  arrange(desc(Median_Popularity), desc(Mean_Popularity))

# Do not highlight profiles based on only a handful of tracks.
reliable_profiles = key_mode_time_summary %>% filter(Count >= 30)
print(head(reliable_profiles, 15))

# E major (Spotify key 4, mode 1) was the prominent key-mode profile.
key4_major = objective4_data %>% filter(key == 4, mode == 1)
key4_major_summary = key4_major %>%
  group_by(time_signature) %>%
  summarise(
    Count = n(),
    Mean_Popularity = mean(popularity),
    Median_Popularity = median(popularity),
    SD_Popularity = sd(popularity),
    .groups = "drop"
  ) %>%
  arrange(time_signature)
print(key4_major_summary)

figure_4_3 = ggplot(key4_major,
                    aes(x = factor(time_signature), y = popularity)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.25) +
  labs(title = "Popularity of E Major Tracks by Time Signature",
       x = "Time Signature", y = "Popularity Score") +
  theme_minimal(base_size = 12)
print(figure_4_3)
ggsave(file.path(figure_dir, "objective4_figure_4_3_e_major.png"),
       figure_4_3, width = 7.2, height = 4.6, dpi = 220)

# Figure 4.4 shows whether the E-major observation is isolated.
# This covers ALL sufficiently populated key/mode/time combinations.
figure_4_4 = ggplot(
  reliable_profiles,
  aes(x = key_name, y = Median_Popularity,
      fill = factor(time_signature))
) +
  geom_col(position = "dodge") +
  facet_wrap(~ mode_name, ncol = 1) +
  labs(title = "Median Popularity Across Key, Mode and Time Signature",
       subtitle = "Profiles with at least 30 records",
       x = "Musical Key", y = "Median Popularity",
       fill = "Time Signature") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(figure_4_4)
ggsave(file.path(figure_dir, "objective4_figure_4_4_full_profiles.png"),
       figure_4_4, width = 10.5, height = 7, dpi = 220)
write.csv(signature_summary, file.path(table_dir, "time_signature_summary.csv"), row.names = FALSE)
write.csv(reliable_profiles, file.path(table_dir, "reliable_profiles.csv"), row.names = FALSE)
write.csv(key4_major_summary, file.path(table_dir, "key4_major_summary.csv"), row.names = FALSE)

# ================================================================
# ANALYSIS 4-3: PREDICTIVE MODELLING AND TESTING
# ================================================================
# Track IDs repeat in this dataset (e.g., tracks occurring in more
# than one genre). A random ROW split could put the same track in
# both sets. Instead, assign each track ID to one set only.
#
# Spotify track IDs are pseudorandom-looking alphanumeric strings.
# This deterministic checksum puts approximately 20% of unique
# track IDs in the test set and is reproducible in base R.
# It is a reproducible partition, not a claim of random sampling.
# IMPORTANT: This uses only the ID, not the target popularity.

track_bucket = function(id) {
  character_codes = utf8ToInt(as.character(id))
  positions = seq_along(character_codes)
  as.integer(sum(character_codes * positions^2) %% 5)
}

prediction_data = objective4_data %>%
  select(track_id, popularity, key, mode, time_signature) %>%
  mutate(
    key = factor(key, levels = 0:11),
    mode = factor(mode, levels = 0:1),
    time_signature = factor(time_signature, levels = c(3, 4, 5))
  )

# Calculate once per unique track to make the split explicit.
track_partition = data.frame(
  track_id = unique(prediction_data$track_id),
  stringsAsFactors = FALSE
)
track_partition$bucket = vapply(track_partition$track_id, track_bucket, integer(1))
prediction_data = prediction_data %>%
  left_join(track_partition, by = "track_id")
training_data = prediction_data %>% filter(bucket != 0)
testing_data = prediction_data %>% filter(bucket == 0)

stopifnot(
  nrow(training_data) > 0,
  nrow(testing_data) > 0,
  length(intersect(unique(training_data$track_id),
                   unique(testing_data$track_id))) == 0
)
cat("\nUnique tracks train/test:",
    n_distinct(training_data$track_id), n_distinct(testing_data$track_id), "\n")
cat("Rows train/test:", nrow(training_data), nrow(testing_data), "\n")

# All 72 key x mode x meter combinations appear in the training
# data, so predictions from the interaction model are estimable.
training_profiles = training_data %>% count(key, mode, time_signature)
testing_profiles = testing_data %>% count(key, mode, time_signature)
stopifnot(nrow(training_profiles) == 72,
          nrow(anti_join(testing_profiles, training_profiles,
                         by = c("key", "mode", "time_signature"))) == 0)

# Constant baseline: predict the mean of TRAINING popularity only.
training_mean = mean(training_data$popularity)
baseline_predictions = rep(training_mean, nrow(testing_data))

# Main-effects model: separate categorical effects of all 3 variables.
model_1 = lm(popularity ~ key + mode + time_signature,
             data = training_data)

# Interaction model: additionally allows the effects to interact.
model_2 = lm(popularity ~ key * mode * time_signature,
             data = training_data)

# Estimate popularity for the same untouched test set.
main_predictions = as.numeric(predict(model_1, newdata = testing_data))
interaction_predictions = as.numeric(predict(model_2, newdata = testing_data))

# One consistent evaluation function for ALL models.
score_predictions = function(actual, predicted) {
  errors = actual - predicted
  test_mean = mean(actual)
  test_total_sum_squares = sum((actual - test_mean)^2)
  data.frame(
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    Test_R2 = 1 - sum(errors^2) / test_total_sum_squares
  )
}

baseline_scores = score_predictions(testing_data$popularity,
                                    baseline_predictions)
main_scores = score_predictions(testing_data$popularity,
                                main_predictions)
interaction_scores = score_predictions(testing_data$popularity,
                                       interaction_predictions)
model_comparison = bind_rows(
  cbind(Model = "Training-mean baseline", baseline_scores),
  cbind(Model = "Main-effects regression", main_scores),
  cbind(Model = "Interaction regression", interaction_scores)
)
print(model_comparison)

prediction_results = testing_data %>%
  select(track_id, popularity, key, mode, time_signature) %>%
  mutate(
    Baseline = baseline_predictions,
    Main_Effects = main_predictions,
    Interaction = interaction_predictions,
    Interaction_Error = popularity - Interaction
  )

# Dense full-data scatterplots hide patterns: use a plotted subset
# ONLY for display. Model fitting/evaluation always uses all rows.
set.seed(25)
plot_rows = sample(seq_len(nrow(prediction_results)),
                   size = min(3500, nrow(prediction_results)))
figure_4_5 = ggplot(prediction_results[plot_rows, ],
                    aes(x = popularity, y = Interaction)) +
  geom_point(alpha = 0.17, size = 0.65) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Actual vs Predicted Popularity: Interaction Regression",
       subtitle = "Display sample only; metrics use the full test set",
       x = "Actual Popularity", y = "Predicted Popularity") +
  theme_minimal(base_size = 12)
print(figure_4_5)
ggsave(file.path(figure_dir, "objective4_figure_4_5_actual_predicted.png"),
       figure_4_5, width = 7.3, height = 5.5, dpi = 220)

figure_4_6 = ggplot(prediction_results,
                    aes(x = Interaction_Error)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Interaction Regression: Prediction Errors",
       x = "Actual Minus Predicted Popularity", y = "Track Records") +
  theme_minimal(base_size = 12)
print(figure_4_6)
ggsave(file.path(figure_dir, "objective4_figure_4_6_errors.png"),
       figure_4_6, width = 7.2, height = 4.5, dpi = 220)

write.csv(model_comparison, file.path(table_dir, "model_comparison.csv"), row.names = FALSE)
write.csv(prediction_results, file.path(table_dir, "test_predictions.csv"), row.names = FALSE)
cat("\nAll Objective 4 analyses and primary prediction outputs completed.\n")

# ================================================================
# ROBUSTNESS CHECK: COMPLETE CASES FROM THE ORIGINAL RAW FILE
# ================================================================
# Limitation: the SHARED cleaned file was prepared before this
# train/test split. If global missing-value imputation used the
# entire dataset, that can introduce minor preprocessing leakage.
# The independent raw complete-case sensitivity check below
# avoids using globally imputed Objective 4 predictor values.
# It tests whether the same weak-prediction conclusion persists;
# it is not a replacement for the group's shared clean dataset.

if (!file.exists(raw_file)) {
  stop("Raw file missing: required for the Objective 4 robustness check.")
}
raw_data = read.csv(raw_file,
                    na.strings = c("", "NA", "N/A", "NULL"),
                    stringsAsFactors = FALSE)
# Drop only the artificial CSV index, not legitimate analysis data.
raw_data = raw_data[, !(names(raw_data) %in%
                          c("X", "Unnamed..0", "Unnamed: 0")), drop = FALSE]
raw_data = distinct(raw_data)

raw_complete = raw_data %>%
  select(all_of(required_columns)) %>%
  filter(
    !is.na(track_id), !is.na(popularity), !is.na(key),
    !is.na(mode), !is.na(time_signature),
    nzchar(track_id),
    popularity >= 0 & popularity <= 100,
    key %in% 0:11,
    mode %in% 0:1,
    time_signature %in% c(3, 4, 5)
  ) %>%
  mutate(
    key = factor(as.integer(key), levels = 0:11),
    mode = factor(as.integer(mode), levels = 0:1),
    time_signature = factor(as.integer(time_signature), levels = c(3, 4, 5))
  ) %>%
  left_join(track_partition, by = "track_id")

raw_training = raw_complete %>% filter(bucket != 0)
raw_testing = raw_complete %>% filter(bucket == 0)
stopifnot(!anyNA(raw_complete$bucket),
          length(intersect(unique(raw_training$track_id),
                           unique(raw_testing$track_id))) == 0)

raw_baseline = rep(mean(raw_training$popularity), nrow(raw_testing))
raw_main = lm(popularity ~ key + mode + time_signature,
              data = raw_training)
raw_interaction = lm(popularity ~ key * mode * time_signature,
                     data = raw_training)
robustness_comparison = bind_rows(
  cbind(Model = "Training-mean baseline",
        score_predictions(raw_testing$popularity, raw_baseline)),
  cbind(Model = "Main-effects regression",
        score_predictions(raw_testing$popularity,
                          predict(raw_main, newdata = raw_testing))),
  cbind(Model = "Interaction regression",
        score_predictions(raw_testing$popularity,
                          predict(raw_interaction, newdata = raw_testing)))
)
cat("\nRaw complete-case sensitivity rows:", nrow(raw_complete), "\n")
print(robustness_comparison)
write.csv(robustness_comparison,
          file.path(table_dir, "robustness_comparison.csv"), row.names = FALSE)
cat("\nOBJECTIVE 4 FINISHED: Rerun from repository root if inputs change.\n")
