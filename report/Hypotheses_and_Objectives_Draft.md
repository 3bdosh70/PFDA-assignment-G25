# Group 25: Hypotheses and Objectives Draft

## Status

Draft Version 1 — to be reviewed after the initial data audit.

---

## Objective 1: Audio Intensity and Acoustic Characteristics

### Objective

To investigate whether audio intensity and acoustic characteristics are associated with song popularity.

### Main Variables

- popularity
- energy
- loudness
- acousticness
- instrumentalness

### Null Hypothesis — H0₁

Energy, loudness, acousticness and instrumentalness do not have a statistically significant relationship with song popularity.

### Alternative Hypothesis — H1₁

At least one audio intensity or acoustic characteristic has a statistically significant relationship with song popularity.

### Planned Methods

- Descriptive statistics
- Scatterplots
- Correlation analysis
- Multiple linear regression
- Multicollinearity checking

---

## Objective 2: Rhythm, Mood and Duration

### Objective

To examine whether rhythmic, emotional and duration-related characteristics are associated with song popularity.

### Main Variables

- popularity
- danceability
- valence
- tempo
- speechiness
- liveness
- duration_ms

### Null Hypothesis — H0₂

Danceability, valence, tempo, speechiness, liveness and duration do not have a statistically significant relationship with song popularity.

### Alternative Hypothesis — H1₂

At least one rhythm, mood or duration-related characteristic has a statistically significant relationship with song popularity.

### Planned Methods

- Descriptive statistics
- Scatterplots
- Correlation analysis
- Multiple regression
- Non-linear terms where justified

---

## Objective 3: Categorical and Genre Factors

### Objective

To determine whether song popularity differs according to genre, explicit status and musical categories.

### Explicit Status

#### H0₃a

There is no statistically significant difference in popularity between explicit and non-explicit tracks.

#### H1₃a

There is a statistically significant difference in popularity between explicit and non-explicit tracks.

### Mode

#### H0₃b

There is no statistically significant difference in popularity between tracks in major and minor mode.

#### H1₃b

There is a statistically significant difference in popularity between tracks in major and minor mode.

### Genre

#### H0₃c

Popularity does not significantly differ between track genres.

#### H1₃c

At least one track genre has a significantly different popularity distribution.

### Planned Methods

- Group summary statistics
- Boxplots or violin plots
- Welch's t-test or Wilcoxon test
- ANOVA, Welch ANOVA or Kruskal-Wallis test
- Effect sizes
- Post-hoc comparisons

---

## Objective 4: Popularity Prediction and Model Comparison

### Objective

To develop and compare models for predicting Spotify track popularity.

### Null Hypothesis — H0₄

The developed prediction models do not improve test-set prediction performance compared with the baseline model.

### Alternative Hypothesis — H1₄

At least one developed prediction model improves test-set prediction performance compared with the baseline model.

### Planned Models and Measures

- Mean baseline model
- Multiple linear regression
- One non-linear prediction model
- RMSE
- MAE
- Test-set R-squared

---

## Important Note

These hypotheses were prepared before formal analysis. Their wording may only be adjusted after the data audit when a variable is unusable or unsuitable. They must not be changed simply to match the final results.