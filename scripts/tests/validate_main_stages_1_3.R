# Run from the repository root:
# Rscript --vanilla scripts/tests/validate_main_stages_1_3.R
# Uses temporary output folders; never overwrites repository data or outputs.
# Load installed packages before treating workflow warnings as failures.
# A package-build/R-version notice is an environment warning, not a data warning.
library(dplyr)
library(ggplot2)
options(warn = 2)
repo = normalizePath(".", mustWork = TRUE)
main_file = file.path(repo, "scripts", "Main_stages_1_3.R")
invisible(parse(main_file))
fixture = tempfile("pfda-main-validation-")
dir.create(file.path(fixture, "data", "raw"), recursive = TRUE)
input_file = file.path(fixture, "data", "raw", "spotify_tracks_data.csv")
stopifnot(file.copy(file.path(repo, "data", "raw", "spotify_tracks_data.csv"), input_file))
setwd(fixture)
run = new.env()
source(main_file, local = run, echo = FALSE)
stopifnot(
  nrow(run$raw_data) == 114000L,
  nrow(run$prepared_before_imputation) == 113579L,
  sum(is.na(run$prepared_before_imputation)) ==
    run$missing_before_validation + sum(run$invalid_values_summary$invalid_count),
  run$rows_removed_duplicates == 396L,
  run$genre_duplicates_removed == 25L,
  run$popularity_rows_removed == 268L,
  run$final_duplicates_removed == 6L,
  nrow(run$clean_data) == 113305L,
  ncol(run$clean_data) == 20L,
  length(unique(run$clean_data$track_genre)) == 114L,
  !anyNA(run$clean_data),
  !anyDuplicated(run$clean_data),
  all(is.finite(run$correlation_matrix)),
  isTRUE(all.equal(run$correlation_matrix,
    cor(run$clean_data[run$correlation_variables]))),
  length(list.files(file.path(run$output_dir, "plots"), pattern = "[.]png$")) == 13L
)
exported = read.csv(file.path(run$output_dir, "spotify_tracks_clean.csv"),
  stringsAsFactors = FALSE)
reference = read.csv(file.path(repo, "data", "processed", "spotify_tracks_clean.csv"),
  stringsAsFactors = FALSE)
stopifnot(isTRUE(all.equal(exported, reference, check.attributes = FALSE)))
for (name in c("general_dataset_profile", "general_numeric_summary",
               "popularity_by_explicit", "popularity_by_mode", "popularity_correlations")) {
  table = read.csv(file.path(run$output_dir, "tables", paste0(name, ".csv")))
  stopifnot(!anyNA(table))
}

# A malformed input must fail explicitly before cleaning rather than silently
# truncating integers, misclassifying explicit labels or accepting bad schemas.
check_rejection = function(data, expected) {
  write.csv(data, input_file, row.names = FALSE)
  error = tryCatch({
    source(main_file, local = new.env(), echo = FALSE)
    NULL
  }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(error), grepl(expected, error, fixed = TRUE))
}
sample = run$raw_data[1:20, ]
bad = sample
bad$popularity[1] = 50.5
check_rejection(bad, "Unsafe integer conversion")
bad = sample
bad$explicit[1] = "unexpected"
check_rejection(bad, "Unexpected explicit label")
bad = sample
bad$energy[1] = Inf
check_rejection(bad, "Non-numeric or infinite input")
bad = sample
bad$track_id[1] = NA
check_rejection(bad, "Track IDs and genres must not be missing")
bad = sample
bad$energy = NULL
check_rejection(bad, "expected 21 columns")
setwd(repo)
cat("\nPASS: full source() run, reference dataset comparison, 13 plot exports,\n",
    "numeric tables, data invariants and five malformed-input checks.\n",
    "Temporary validation outputs:", fixture, "\n")
