library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

source("R/functions_data_quality.R")

curated_dir <- Sys.getenv("ROLLS_CURATED_DATA")

if (curated_dir == "") {
  stop("ROLLS_CURATED_DATA has not been set.")
}

if (!dir.exists(curated_dir)) {
  stop("The curated data folder could not be found.")
}

# Find every CSV in the curated data directory
all_csvs <- list.files(
  path = curated_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Standardize Windows file paths
all_csvs <- normalizePath(
  all_csvs,
  winslash = "/",
  mustWork = FALSE
)

# Keep only CSV files that are inside a data folder
data_csvs <- all_csvs[
  str_detect(
    all_csvs,
    regex("/data/", ignore_case = TRUE)
  )
]

# Stop if no data files were found
if (length(data_csvs) == 0) {
  stop("No CSV files were found inside the study data folders.")
}

# Run the data-quality function on every CSV
data_quality <- map_dfr(
  data_csvs,
  calculate_data_quality
)

# Arrange results by study and file
data_quality <- data_quality %>%
  arrange(
    study_folder,
    file_name
  )

# View the full results
View(data_quality)

# Save the results
write_csv(
  data_quality,
  "outputs/data_quality.csv",
  na = ""
)

# Print a smaller check to the Console
data_quality %>%
  select(
    study_folder,
    file_name,
    total_missing,
    percent_missing_overall,
    mean_percent_missing_participant,
    range_percent_missing_participant,
    complete_cases,
    participants_85_percent_complete,
    intake_macros_computed
  ) %>%
  print(n = 30)

# Show only datasets with at least one missing value
data_quality %>%
  filter(total_missing > 0) %>%
  select(
    study_folder,
    file_name,
    total_missing,
    percent_missing_overall
  ) %>%
  print(n = 30)