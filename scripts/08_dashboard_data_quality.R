library(dplyr)
library(readr)

study_data_quality <- read_csv(
  "outputs/study_data_quality.csv",
  show_col_types = FALSE
)

dashboard_data_quality <- study_data_quality %>%
  select(
    study_folder,
    data_group,
    total_missing,
    percent_missing_overall,
    mean_percent_missing_participant,
    range_percent_missing_participant,
    complete_cases,
    participants_85_percent_complete,
    intake_macros_computed
  ) %>%
  rename(
    study = study_folder,
    `Data group` = data_group,
    `Total missing` = total_missing,
    `% missing overall` = percent_missing_overall,
    `Mean % missing by participant` =
      mean_percent_missing_participant,
    `Range of % missing by participant` =
      range_percent_missing_participant,
    `# complete cases` = complete_cases,
    `# participants with at least 85% complete data` =
      participants_85_percent_complete,
    `Intake macros computed` =
      intake_macros_computed
  )

View(dashboard_data_quality)

write_csv(
  dashboard_data_quality,
  "outputs/dashboard_data_quality.csv",
  na = ""
)