library(tidyverse)

output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

study_quality_path <- file.path(
  output_dir,
  "study_data_quality.csv"
)

dataset_quality_path <- file.path(
  output_dir,
  "grouped_data_quality.csv"
)

if (!file.exists(study_quality_path)) {
  stop(
    "outputs/study_data_quality.csv was not found. ",
    "Run Script 07 first."
  )
}

if (!file.exists(dataset_quality_path)) {
  stop(
    "outputs/grouped_data_quality.csv was not found. ",
    "Run Script 06 first."
  )
}

study_data_quality <- readr::read_csv(
  study_quality_path,
  show_col_types = FALSE
)

grouped_data_quality <- readr::read_csv(
  dataset_quality_path,
  show_col_types = FALSE
)

required_study_columns <- c(
  "study_folder",
  "data_group",
  "n_variables_observed",
  "total_data_points",
  "total_missing",
  "percent_missing_overall",
  "mean_percent_missing_participant",
  "range_percent_missing_participant",
  "complete_cases",
  "participants_85_percent_complete",
  "intake_macros_computed"
)

missing_study_columns <- setdiff(
  required_study_columns,
  names(study_data_quality)
)

if (length(missing_study_columns) > 0) {
  stop(
    paste0(
      "study_data_quality.csv is missing these columns: ",
      paste(
        missing_study_columns,
        collapse = ", "
      )
    )
  )
}

required_dataset_columns <- c(
  "study_folder",
  "file_name",
  "data_group",
  "n_variables_observed",
  "total_data_points",
  "total_missing",
  "percent_missing_overall",
  "mean_percent_missing_participant",
  "percent_missing_range",
  "complete_cases",
  "participants_85_complete"
)

missing_dataset_columns <- setdiff(
  required_dataset_columns,
  names(grouped_data_quality)
)

if (length(missing_dataset_columns) > 0) {
  stop(
    paste0(
      "grouped_data_quality.csv is missing these columns: ",
      paste(
        missing_dataset_columns,
        collapse = ", "
      )
    )
  )
}

dashboard_data_quality <- study_data_quality %>%
  
  transmute(
    
    study =
      study_folder,
    
    `Data group` =
      data_group,
    
    `# variables observed` =
      n_variables_observed,
    
    `Total data points` =
      total_data_points,
    
    `Total missing` =
      total_missing,
    
    `% missing overall` =
      percent_missing_overall,
    
    `Mean % missing by participant` =
      mean_percent_missing_participant,
    
    `Range of % missing by participant` =
      range_percent_missing_participant,
    
    `# complete cases` =
      complete_cases,
    
    `# participants ≥85% complete` =
      participants_85_percent_complete,
    
    `Intake macros computed` =
      intake_macros_computed
  )

dashboard_data_quality <- dashboard_data_quality %>%
  
  mutate(
    
    `Data group` = factor(
      `Data group`,
      levels = c(
        "All data",
        "Intake data",
        "Questionnaire data",
        "Total intake variables"
      )
    )
  ) %>%
  
  arrange(
    study,
    `Data group`
  ) %>%
  
  mutate(
    `Data group` =
      as.character(
        `Data group`
      )
  )


readr::write_csv(
  
  dashboard_data_quality,
  
  file.path(
    output_dir,
    "dashboard_data_quality.csv"
  ),
  
  na = ""
)


dashboard_dataset_data_quality <- grouped_data_quality %>%
  
  transmute(
    
    study =
      study_folder,
    
    Dataset =
      file_name,
    
    `Data group` =
      data_group,
    
    `# variables observed` =
      n_variables_observed,
    
    `Total data points` =
      total_data_points,
    
    `Total missing` =
      total_missing,
    
    `% missing overall` =
      percent_missing_overall,
    
    `Mean % missing by participant` =
      mean_percent_missing_participant,
    
    `Range of % missing by participant` =
      percent_missing_range,
    
    `# complete cases` =
      complete_cases,
    
    `# participants ≥85% complete` =
      participants_85_complete
  )


dashboard_dataset_data_quality <-
  dashboard_dataset_data_quality %>%
  
  filter(
    `# variables observed` > 0
  )


dashboard_dataset_data_quality <-
  dashboard_dataset_data_quality %>%
  
  mutate(
    
    `Data group` = factor(
      `Data group`,
      levels = c(
        "All data",
        "Intake data",
        "Questionnaire data",
        "Total intake variables"
      )
    )
  ) %>%
  
  arrange(
    study,
    Dataset,
    `Data group`
  ) %>%
  
  mutate(
    `Data group` =
      as.character(
        `Data group`
      )
  )

readr::write_csv(
  
  dashboard_dataset_data_quality,
  
  file.path(
    output_dir,
    "dashboard_dataset_data_quality.csv"
  ),
  
  na = ""
)


cat(
  "\nDashboard STUDY-LEVEL 1992_eatdis_deprivation check:\n\n"
)

dashboard_data_quality %>%
  
  filter(
    study ==
      "1992_eatdis_deprivation"
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )

cat(
  "\nDashboard DATASET-LEVEL 1992_eatdis_deprivation check:\n\n"
)

dashboard_dataset_data_quality %>%
  
  filter(
    study ==
      "1992_eatdis_deprivation",
    `Data group` ==
      "All data"
  ) %>%
  
  arrange(
    desc(
      `% missing overall`
    )
  ) %>%
  
  select(
    Dataset,
    `# variables observed`,
    `Total data points`,
    `Total missing`,
    `% missing overall`
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\nScript 08 complete.\n\n",
  "Created:\n",
  "  outputs/dashboard_data_quality.csv\n",
  "  outputs/dashboard_dataset_data_quality.csv\n",
  sep = ""
)