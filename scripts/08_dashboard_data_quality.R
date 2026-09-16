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
  "percent_data_present",
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
  "n_variables",
  "total_data_points",
  "total_missing",
  "percent_missing_overall",
  "percent_data_present",
  "mean_percent_missing_participant",
  "range_percent_missing_participant",
  "complete_cases",
  "participants_85_percent_complete"
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


grouped_data_quality <- grouped_data_quality %>%
  
  filter(
    !(
      study_folder == "1992_eatdis_deprivation" &
        str_detect(
          file_name,
          regex(
            "mealtime",
            ignore_case = TRUE
          )
        )
    )
  )

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
    
    `% data present` =
      percent_data_present,
    
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


study_group_order <- c(
  "All data",
  "Measured intake",
  "Macronutrient intake",
  "Total intake",
  "Questionnaire data"
)


dashboard_data_quality <- dashboard_data_quality %>%
  
  mutate(
    
    `Data group` = factor(
      `Data group`,
      levels = study_group_order
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
  
  mutate(
    
    dashboard_group = case_when(
      
      data_group == "Intake variables" ~
        "Measured intake",
      
      data_group == "Macronutrient data" ~
        "Macronutrient intake",
      
      data_group == "Questionnaire data" ~
        "Questionnaire data",
      
      data_group == "Other data" ~
        "Other data",
      
      TRUE ~
        data_group
    )
  ) %>%
  
  transmute(
    
    study =
      study_folder,
    
    Dataset =
      file_name,
    
    `Data group` =
      dashboard_group,
    
    `# variables observed` =
      n_variables,
    
    `Total data points` =
      total_data_points,
    
    `Total missing` =
      total_missing,
    
    `% missing overall` =
      percent_missing_overall,
    
    `% data present` =
      percent_data_present,
    
    `Mean % missing by participant` =
      mean_percent_missing_participant,
    
    `Range of % missing by participant` =
      range_percent_missing_participant,
    
    `# complete cases` =
      complete_cases,
    
    `# participants ≥85% complete` =
      participants_85_percent_complete
  )



dashboard_dataset_data_quality <-
  dashboard_dataset_data_quality %>%
  
  filter(
    !is.na(
      `# variables observed`
    ),
    `# variables observed` > 0
  )



dataset_group_order <- c(
  "Measured intake",
  "Macronutrient intake",
  "Questionnaire data",
  "Other data"
)


dashboard_dataset_data_quality <-
  dashboard_dataset_data_quality %>%
  
  mutate(
    
    `Data group` = factor(
      `Data group`,
      levels = dataset_group_order
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




mealtime_final <- dashboard_dataset_data_quality %>%
  
  filter(
    
    study ==
      "1992_eatdis_deprivation",
    
    str_detect(
      Dataset,
      regex(
        "mealtime",
        ignore_case = TRUE
      )
    )
  )


if (nrow(mealtime_final) == 0) {
  
  cat(
    "GOOD: 1992 EatDis mealtime dataset is absent from final dashboard data.\n"
  )
  
} else {
  
  cat(
    "WARNING: 1992 EatDis mealtime dataset is still present.\n"
  )
}



get_dashboard_count <- function(
    study_name,
    group_name
) {
  
  result <- dashboard_data_quality %>%
    
    filter(
      study == study_name,
      `Data group` == group_name
    )
  
  
  if (nrow(result) != 1) {
    return(
      NA_integer_
    )
  }
  
  
  result$`# variables observed`[1]
}


check_dashboard_group <- function(
    study_name,
    group_name,
    display_name
) {
  
  result <- dashboard_data_quality %>%
    
    filter(
      study == study_name,
      `Data group` == group_name
    )
  
  
  if (
    nrow(result) == 1 &&
    !is.na(
      result$`# variables observed`[1]
    ) &&
    result$`# variables observed`[1] > 0 &&
    !is.na(
      result$`Total data points`[1]
    )
  ) {
    
    cat(
      "GOOD: ",
      display_name,
      " - ",
      group_name,
      " = ",
      result$`# variables observed`[1],
      " variables and ",
      result$`Total data points`[1],
      " data points.\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "WARNING: ",
      display_name,
      " - ",
      group_name,
      " is zero or NA.\n",
      sep = ""
    )
  }
}

check_dashboard_total_intake <- function(
    study_name,
    display_name
) {
  
  measured_n <- get_dashboard_count(
    study_name,
    "Measured intake"
  )
  
  
  macro_n <- get_dashboard_count(
    study_name,
    "Macronutrient intake"
  )
  
  
  total_n <- get_dashboard_count(
    study_name,
    "Total intake"
  )
  
  
  if (
    !is.na(measured_n) &&
    !is.na(macro_n) &&
    !is.na(total_n) &&
    total_n ==
    measured_n +
    macro_n
  ) {
    
    cat(
      "GOOD: ",
      display_name,
      " total intake = measured + macronutrient (",
      measured_n,
      " + ",
      macro_n,
      " = ",
      total_n,
      ").\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "WARNING: ",
      display_name,
      " total intake does not equal measured + macronutrient.\n",
      sep = ""
    )
  }
}


check_dashboard_group(
  "1991_eatdis",
  "Measured intake",
  "1991 EatDis"
)


check_dashboard_group(
  "1991_eatdis",
  "Macronutrient intake",
  "1991 EatDis"
)


check_dashboard_group(
  "1991_eatdis",
  "Total intake",
  "1991 EatDis"
)


check_dashboard_total_intake(
  "1991_eatdis",
  "1991 EatDis"
)




check_dashboard_group(
  "1991_ivig_preload",
  "Measured intake",
  "1991 IVIG preload"
)


check_dashboard_group(
  "1991_ivig_preload",
  "Macronutrient intake",
  "1991 IVIG preload"
)


check_dashboard_group(
  "1991_ivig_preload",
  "Total intake",
  "1991 IVIG preload"
)


check_dashboard_total_intake(
  "1991_ivig_preload",
  "1991 IVIG preload"
)



ivig_measured <- get_dashboard_count(
  "1991_ivig_preload",
  "Measured intake"
)

ivig_macro <- get_dashboard_count(
  "1991_ivig_preload",
  "Macronutrient intake"
)

ivig_total <- get_dashboard_count(
  "1991_ivig_preload",
  "Total intake"
)


if (
  !is.na(ivig_measured) &&
  !is.na(ivig_macro) &&
  !is.na(ivig_total) &&
  ivig_measured == 60 &&
  ivig_macro == 102 &&
  ivig_total == 162
) {
  
  cat(
    "GOOD: IVIG counts are exactly 60 measured + 102 macronutrient = 162 total.\n"
  )
  
} else {
  
  cat(
    "WARNING: IVIG counts differ from the verified 60/102/162 counts.\n"
  )
}





dashboard_data_quality %>%
  
  filter(
    study %in% c(
      "1991_eatdis",
      "1991_ivig_preload",
      "1992_eatdis_deprivation"
    )
  ) %>%
  
  select(
    study,
    `Data group`,
    `# variables observed`,
    `Total data points`,
    `Total missing`,
    `% missing overall`,
    `% data present`
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