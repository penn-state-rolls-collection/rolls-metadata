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
  filter(
    data_group %in% c(
      "All data",
      "Demographic data",
      "Measured intake",
      "Macronutrient intake",
      "Questionnaire data"
    )
  ) %>%
  transmute(
    study = study_folder,
    `Data group` = data_group,
    `# variables observed` = n_variables_observed,
    `Total data points` = total_data_points,
    `Total missing` = total_missing,
    `% missing overall` = percent_missing_overall,
    `% data present` = percent_data_present,
    `Mean % missing by participant` =
      mean_percent_missing_participant,
    `Range of % missing by participant` =
      range_percent_missing_participant,
    `# complete cases` = complete_cases,
    `# participants ≥85% complete` =
      participants_85_percent_complete,
    `Intake macros computed` =
      intake_macros_computed
  )

study_group_order <- c(
  "All data",
  "Demographic data",
  "Measured intake",
  "Macronutrient intake",
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
    `Data group` = as.character(
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

curated_root <- Sys.getenv("ROLLS_CURATED_DATA")

if (curated_root == "") {
  stop(
    "ROLLS_CURATED_DATA is not set. ",
    "Check the .Renviron file and restart R."
  )
}

if (!dir.exists(curated_root)) {
  stop(
    "ROLLS_CURATED_DATA does not point to an existing folder: ",
    curated_root
  )
}

data_csvs <- list.files(
  path = curated_root,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

data_csvs <- data_csvs[
  grepl(
    "[/\\\\]data[/\\\\]",
    data_csvs,
    ignore.case = TRUE
  )
]

data_csvs <- data_csvs[
  !(
    grepl(
      "1992_eatdis_deprivation",
      data_csvs,
      ignore.case = TRUE
    ) &
      grepl(
        "mealtime",
        basename(data_csvs),
        ignore.case = TRUE
      )
  )
]

normalized_root <- normalizePath(
  curated_root,
  winslash = "/",
  mustWork = FALSE
)

get_study_folder <- function(file_path) {
  normalized_file <- normalizePath(
    file_path,
    winslash = "/",
    mustWork = FALSE
  )
  
  relative_path <- str_remove(
    normalized_file,
    fixed(
      paste0(
        normalized_root,
        "/"
      )
    )
  )
  
  pieces <- str_split(
    relative_path,
    "/"
  )[[1]]
  
  pieces[1]
}

file_lookup <- tibble(
  file_path = data_csvs,
  study = map_chr(
    data_csvs,
    get_study_folder
  ),
  Dataset = basename(
    data_csvs
  )
)

read_quality_data <- function(file_path) {
  dat <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    na = c(
      "",
      "NA",
      "N/A",
      "n/a",
      "na",
      "NULL",
      "null",
      "."
    ),
    trim_ws = TRUE
  )
  
  dat <- dat %>%
    mutate(
      across(
        everything(),
        ~ {
          x <- str_trim(.x)
          x[x == ""] <- NA_character_
          x
        }
      )
    )
  
  dat
}

calculate_dataset_quality <- function(
    file_path,
    study_name,
    dataset_name
) {
  dat <- tryCatch(
    read_quality_data(
      file_path
    ),
    error = function(e) {
      warning(
        "Could not read ",
        file_path,
        ": ",
        conditionMessage(e)
      )
      NULL
    }
  )
  
  if (is.null(dat)) {
    return(NULL)
  }
  
  has_observed_value <- vapply(
    dat,
    function(x) {
      any(
        !is.na(x)
      )
    },
    logical(1)
  )
  
  observed_variables <- names(dat)[
    has_observed_value
  ]
  
  if (length(observed_variables) == 0) {
    return(NULL)
  }
  
  selected_data <- dat[
    ,
    observed_variables,
    drop = FALSE
  ]
  
  total_data_points <-
    nrow(selected_data) *
    ncol(selected_data)
  
  total_missing <- sum(
    is.na(
      selected_data
    )
  )
  
  percent_missing_overall <- if (
    total_data_points > 0
  ) {
    round(
      100 *
        total_missing /
        total_data_points,
      2
    )
  } else {
    NA_real_
  }
  
  percent_data_present <- if (
    is.na(
      percent_missing_overall
    )
  ) {
    NA_real_
  } else {
    round(
      100 -
        percent_missing_overall,
      2
    )
  }
  
  mean_percent_missing_participant <- NA_real_
  range_percent_missing_participant <- NA_character_
  complete_cases <- NA_integer_
  participants_85_percent_complete <- NA_integer_
  
  id_variable <- intersect(
    c(
      "id",
      "ID",
      "participant_id",
      "participantid",
      "subject_id",
      "subjectid"
    ),
    names(dat)
  )
  
  if (length(id_variable) > 0) {
    id_variable <- id_variable[1]
    
    participant_variables <- setdiff(
      observed_variables,
      id_variable
    )
    
    if (length(participant_variables) > 0) {
      participant_missing <- tibble(
        participant_id = as.character(
          dat[[id_variable]]
        ),
        missing_cells = rowSums(
          is.na(
            dat[
              ,
              participant_variables,
              drop = FALSE
            ]
          )
        ),
        total_cells = rep(
          length(
            participant_variables
          ),
          nrow(dat)
        )
      ) %>%
        filter(
          !is.na(
            participant_id
          ),
          nzchar(
            str_trim(
              participant_id
            )
          )
        ) %>%
        group_by(
          participant_id
        ) %>%
        summarise(
          missing_cells =
            sum(
              missing_cells
            ),
          total_cells =
            sum(
              total_cells
            ),
          .groups = "drop"
        ) %>%
        filter(
          total_cells > 0
        ) %>%
        mutate(
          percent_missing =
            100 *
            missing_cells /
            total_cells,
          percent_complete =
            100 -
            percent_missing
        )
      
      if (nrow(participant_missing) > 0) {
        mean_percent_missing_participant <- round(
          mean(
            participant_missing$percent_missing,
            na.rm = TRUE
          ),
          2
        )
        
        participant_range <- range(
          participant_missing$percent_missing,
          na.rm = TRUE
        )
        
        range_percent_missing_participant <- paste0(
          round(
            participant_range[1],
            2
          ),
          "% - ",
          round(
            participant_range[2],
            2
          ),
          "%"
        )
        
        complete_cases <- sum(
          participant_missing$percent_missing == 0,
          na.rm = TRUE
        )
        
        participants_85_percent_complete <- sum(
          participant_missing$percent_complete >= 85,
          na.rm = TRUE
        )
      }
    }
  }
  
  tibble(
    study = study_name,
    Dataset = dataset_name,
    `# variables observed` =
      as.integer(
        length(
          observed_variables
        )
      ),
    `Total data points` =
      as.numeric(
        total_data_points
      ),
    `Total missing` =
      as.numeric(
        total_missing
      ),
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
}

dataset_results <- list()

dataset_counter <- 1L

for (
  i in seq_len(
    nrow(
      file_lookup
    )
  )
) {
  current_result <- calculate_dataset_quality(
    file_path =
      file_lookup$file_path[i],
    study_name =
      file_lookup$study[i],
    dataset_name =
      file_lookup$Dataset[i]
  )
  
  if (!is.null(current_result)) {
    dataset_results[[dataset_counter]] <-
      current_result
    
    dataset_counter <-
      dataset_counter + 1L
  }
}

if (length(dataset_results) == 0) {
  stop(
    "No dataset-level data-quality results were created."
  )
}

dashboard_dataset_data_quality <- bind_rows(
  dataset_results
) %>%
  arrange(
    study,
    Dataset
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

total_intake_check <- dashboard_data_quality %>%
  filter(
    `Data group` ==
      "Total intake"
  )

if (nrow(total_intake_check) == 0) {
  cat(
    "GOOD: Total intake is absent from the final study-level dashboard data.\n"
  )
} else {
  cat(
    "WARNING: Total intake is still present in final study-level dashboard data.\n"
  )
}

demographic_check <- dashboard_data_quality %>%
  filter(
    `Data group` ==
      "Demographic data",
    `# variables observed` > 0
  )

if (nrow(demographic_check) > 0) {
  cat(
    "GOOD: Demographic data are present in final study-level dashboard data.\n"
  )
} else {
  cat(
    "WARNING: Demographic data are missing from final study-level dashboard data.\n"
  )
}

expected_study_groups <- c(
  "All data",
  "Demographic data",
  "Measured intake",
  "Macronutrient intake",
  "Questionnaire data"
)

unexpected_study_groups <- dashboard_data_quality %>%
  filter(
    !`Data group` %in%
      expected_study_groups
  )

if (nrow(unexpected_study_groups) == 0) {
  cat(
    "GOOD: Final study-level dashboard data contain only the five expected groups.\n"
  )
} else {
  cat(
    "WARNING: Unexpected study-level data groups are present.\n"
  )
}

study_row_check <- dashboard_data_quality %>%
  count(
    study,
    name = "n_rows"
  )

if (
  nrow(study_row_check) > 0 &&
  all(
    study_row_check$n_rows ==
    5
  )
) {
  cat(
    "GOOD: Every study has exactly five rows in final study-level dashboard data.\n"
  )
} else {
  cat(
    "WARNING: One or more studies do not have exactly five study-level rows.\n"
  )
}

dataset_duplicate_check <- dashboard_dataset_data_quality %>%
  count(
    study,
    Dataset,
    name = "n_rows"
  ) %>%
  filter(
    n_rows != 1
  )

if (nrow(dataset_duplicate_check) == 0) {
  cat(
    "GOOD: Dataset-level detail has exactly one row per dataset.\n"
  )
} else {
  cat(
    "WARNING: Some datasets have more than one row in dataset-level detail.\n"
  )
}

dataset_group_column_check <-
  "Data group" %in%
  names(
    dashboard_dataset_data_quality
  )

if (!dataset_group_column_check) {
  cat(
    "GOOD: Data group column is absent from dataset-level detail.\n"
  )
} else {
  cat(
    "WARNING: Data group column is still present in dataset-level detail.\n"
  )
}

ivig_measured <- dashboard_data_quality %>%
  filter(
    study ==
      "1991_ivig_preload",
    `Data group` ==
      "Measured intake"
  ) %>%
  pull(
    `# variables observed`
  )

ivig_macro <- dashboard_data_quality %>%
  filter(
    study ==
      "1991_ivig_preload",
    `Data group` ==
      "Macronutrient intake"
  ) %>%
  pull(
    `# variables observed`
  )

if (
  length(ivig_measured) == 1 &&
  length(ivig_macro) == 1 &&
  !is.na(ivig_measured) &&
  !is.na(ivig_macro) &&
  ivig_measured == 60 &&
  ivig_macro == 102
) {
  cat(
    "GOOD: IVIG remains at 60 measured intake and 102 macronutrient variables.\n"
  )
} else {
  cat(
    "WARNING: IVIG measured/macronutrient counts changed from the verified 60/102 values.\n"
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

dashboard_dataset_data_quality %>%
  filter(
    study %in% c(
      "1991_eatdis",
      "1991_ivig_preload",
      "1992_eatdis_deprivation"
    )
  ) %>%
  select(
    study,
    Dataset,
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
  "Script 08 complete.\n",
  "Created: outputs/dashboard_data_quality.csv\n",
  "Created: outputs/dashboard_dataset_data_quality.csv\n",
  sep = ""
)