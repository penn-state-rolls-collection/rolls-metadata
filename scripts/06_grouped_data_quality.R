library(tidyverse)

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

output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

variable_groups_path <- file.path(
  output_dir,
  "variable_groups.csv"
)

if (!file.exists(variable_groups_path)) {
  stop(
    "outputs/variable_groups.csv was not found. ",
    "Run Script 05 first."
  )
}

variable_groups <- readr::read_csv(
  variable_groups_path,
  show_col_types = FALSE
)


data_csvs <- list.files(
  path = curated_root,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Only keep CSV files inside a data folder.
data_csvs <- data_csvs[
  grepl(
    "[/\\\\]data[/\\\\]",
    data_csvs,
    ignore.case = TRUE
  )
]

if (length(data_csvs) == 0) {
  stop(
    "No CSV files were found inside data folders under ",
    curated_root
  )
}

get_study_folder <- function(file_path, root_path) {
  
  root_normalized <- normalizePath(
    root_path,
    winslash = "/",
    mustWork = FALSE
  )
  
  file_normalized <- normalizePath(
    file_path,
    winslash = "/",
    mustWork = FALSE
  )
  
  relative_path <- sub(
    paste0(
      "^",
      stringr::str_replace_all(
        root_normalized,
        "([.()+^$|{}\\[\\]\\\\])",
        "\\\\\\1"
      ),
      "/?"
    ),
    "",
    file_normalized
  )
  
  strsplit(relative_path, "/", fixed = TRUE)[[1]][1]
}


read_data_file <- function(file_path) {
  
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
          x <- stringr::str_trim(.x)
          x[x == ""] <- NA_character_
          x
        }
      )
    )
  
  dat
}

calculate_quality <- function(dat) {
  
  # Number of variables originally supplied to this function.
  n_variables_source <- ncol(dat)
  
  # No variables at all.
  if (n_variables_source == 0) {
    
    return(
      tibble(
        n_variables_source = 0L,
        n_variables_observed = 0L,
        total_data_points = NA_integer_,
        total_missing = NA_integer_,
        percent_missing_overall = NA_real_,
        mean_percent_missing_participant = NA_real_,
        min_percent_missing_participant = NA_real_,
        max_percent_missing_participant = NA_real_,
        complete_cases = NA_integer_,
        participants_85_complete = NA_integer_
      )
    )
  }
  
  # Determine which variables contain at least one
  # observed value.
  observed_variable <- vapply(
    dat,
    function(x) any(!is.na(x)),
    logical(1)
  )
  
  # Retain only variables that actually contain data.
  dat_observed <- dat[
    ,
    observed_variable,
    drop = FALSE
  ]
  
  n_variables_observed <- ncol(dat_observed)
  
  # If every variable in the requested group is completely
  # empty, there is no meaningful denominator.
  if (n_variables_observed == 0) {
    
    return(
      tibble(
        n_variables_source = n_variables_source,
        n_variables_observed = 0L,
        total_data_points = NA_integer_,
        total_missing = NA_integer_,
        percent_missing_overall = NA_real_,
        mean_percent_missing_participant = NA_real_,
        min_percent_missing_participant = NA_real_,
        max_percent_missing_participant = NA_real_,
        complete_cases = NA_integer_,
        participants_85_complete = NA_integer_
      )
    )
  }
  
  total_data_points <-
    nrow(dat_observed) * ncol(dat_observed)
  
  total_missing <-
    sum(is.na(dat_observed))
  
  percent_missing_overall <-
    ifelse(
      total_data_points > 0,
      100 * total_missing / total_data_points,
      NA_real_
    )
  
  if (nrow(dat_observed) > 0) {
    
    missing_per_participant <-
      rowSums(is.na(dat_observed))
    
    percent_missing_participant <-
      100 *
      missing_per_participant /
      ncol(dat_observed)
    
    percent_complete_participant <-
      100 - percent_missing_participant
    
    mean_percent_missing_participant <-
      mean(percent_missing_participant)
    
    min_percent_missing_participant <-
      min(percent_missing_participant)
    
    max_percent_missing_participant <-
      max(percent_missing_participant)
    
    complete_cases <-
      sum(missing_per_participant == 0)
    
    participants_85_complete <-
      sum(percent_complete_participant >= 85)
    
  } else {
    
    mean_percent_missing_participant <- NA_real_
    min_percent_missing_participant <- NA_real_
    max_percent_missing_participant <- NA_real_
    complete_cases <- NA_integer_
    participants_85_complete <- NA_integer_
  }
  
  tibble(
    n_variables_source =
      as.integer(n_variables_source),
    
    n_variables_observed =
      as.integer(n_variables_observed),
    
    total_data_points =
      as.integer(total_data_points),
    
    total_missing =
      as.integer(total_missing),
    
    percent_missing_overall =
      round(percent_missing_overall, 2),
    
    mean_percent_missing_participant =
      round(mean_percent_missing_participant, 2),
    
    min_percent_missing_participant =
      round(min_percent_missing_participant, 2),
    
    max_percent_missing_participant =
      round(max_percent_missing_participant, 2),
    
    complete_cases =
      as.integer(complete_cases),
    
    participants_85_complete =
      as.integer(participants_85_complete)
  )
}


calculate_group_quality <- function(
    dat,
    variable_names,
    study_folder,
    file_name,
    data_group
) {
  
  # Keep only variables that actually exist in this CSV.
  variable_names <- intersect(
    variable_names,
    names(dat)
  )
  
  # If the group does not exist in this dataset, return
  # an NA row rather than pretending it has zero missingness.
  if (length(variable_names) == 0) {
    
    return(
      tibble(
        study_folder = study_folder,
        file_name = file_name,
        data_group = data_group,
        n_variables_source = 0L,
        n_variables_observed = 0L,
        total_data_points = NA_integer_,
        total_missing = NA_integer_,
        percent_missing_overall = NA_real_,
        mean_percent_missing_participant = NA_real_,
        min_percent_missing_participant = NA_real_,
        max_percent_missing_participant = NA_real_,
        complete_cases = NA_integer_,
        participants_85_complete = NA_integer_
      )
    )
  }
  
  group_data <- dat %>%
    select(all_of(variable_names))
  
  calculate_quality(group_data) %>%
    mutate(
      study_folder = study_folder,
      file_name = file_name,
      data_group = data_group,
      .before = 1
    )
}


quality_results <- vector(
  mode = "list",
  length = length(data_csvs)
)


for (i in seq_along(data_csvs)) {
  
  file_path <- data_csvs[i]
  
  study_folder <- get_study_folder(
    file_path,
    curated_root
  )
  
  file_name <- basename(file_path)
  
  message(
    "[",
    i,
    "/",
    length(data_csvs),
    "] ",
    study_folder,
    " / ",
    file_name
  )
  
  
  dat <- tryCatch(
    read_data_file(file_path),
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
    next
  }
  
  
  file_variable_groups <- variable_groups %>%
    filter(
      .data$study_folder == study_folder,
      .data$file_name == file_name
    )
  
  
  all_result <- calculate_group_quality(
    dat = dat,
    variable_names = names(dat),
    study_folder = study_folder,
    file_name = file_name,
    data_group = "All data"
  )
  
  
  intake_variables <- file_variable_groups %>%
    filter(variable_group == "intake") %>%
    pull(variable_name) %>%
    unique()
  
  intake_result <- calculate_group_quality(
    dat = dat,
    variable_names = intake_variables,
    study_folder = study_folder,
    file_name = file_name,
    data_group = "Intake data"
  )
  
  
  questionnaire_variables <- file_variable_groups %>%
    filter(variable_group == "questionnaire") %>%
    pull(variable_name) %>%
    unique()
  
  questionnaire_result <- calculate_group_quality(
    dat = dat,
    variable_names = questionnaire_variables,
    study_folder = study_folder,
    file_name = file_name,
    data_group = "Questionnaire data"
  )
  
  
  total_intake_variables <- file_variable_groups %>%
    filter(variable_group == "total_intake") %>%
    pull(variable_name) %>%
    unique()
  
  total_intake_result <- calculate_group_quality(
    dat = dat,
    variable_names = total_intake_variables,
    study_folder = study_folder,
    file_name = file_name,
    data_group = "Total intake variables"
  )

  
  quality_results[[i]] <- bind_rows(
    all_result,
    intake_result,
    questionnaire_result,
    total_intake_result
  )
}

grouped_quality <- bind_rows(
  quality_results
)

grouped_quality <- grouped_quality %>%
  mutate(
    percent_missing_range = case_when(
      
      is.na(min_percent_missing_participant) |
        is.na(max_percent_missing_participant) ~ NA_character_,
      
      TRUE ~ paste0(
        format(
          round(
            min_percent_missing_participant,
            2
          ),
          trim = TRUE
        ),
        "% - ",
        format(
          round(
            max_percent_missing_participant,
            2
          ),
          trim = TRUE
        ),
        "%"
      )
    )
  )

grouped_quality <- grouped_quality %>%
  select(
    study_folder,
    file_name,
    data_group,
    
    # Useful for diagnosing structural empty variables.
    n_variables_source,
    
    # Number actually used in quality calculations.
    n_variables_observed,
    
    # Denominator first.
    total_data_points,
    
    # Numerator second.
    total_missing,
    
    percent_missing_overall,
    mean_percent_missing_participant,
    percent_missing_range,
    complete_cases,
    participants_85_complete,
    
    # Keep underlying range components for later aggregation.
    min_percent_missing_participant,
    max_percent_missing_participant
  ) %>%
  arrange(
    study_folder,
    file_name,
    factor(
      data_group,
      levels = c(
        "All data",
        "Intake data",
        "Questionnaire data",
        "Total intake variables"
      )
    )
  )


readr::write_csv(
  grouped_quality,
  file.path(
    output_dir,
    "grouped_data_quality.csv"
  ),
  na = ""
)

cat(
  "\nScript 06 complete.\n",
  "Created: outputs/grouped_data_quality.csv\n\n",
  sep = ""
)

cat(
  "\n1992_eatdis_deprivation dataset-level check:\n\n"
)

grouped_quality %>%
  filter(
    study_folder == "1992_eatdis_deprivation",
    data_group == "All data"
  ) %>%
  arrange(
    desc(percent_missing_overall)
  ) %>%
  select(
    file_name,
    n_variables_source,
    n_variables_observed,
    total_data_points,
    total_missing,
    percent_missing_overall
  ) %>%
  print(n = Inf)

cat(
  "\nFiles containing completely empty variables:\n\n"
)

grouped_quality %>%
  filter(
    data_group == "All data",
    n_variables_source > n_variables_observed
  ) %>%
  mutate(
    n_completely_empty_variables =
      n_variables_source - n_variables_observed
  ) %>%
  select(
    study_folder,
    file_name,
    n_variables_source,
    n_variables_observed,
    n_completely_empty_variables,
    percent_missing_overall
  ) %>%
  arrange(
    study_folder,
    file_name
  ) %>%
  print(n = Inf)