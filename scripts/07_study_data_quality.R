library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)


curated_dir <- Sys.getenv("ROLLS_CURATED_DATA")

if (curated_dir == "") {
  stop("ROLLS_CURATED_DATA has not been set.")
}

if (!dir.exists(curated_dir)) {
  stop("The curated data folder could not be found.")
}

normalized_curated <- normalizePath(
  curated_dir,
  winslash = "/",
  mustWork = FALSE
)

# Read the variable classifications from Script 05
variable_groups <- read_csv(
  "outputs/variable_groups.csv",
  show_col_types = FALSE
)


all_csvs <- list.files(
  path = curated_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

all_csvs <- normalizePath(
  all_csvs,
  winslash = "/",
  mustWork = FALSE
)

data_csvs <- all_csvs[
  str_detect(
    all_csvs,
    regex("/data/", ignore_case = TRUE)
  )
]

if (length(data_csvs) == 0) {
  stop("No CSV files were found inside the study data folders.")
}


get_study_folder <- function(file) {
  
  normalized_file <- normalizePath(
    file,
    winslash = "/",
    mustWork = FALSE
  )
  
  relative_path <- str_remove(
    normalized_file,
    paste0(
      "^",
      fixed(normalized_curated),
      "/?"
    )
  )
  
  str_split(
    relative_path,
    "/",
    simplify = TRUE
  )[1]
}

# Create a file-to-study lookup
study_files <- tibble(
  file = data_csvs,
  study_folder = map_chr(
    data_csvs,
    get_study_folder
  )
)


read_curated_csv <- function(file) {
  
  dat <- read_csv(
    file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character()),
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
  
  dat %>%
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
}


get_file_variable_groups <- function(file, dat) {
  
  current_file <- basename(file)
  
  # Match by filename, then keep only variables that
  # actually occur in this specific file.
  variable_groups %>%
    filter(
      file_name == current_file,
      variable_name %in% names(dat)
    ) %>%
    distinct(
      variable_name,
      variable_group
    )
}


get_selected_variables <- function(
    file,
    dat,
    data_group
) {
  
  if (data_group == "All data") {
    return(names(dat))
  }
  
  current_groups <- get_file_variable_groups(
    file,
    dat
  )
  
  wanted_group <- case_when(
    data_group == "Intake data" ~ "intake",
    data_group == "Questionnaire data" ~ "questionnaire",
    data_group == "Total intake variables" ~ "total_intake",
    TRUE ~ NA_character_
  )
  
  current_groups %>%
    filter(
      variable_group == wanted_group
    ) %>%
    pull(variable_name) %>%
    unique()
}


study_macros_computed <- function(files) {
  
  intake_variable_names <- map(
    files,
    function(file) {
      
      dat <- read_curated_csv(file)
      
      current_groups <- get_file_variable_groups(
        file,
        dat
      )
      
      current_groups %>%
        filter(
          variable_group %in% c(
            "intake",
            "total_intake"
          )
        ) %>%
        pull(variable_name)
    }
  ) %>%
    unlist() %>%
    unique()
  
  has_carbohydrate <- any(
    str_detect(
      intake_variable_names,
      regex(
        "(^|_)(cho|carb|carbohydrate)($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  has_fat <- any(
    str_detect(
      intake_variable_names,
      regex(
        "(^|_)fat($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  has_protein <- any(
    str_detect(
      intake_variable_names,
      regex(
        "(^|_)(pro|protein)($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  if (
    has_carbohydrate &&
    has_fat &&
    has_protein
  ) {
    "Yes"
  } else {
    "No"
  }
}


calculate_study_group_quality <- function(
    study_name,
    files,
    data_group
) {
  
  total_missing <- 0
  total_cells <- 0
  total_variable_occurrences <- 0
  
  participant_results <- list()
  
  participant_counter <- 1
  
  for (file in files) {
    
    dat <- read_curated_csv(file)
    
    selected_variables <- get_selected_variables(
      file = file,
      dat = dat,
      data_group = data_group
    )
    
    # Skip this file if it contributes no variables
    # to the requested category.
    if (length(selected_variables) == 0) {
      next
    }
    
    analysis_dat <- dat %>%
      select(
        all_of(selected_variables)
      )

    
    total_missing <- total_missing +
      sum(is.na(analysis_dat))
    
    total_cells <- total_cells +
      (nrow(analysis_dat) * ncol(analysis_dat))
    
    total_variable_occurrences <-
      total_variable_occurrences +
      length(selected_variables)
    
    
    if ("id" %in% names(dat)) {
      
      # Do not count the participant ID itself as
      # part of participant completeness.
      participant_variables <- setdiff(
        selected_variables,
        "id"
      )
      
      if (length(participant_variables) > 0) {
        
        participant_dat <- dat %>%
          transmute(
            participant_id = as.character(id),
            row_missing = rowSums(
              is.na(
                across(
                  all_of(participant_variables)
                )
              )
            ),
            row_cells = length(
              participant_variables
            )
          ) %>%
          filter(
            !is.na(participant_id),
            nzchar(str_trim(participant_id))
          ) %>%
          group_by(
            participant_id
          ) %>%
          summarise(
            missing_cells = sum(row_missing),
            total_cells = sum(row_cells),
            .groups = "drop"
          )
        
        participant_results[[participant_counter]] <-
          participant_dat
        
        participant_counter <-
          participant_counter + 1
      }
    }
  }
  
  
  if (total_cells == 0) {
    
    return(
      tibble(
        study_folder = study_name,
        data_group = data_group,
        n_variable_occurrences = 0L,
        total_missing = NA_integer_,
        percent_missing_overall = NA_real_,
        mean_percent_missing_participant = NA_real_,
        range_percent_missing_participant = NA_character_,
        complete_cases = NA_integer_,
        participants_85_percent_complete = NA_integer_
      )
    )
  }
  
  
  percent_missing_overall <- round(
    100 * total_missing / total_cells,
    2
  )
  
  if (length(participant_results) > 0) {
    
    participant_summary <- bind_rows(
      participant_results
    ) %>%
      group_by(
        participant_id
      ) %>%
      summarise(
        missing_cells = sum(missing_cells),
        total_cells = sum(total_cells),
        .groups = "drop"
      ) %>%
      mutate(
        percent_missing =
          100 * missing_cells / total_cells,
        
        percent_complete =
          100 - percent_missing
      )
    
    mean_percent_missing_participant <- round(
      mean(
        participant_summary$percent_missing,
        na.rm = TRUE
      ),
      2
    )
    
    missing_range <- range(
      participant_summary$percent_missing,
      na.rm = TRUE
    )
    
    range_percent_missing_participant <- paste0(
      round(
        missing_range[1],
        2
      ),
      "% - ",
      round(
        missing_range[2],
        2
      ),
      "%"
    )
    
    # At the study level, "complete cases"
    # means participants with 100% complete data.
    complete_cases <- sum(
      participant_summary$percent_missing == 0,
      na.rm = TRUE
    )
    
    participants_85_percent_complete <- sum(
      participant_summary$percent_complete >= 85,
      na.rm = TRUE
    )
    
  } else {
    
    mean_percent_missing_participant <- NA_real_
    
    range_percent_missing_participant <- NA_character_
    
    complete_cases <- NA_integer_
    
    participants_85_percent_complete <- NA_integer_
  }
  
  tibble(
    study_folder = study_name,
    data_group = data_group,
    n_variable_occurrences =
      total_variable_occurrences,
    total_missing = total_missing,
    percent_missing_overall =
      percent_missing_overall,
    mean_percent_missing_participant =
      mean_percent_missing_participant,
    range_percent_missing_participant =
      range_percent_missing_participant,
    complete_cases = complete_cases,
    participants_85_percent_complete =
      participants_85_percent_complete
  )
}


data_groups <- c(
  "All data",
  "Intake data",
  "Questionnaire data",
  "Total intake variables"
)

study_data_quality <- map_dfr(
  unique(study_files$study_folder),
  function(study_name) {
    
    files <- study_files %>%
      filter(
        study_folder == study_name
      ) %>%
      pull(file)
    
    macros <- study_macros_computed(files)
    
    study_results <- map_dfr(
      data_groups,
      function(group_name) {
        
        calculate_study_group_quality(
          study_name = study_name,
          files = files,
          data_group = group_name
        )
      }
    )
    
    study_results %>%
      mutate(
        intake_macros_computed = macros
      )
  }
)


study_data_quality <- study_data_quality %>%
  mutate(
    data_group = factor(
      data_group,
      levels = c(
        "All data",
        "Intake data",
        "Questionnaire data",
        "Total intake variables"
      )
    )
  ) %>%
  arrange(
    study_folder,
    data_group
  ) %>%
  mutate(
    data_group = as.character(
      data_group
    )
  )


View(study_data_quality)


write_csv(
  study_data_quality,
  "outputs/study_data_quality.csv",
  na = ""
)


study_data_quality %>%
  print(
    n = Inf,
    width = Inf
  )