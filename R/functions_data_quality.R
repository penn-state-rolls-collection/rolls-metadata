library(dplyr)
library(readr)
library(stringr)
library(tibble)

calculate_data_quality <- function(file) {
  
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
  
  study_folder <- basename(
    dirname(
      dirname(
        normalizePath(
          file,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  )
  
  # -------------------------
  # OVERALL DATASET MISSINGNESS
  # -------------------------
  
  total_cells <- nrow(dat) * ncol(dat)
  
  total_missing <- sum(is.na(dat))
  
  percent_missing_overall <- if (total_cells > 0) {
    round(
      100 * total_missing / total_cells,
      2
    )
  } else {
    NA_real_
  }
  
  complete_cases <- sum(
    complete.cases(dat)
  )
  
  # -------------------------
  # PARTICIPANT-LEVEL MISSINGNESS
  # -------------------------
  
  if ("id" %in% names(dat)) {
    
    participant_missing <- dat %>%
      mutate(
        .participant_id = as.character(id),
        .row_missing = rowSums(
          is.na(across(everything()))
        ),
        .row_cells = ncol(dat)
      ) %>%
      group_by(.participant_id) %>%
      summarise(
        missing_cells = sum(.row_missing),
        total_cells = sum(.row_cells),
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
      round(participant_range[1], 2),
      "% - ",
      round(participant_range[2], 2),
      "%"
    )
    
    participants_85_percent_complete <- sum(
      participant_missing$percent_complete >= 85,
      na.rm = TRUE
    )
    
  } else {
    
    mean_percent_missing_participant <- NA_real_
    
    range_percent_missing_participant <- NA_character_
    
    participants_85_percent_complete <- NA_integer_
  }
  
  # -------------------------
  # MACRONUTRIENTS COMPUTED
  # -------------------------
  
  variable_names <- names(dat)
  
  carb_present <- any(
    str_detect(
      variable_names,
      regex(
        "(^|_)(cho|carb|carbohydrate)($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  fat_present <- any(
    str_detect(
      variable_names,
      regex(
        "(^|_)fat($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  protein_present <- any(
    str_detect(
      variable_names,
      regex(
        "(^|_)(pro|protein)($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  macros_computed <- if (
    carb_present &&
    fat_present &&
    protein_present
  ) {
    "Yes"
  } else {
    "No"
  }
  
  # -------------------------
  # OUTPUT
  # -------------------------
  
  tibble(
    study_folder = study_folder,
    file_name = basename(file),
    n_rows = nrow(dat),
    n_variables = ncol(dat),
    total_missing = total_missing,
    percent_missing_overall = percent_missing_overall,
    mean_percent_missing_participant =
      mean_percent_missing_participant,
    range_percent_missing_participant =
      range_percent_missing_participant,
    complete_cases = complete_cases,
    participants_85_percent_complete =
      participants_85_percent_complete,
    intake_macros_computed = macros_computed
  )
}