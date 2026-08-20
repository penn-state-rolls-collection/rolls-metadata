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

# Normalize curated directory path once
normalized_curated <- normalizePath(
  curated_dir,
  winslash = "/",
  mustWork = FALSE
)

# Read variable classifications
variable_groups <- read_csv(
  "outputs/variable_groups.csv",
  show_col_types = FALSE
)

# Find all curated CSV files
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
  
  study_folder <- str_split(
    relative_path,
    "/",
    simplify = TRUE
  )[1]
  
  study_folder
}

calculate_group_quality <- function(
    file,
    group_name,
    selected_variables = NULL
) {
  
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
  
  study_folder <- get_study_folder(file)
  
  file_name <- basename(file)

  
  if (is.null(selected_variables)) {
    
    analysis_dat <- dat
    
  } else {
    
    vars_in_file <- intersect(
      selected_variables,
      names(dat)
    )
    
    # No variables from this group in this file
    if (length(vars_in_file) == 0) {
      return(NULL)
    }
    
    analysis_dat <- dat %>%
      select(
        all_of(vars_in_file)
      )
  }

  
  total_cells <- nrow(analysis_dat) * ncol(analysis_dat)
  
  total_missing <- sum(
    is.na(analysis_dat)
  )
  
  percent_missing_overall <- if (total_cells > 0) {
    round(
      100 * total_missing / total_cells,
      2
    )
  } else {
    NA_real_
  }
  
  complete_cases <- sum(
    complete.cases(analysis_dat)
  )
  
  
  if ("id" %in% names(dat)) {
    
    participant_dat <- analysis_dat
    
    participant_dat$id <- dat$id
    
    participant_missing <- participant_dat %>%
      mutate(
        .participant_id = as.character(id)
      ) %>%
      select(
        .participant_id,
        everything(),
        -id
      ) %>%
      mutate(
        .row_missing = rowSums(
          is.na(
            across(
              -.participant_id
            )
          )
        ),
        .row_cells = ncol(.) - 1
      ) %>%
      group_by(
        .participant_id
      ) %>%
      summarise(
        missing_cells = sum(
          .row_missing
        ),
        total_cells = sum(
          .row_cells
        ),
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
    
    participants_85_percent_complete <- sum(
      participant_missing$percent_complete >= 85,
      na.rm = TRUE
    )
    
  } else {
    
    mean_percent_missing_participant <- NA_real_
    
    range_percent_missing_participant <- NA_character_
    
    participants_85_percent_complete <- NA_integer_
  }

  
  tibble(
    study_folder = study_folder,
    file_name = file_name,
    data_group = group_name,
    n_variables = ncol(analysis_dat),
    total_missing = total_missing,
    percent_missing_overall = percent_missing_overall,
    mean_percent_missing_participant =
      mean_percent_missing_participant,
    range_percent_missing_participant =
      range_percent_missing_participant,
    complete_cases = complete_cases,
    participants_85_percent_complete =
      participants_85_percent_complete
  )
}



grouped_quality <- map_dfr(
  data_csvs,
  function(file) {
    
    file_name <- basename(file)
    
    study_folder <- get_study_folder(file)
    
    # Match classifications by BOTH study and filename
    file_variable_groups <- variable_groups %>%
      filter(
        .data$study_folder == study_folder,
        .data$file_name == file_name
      )
    
    intake_vars <- file_variable_groups %>%
      filter(
        variable_group == "intake"
      ) %>%
      pull(
        variable_name
      ) %>%
      unique()
    
    questionnaire_vars <- file_variable_groups %>%
      filter(
        variable_group == "questionnaire"
      ) %>%
      pull(
        variable_name
      ) %>%
      unique()
    
    total_intake_vars <- file_variable_groups %>%
      filter(
        variable_group == "total_intake"
      ) %>%
      pull(
        variable_name
      ) %>%
      unique()
    
    bind_rows(
      
      calculate_group_quality(
        file = file,
        group_name = "All data",
        selected_variables = NULL
      ),
      
      calculate_group_quality(
        file = file,
        group_name = "Intake data",
        selected_variables = intake_vars
      ),
      
      calculate_group_quality(
        file = file,
        group_name = "Questionnaire data",
        selected_variables = questionnaire_vars
      ),
      
      calculate_group_quality(
        file = file,
        group_name = "Total intake variables",
        selected_variables = total_intake_vars
      )
    )
  }
)


grouped_quality <- grouped_quality %>%
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


View(grouped_quality)

write_csv(
  grouped_quality,
  "outputs/grouped_data_quality.csv",
  na = ""
)

grouped_quality %>%
  select(
    study_folder,
    file_name,
    data_group,
    n_variables,
    total_missing,
    percent_missing_overall,
    mean_percent_missing_participant,
    range_percent_missing_participant,
    complete_cases,
    participants_85_percent_complete
  ) %>%
  print(n = 40)