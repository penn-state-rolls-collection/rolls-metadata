library(tidyverse)

output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

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

if (length(data_csvs) == 0) {
  stop(
    "No CSV files were found inside the curated study data folders."
  )
}

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

read_variable_data <- function(file_path) {
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

determine_variable_type <- function(x) {
  observed <- x[
    !is.na(x)
  ]
  
  if (length(observed) == 0) {
    return(
      "Empty"
    )
  }
  
  numeric_values <- suppressWarnings(
    as.numeric(
      observed
    )
  )
  
  percent_numeric <- mean(
    !is.na(
      numeric_values
    )
  )
  
  if (percent_numeric >= 0.95) {
    return(
      "Numeric"
    )
  }
  
  unique_values <- length(
    unique(
      observed
    )
  )
  
  if (unique_values <= 20) {
    return(
      "Categorical"
    )
  }
  
  return(
    "Character"
  )
}

calculate_variable_completeness <- function(
    file_path,
    study_name,
    dataset_name
) {
  dat <- tryCatch(
    read_variable_data(
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
    return(
      NULL
    )
  }
  
  if (ncol(dat) == 0) {
    return(
      NULL
    )
  }
  
  variable_results <- vector(
    "list",
    length(
      names(dat)
    )
  )
  
  for (
    j in seq_along(
      names(dat)
    )
  ) {
    variable_name <- names(dat)[j]
    
    x <- dat[[variable_name]]
    
    total_observations <- length(
      x
    )
    
    complete_observations <- sum(
      !is.na(
        x
      )
    )
    
    missing_observations <- sum(
      is.na(
        x
      )
    )
    
    percent_complete <- if (
      total_observations > 0
    ) {
      round(
        100 *
          complete_observations /
          total_observations,
        2
      )
    } else {
      NA_real_
    }
    
    percent_missing <- if (
      total_observations > 0
    ) {
      round(
        100 *
          missing_observations /
          total_observations,
        2
      )
    } else {
      NA_real_
    }
    
    variable_results[[j]] <- tibble(
      study = study_name,
      Dataset = dataset_name,
      Variable = variable_name,
      Type = determine_variable_type(
        x
      ),
      `Total observations` =
        as.integer(
          total_observations
        ),
      `Complete observations` =
        as.integer(
          complete_observations
        ),
      `Missing observations` =
        as.integer(
          missing_observations
        ),
      `% complete` =
        percent_complete,
      `% missing` =
        percent_missing
    )
  }
  
  bind_rows(
    variable_results
  )
}

variable_results <- list()

variable_counter <- 1L

for (
  i in seq_len(
    nrow(
      file_lookup
    )
  )
) {
  current_result <- calculate_variable_completeness(
    file_path =
      file_lookup$file_path[i],
    study_name =
      file_lookup$study[i],
    dataset_name =
      file_lookup$Dataset[i]
  )
  
  if (!is.null(current_result)) {
    variable_results[[variable_counter]] <-
      current_result
    
    variable_counter <-
      variable_counter + 1L
  }
}

if (length(variable_results) == 0) {
  stop(
    "No variable-level completeness results were created."
  )
}

dashboard_variable_completeness <- bind_rows(
  variable_results
) %>%
  arrange(
    study,
    Dataset,
    Variable
  )

duplicate_check <- dashboard_variable_completeness %>%
  count(
    study,
    Dataset,
    Variable,
    name = "n_rows"
  ) %>%
  filter(
    n_rows != 1
  )

if (nrow(duplicate_check) == 0) {
  cat(
    "GOOD: Every study/dataset/variable combination has exactly one row.\n"
  )
} else {
  cat(
    "WARNING: Duplicate study/dataset/variable rows were detected.\n"
  )
}

calculation_check <- dashboard_variable_completeness %>%
  filter(
    `Complete observations` +
      `Missing observations` !=
      `Total observations`
  )

if (nrow(calculation_check) == 0) {
  cat(
    "GOOD: Complete + missing observations equals total observations for every variable.\n"
  )
} else {
  cat(
    "WARNING: One or more variables failed the observation-count check.\n"
  )
}

percentage_check <- dashboard_variable_completeness %>%
  filter(
    !is.na(
      `% complete`
    ),
    !is.na(
      `% missing`
    ),
    abs(
      `% complete` +
        `% missing` -
        100
    ) > 0.02
  )

if (nrow(percentage_check) == 0) {
  cat(
    "GOOD: Percent complete + percent missing equals 100% for every variable.\n"
  )
} else {
  cat(
    "WARNING: One or more variables failed the percentage check.\n"
  )
}

empty_variable_check <- dashboard_variable_completeness %>%
  filter(
    `Complete observations` == 0
  )

if (nrow(empty_variable_check) == 0) {
  cat(
    "GOOD: No completely empty variables were detected.\n"
  )
} else {
  cat(
    "NOTE: ",
    nrow(
      empty_variable_check
    ),
    " completely empty variables were detected and retained in the output.\n",
    sep = ""
  )
}

mealtime_check <- dashboard_variable_completeness %>%
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

if (nrow(mealtime_check) > 0) {
  cat(
    "GOOD: 1992 EatDis mealtime dataset is included in variable completeness.\n"
  )
} else {
  cat(
    "WARNING: 1992 EatDis mealtime dataset was not found in variable completeness.\n"
  )
}

study_count <- dashboard_variable_completeness %>%
  distinct(
    study
  ) %>%
  nrow()

dataset_count <- dashboard_variable_completeness %>%
  distinct(
    study,
    Dataset
  ) %>%
  nrow()

variable_row_count <- nrow(
  dashboard_variable_completeness
)

unique_variable_count <- dashboard_variable_completeness %>%
  distinct(
    Variable
  ) %>%
  nrow()

cat(
  "GOOD: ",
  study_count,
  " studies were included.\n",
  sep = ""
)

cat(
  "GOOD: ",
  dataset_count,
  " study/dataset combinations were included.\n",
  sep = ""
)

cat(
  "GOOD: ",
  variable_row_count,
  " variable-level rows were created.\n",
  sep = ""
)

cat(
  "GOOD: ",
  unique_variable_count,
  " unique variable names were found across the collection.\n",
  sep = ""
)

type_check <- dashboard_variable_completeness %>%
  count(
    Type,
    name = "n_variables"
  ) %>%
  arrange(
    Type
  )

print(
  type_check,
  n = Inf,
  width = Inf
)

mealtime_preview <- dashboard_variable_completeness %>%
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
  ) %>%
  select(
    study,
    Dataset,
    Variable,
    Type,
    `Total observations`,
    `Complete observations`,
    `Missing observations`,
    `% complete`,
    `% missing`
  ) %>%
  slice_head(
    n = 10
  )

if (nrow(mealtime_preview) > 0) {
  print(
    mealtime_preview,
    n = 10,
    width = Inf
  )
}

dashboard_variable_completeness %>%
  select(
    study,
    Dataset,
    Variable,
    Type,
    `Total observations`,
    `Complete observations`,
    `Missing observations`,
    `% complete`,
    `% missing`
  ) %>%
  slice_head(
    n = 30
  ) %>%
  print(
    n = 30,
    width = Inf
  )

readr::write_csv(
  dashboard_variable_completeness,
  file.path(
    output_dir,
    "dashboard_variable_completeness.csv"
  ),
  na = ""
)

cat(
  "Script 09 complete.\n",
  "Created: outputs/dashboard_variable_completeness.csv\n",
  sep = ""
)