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
  dir.create(
    output_dir,
    recursive = TRUE
  )
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




grouped_quality_path <- file.path(
  output_dir,
  "grouped_data_quality.csv"
)

if (!file.exists(grouped_quality_path)) {
  stop(
    "outputs/grouped_data_quality.csv was not found. ",
    "Run Script 06 first."
  )
}

grouped_quality <- readr::read_csv(
  grouped_quality_path,
  show_col_types = FALSE
)



required_grouped_columns <- c(
  "study_folder",
  "file_name",
  "data_group",
  "n_variables_observed",
  "total_data_points",
  "total_missing",
  "percent_missing_overall"
)

missing_grouped_columns <- setdiff(
  required_grouped_columns,
  names(grouped_quality)
)

if (length(missing_grouped_columns) > 0) {
  stop(
    paste0(
      "grouped_data_quality.csv is missing required columns: ",
      paste(
        missing_grouped_columns,
        collapse = ", "
      )
    )
  )
}

data_groups <- c(
  "All data",
  "Intake data",
  "Questionnaire data",
  "Total intake variables"
)

study_totals <- grouped_quality %>%
  group_by(
    study_folder,
    data_group
  ) %>%
  summarise(
    
    total_data_points = if (
      all(is.na(total_data_points))
    ) {
      NA_real_
    } else {
      sum(
        total_data_points,
        na.rm = TRUE
      )
    },
    
    total_missing = if (
      all(is.na(total_missing))
    ) {
      NA_real_
    } else {
      sum(
        total_missing,
        na.rm = TRUE
      )
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    
    percent_missing_overall = case_when(
      
      is.na(total_data_points) ~
        NA_real_,
      
      total_data_points == 0 ~
        NA_real_,
      
      TRUE ~
        round(
          100 *
            total_missing /
            total_data_points,
          2
        )
    )
  )


cat(
  "\nCorrected Script 06 totals for 1992_eatdis_deprivation:\n\n"
)

study_totals %>%
  filter(
    study_folder == "1992_eatdis_deprivation",
    data_group == "All data"
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


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
    "No CSV files were found inside data folders."
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
  
  relative_path <- stringr::str_remove(
    normalized_file,
    stringr::fixed(
      paste0(
        normalized_root,
        "/"
      )
    )
  )
  
  pieces <- stringr::str_split(
    relative_path,
    "/"
  )[[1]]
  
  pieces[1]
}


file_lookup <- tibble(
  file_path = data_csvs,
  study_folder = map_chr(
    data_csvs,
    get_study_folder
  ),
  file_name = basename(
    data_csvs
  )
)


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



get_group_variables <- function(
    dat,
    study_folder,
    file_name,
    data_group
) {
  
  # All-data group uses every variable in the file.
  if (data_group == "All data") {
    return(
      names(dat)
    )
  }
  
  
  # Match Script 05 classifications using BOTH
  # study folder and filename.
  current_groups <- variable_groups %>%
    filter(
      .data$study_folder == study_folder,
      .data$file_name == file_name
    )
  
  
  wanted_group <- case_when(
    
    data_group == "Intake data" ~
      "intake",
    
    data_group == "Questionnaire data" ~
      "questionnaire",
    
    data_group == "Total intake variables" ~
      "total_intake",
    
    TRUE ~
      NA_character_
  )
  
  
  current_groups %>%
    filter(
      variable_group == wanted_group
    ) %>%
    pull(
      variable_name
    ) %>%
    unique() %>%
    intersect(
      names(dat)
    )
}

keep_observed_variables <- function(
    dat,
    variable_names
) {
  
  variable_names <- intersect(
    variable_names,
    names(dat)
  )
  
  if (length(variable_names) == 0) {
    return(
      character(0)
    )
  }
  
  
  has_observed_value <- vapply(
    dat[
      ,
      variable_names,
      drop = FALSE
    ],
    function(x) {
      any(!is.na(x))
    },
    logical(1)
  )
  
  
  variable_names[
    has_observed_value
  ]
}


unique_variable_results <- list()

unique_variable_counter <- 1L


for (
  study_name in sort(
    unique(
      file_lookup$study_folder
    )
  )
) {
  
  study_files <- file_lookup %>%
    filter(
      study_folder == study_name
    )
  
  
  for (
    group_name in data_groups
  ) {
    
    observed_names <- character(0)
    
    
    for (
      j in seq_len(
        nrow(
          study_files
        )
      )
    ) {
      
      file_path <-
        study_files$file_path[j]
      
      file_name <-
        study_files$file_name[j]
      
      
      dat <- tryCatch(
        read_data_file(
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
        next
      }
      
      
      selected_variables <- get_group_variables(
        dat = dat,
        study_folder = study_name,
        file_name = file_name,
        data_group = group_name
      )
      
      
      observed_variables <- keep_observed_variables(
        dat = dat,
        variable_names = selected_variables
      )
      
      
      observed_names <- union(
        observed_names,
        observed_variables
      )
    }
    
    
    unique_variable_results[[unique_variable_counter]] <- tibble(
      
      study_folder =
        study_name,
      
      data_group =
        group_name,
      
      n_variables_observed =
        as.integer(
          length(
            observed_names
          )
        )
    )
    
    
    unique_variable_counter <-
      unique_variable_counter + 1L
  }
}


study_variable_counts <- bind_rows(
  unique_variable_results
)

participant_results <- list()

participant_counter <- 1L


for (
  study_name in sort(
    unique(
      file_lookup$study_folder
    )
  )
) {
  
  study_files <- file_lookup %>%
    filter(
      study_folder == study_name
    )
  
  
  for (
    group_name in data_groups
  ) {
    
    
    for (
      j in seq_len(
        nrow(
          study_files
        )
      )
    ) {
      
      file_path <-
        study_files$file_path[j]
      
      file_name <-
        study_files$file_name[j]
      
      
      dat <- tryCatch(
        read_data_file(
          file_path
        ),
        error = function(e) {
          NULL
        }
      )
      
      
      if (is.null(dat)) {
        next
      }
      
      
      # Participant calculations require an ID variable.
      if (!"id" %in% names(dat)) {
        next
      }
      
      
      selected_variables <- get_group_variables(
        dat = dat,
        study_folder = study_name,
        file_name = file_name,
        data_group = group_name
      )
      
      
      observed_variables <- keep_observed_variables(
        dat = dat,
        variable_names = selected_variables
      )
      
      
      # ID is used for grouping but should not count toward
      # participant completeness.
      participant_variables <- setdiff(
        observed_variables,
        "id"
      )
      
      
      if (length(participant_variables) == 0) {
        next
      }
      
      
      current_participant <- tibble(
        
        participant_id =
          as.character(
            dat$id
          ),
        
        missing_cells =
          rowSums(
            is.na(
              dat[
                ,
                participant_variables,
                drop = FALSE
              ]
            )
          ),
        
        total_cells =
          rep(
            length(
              participant_variables
            ),
            nrow(
              dat
            )
          )
      ) %>%
        
        filter(
          !is.na(
            participant_id
          ),
          nzchar(
            stringr::str_trim(
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
          
          .groups =
            "drop"
        ) %>%
        
        mutate(
          
          study_folder =
            study_name,
          
          data_group =
            group_name,
          
          .before =
            1
        )
      
      
      participant_results[[participant_counter]] <-
        current_participant
      
      
      participant_counter <-
        participant_counter + 1L
    }
  }
}


participant_cells <- bind_rows(
  participant_results
)


if (nrow(participant_cells) > 0) {
  
  participant_summary <- participant_cells %>%
    
    group_by(
      study_folder,
      data_group,
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
      
      .groups =
        "drop"
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
  
  
  participant_study_summary <- participant_summary %>%
    
    group_by(
      study_folder,
      data_group
    ) %>%
    
    summarise(
      
      mean_percent_missing_participant =
        round(
          mean(
            percent_missing,
            na.rm = TRUE
          ),
          2
        ),
      
      min_percent_missing_participant =
        min(
          percent_missing,
          na.rm = TRUE
        ),
      
      max_percent_missing_participant =
        max(
          percent_missing,
          na.rm = TRUE
        ),
      
      complete_cases =
        sum(
          percent_missing == 0,
          na.rm = TRUE
        ),
      
      participants_85_percent_complete =
        sum(
          percent_complete >= 85,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    ) %>%
    
    mutate(
      
      range_percent_missing_participant =
        paste0(
          round(
            min_percent_missing_participant,
            2
          ),
          "% - ",
          round(
            max_percent_missing_participant,
            2
          ),
          "%"
        )
    ) %>%
    
    select(
      -min_percent_missing_participant,
      -max_percent_missing_participant
    )
  
} else {
  
  participant_study_summary <- tibble(
    
    study_folder =
      character(),
    
    data_group =
      character(),
    
    mean_percent_missing_participant =
      numeric(),
    
    range_percent_missing_participant =
      character(),
    
    complete_cases =
      integer(),
    
    participants_85_percent_complete =
      integer()
  )
}

calculate_macro_status <- function(
    study_name
) {
  
  intake_names <- variable_groups %>%
    filter(
      study_folder == study_name,
      variable_group %in% c(
        "intake",
        "total_intake"
      )
    ) %>%
    pull(
      variable_name
    ) %>%
    unique()
  
  
  has_carbohydrate <- any(
    stringr::str_detect(
      intake_names,
      stringr::regex(
        "(^|_)(cho|carb|carbohydrate)($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  
  has_fat <- any(
    stringr::str_detect(
      intake_names,
      stringr::regex(
        "(^|_)fat($|_)",
        ignore_case = TRUE
      )
    )
  )
  
  
  has_protein <- any(
    stringr::str_detect(
      intake_names,
      stringr::regex(
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


study_grid <- tidyr::expand_grid(
  
  study_folder =
    sort(
      unique(
        file_lookup$study_folder
      )
    ),
  
  data_group =
    data_groups
)



study_data_quality <- study_grid %>%
  
  left_join(
    study_variable_counts,
    by = c(
      "study_folder",
      "data_group"
    )
  ) %>%
  
  left_join(
    study_totals,
    by = c(
      "study_folder",
      "data_group"
    )
  ) %>%
  
  left_join(
    participant_study_summary,
    by = c(
      "study_folder",
      "data_group"
    )
  ) %>%
  
  mutate(
    
    n_variables_observed =
      replace_na(
        n_variables_observed,
        0L
      ),
    
    intake_macros_computed =
      map_chr(
        study_folder,
        calculate_macro_status
      )
  )



study_data_quality <- study_data_quality %>%
  
  mutate(
    
    total_data_points =
      if_else(
        n_variables_observed == 0,
        NA_real_,
        total_data_points
      ),
    
    total_missing =
      if_else(
        n_variables_observed == 0,
        NA_real_,
        total_missing
      ),
    
    percent_missing_overall =
      if_else(
        n_variables_observed == 0,
        NA_real_,
        percent_missing_overall
      ),
    
    mean_percent_missing_participant =
      if_else(
        n_variables_observed == 0,
        NA_real_,
        mean_percent_missing_participant
      ),
    
    range_percent_missing_participant =
      if_else(
        n_variables_observed == 0,
        NA_character_,
        range_percent_missing_participant
      ),
    
    complete_cases =
      if_else(
        n_variables_observed == 0,
        NA_integer_,
        complete_cases
      ),
    
    participants_85_percent_complete =
      if_else(
        n_variables_observed == 0,
        NA_integer_,
        participants_85_percent_complete
      )
  )


study_data_quality <- study_data_quality %>%
  
  mutate(
    
    data_group = factor(
      data_group,
      levels = data_groups
    )
  ) %>%
  
  arrange(
    study_folder,
    data_group
  ) %>%
  
  mutate(
    
    data_group =
      as.character(
        data_group
      )
  )


study_data_quality <- study_data_quality %>%
  
  select(
    
    study_folder,
    
    data_group,
    
    n_variables_observed,
    
    total_data_points,
    
    total_missing,
    
    percent_missing_overall,
    
    mean_percent_missing_participant,
    
    range_percent_missing_participant,
    
    complete_cases,
    
    participants_85_percent_complete,
    
    intake_macros_computed
  )



readr::write_csv(
  study_data_quality,
  file.path(
    output_dir,
    "study_data_quality.csv"
  ),
  na = ""
)


View(
  study_data_quality
)

study_data_quality %>%
  print(
    n = Inf,
    width = Inf
  )

cat(
  "\nFINAL 1992_eatdis_deprivation verification:\n\n"
)

study_data_quality %>%
  
  filter(
    study_folder ==
      "1992_eatdis_deprivation"
  ) %>%
  
  select(
    
    data_group,
    
    n_variables_observed,
    
    total_data_points,
    
    total_missing,
    
    percent_missing_overall,
    
    mean_percent_missing_participant,
    
    range_percent_missing_participant,
    
    complete_cases,
    
    participants_85_percent_complete
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\nScript 07 complete.\n",
  "Created: outputs/study_data_quality.csv\n",
  sep = ""
)
