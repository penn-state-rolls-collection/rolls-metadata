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


required_columns <- c(
  "study_folder",
  "file_name",
  "variable_name",
  "variable_group"
)


missing_columns <- setdiff(
  required_columns,
  names(variable_groups)
)


if (length(missing_columns) > 0) {
  stop(
    paste0(
      "variable_groups.csv is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


variable_groups <- variable_groups %>%
  
  filter(
    !(
      study_folder ==
        "1992_eatdis_deprivation" &
        str_detect(
          file_name,
          regex(
            "mealtime",
            ignore_case = TRUE
          )
        )
    )
  )


variable_groups <- variable_groups %>%
  
  mutate(
    
    data_group = case_when(
      
      variable_group == "intake" ~
        "Intake variables",
      
      variable_group == "macronutrient" ~
        "Macronutrient data",
      
      variable_group == "questionnaire" ~
        "Questionnaire data",
      
      TRUE ~
        "Other data"
    )
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


# Explicitly remove 1992 EatDis deprivation mealtime
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


if (length(data_csvs) == 0) {
  stop(
    "No CSV files were found inside the curated data folders."
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


# ============================================================
# FILE LOOKUP
# ============================================================

file_lookup <- tibble(
  
  file_path =
    data_csvs,
  
  study_folder =
    map_chr(
      data_csvs,
      get_study_folder
    ),
  
  file_name =
    basename(
      data_csvs
    )
)


read_quality_data <- function(file_path) {
  
  dat <- readr::read_csv(
    
    file_path,
    
    show_col_types = FALSE,
    
    col_types = readr::cols(
      .default =
        readr::col_character()
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
          
          x[x == ""] <-
            NA_character_
          
          x
        }
      )
    )
  
  
  dat
}


calculate_group_quality <- function(
    file_path,
    study_name,
    file_name,
    group_name
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
  
  
  # Get variables belonging to this exact file/group
  selected_variables <- variable_groups %>%
    
    filter(
      study_folder == study_name,
      .data$file_name == file_name,
      data_group == group_name
    ) %>%
    
    pull(
      variable_name
    ) %>%
    
    unique() %>%
    
    intersect(
      names(dat)
    )
  
  
  # No variables from this group in this file
  if (length(selected_variables) == 0) {
    return(NULL)
  }
  
  
  # Keep variables with at least one observed value
  has_observed_value <- vapply(
    
    dat[
      ,
      selected_variables,
      drop = FALSE
    ],
    
    function(x) {
      any(
        !is.na(x)
      )
    },
    
    logical(1)
  )
  
  
  observed_variables <-
    selected_variables[
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
  
  
  total_missing <-
    sum(
      is.na(
        selected_data
      )
    )
  
  
  percent_missing_overall <-
    if (
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
  
  
  percent_data_present <-
    if (
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
  

  mean_percent_missing_participant <-
    NA_real_
  
  range_percent_missing_participant <-
    NA_character_
  
  complete_cases <-
    NA_integer_
  
  participants_85_percent_complete <-
    NA_integer_
  
  
  if ("id" %in% names(dat)) {
    
    participant_variables <- setdiff(
      observed_variables,
      "id"
    )
    
    
    if (length(participant_variables) > 0) {
      
      participant_missing <- tibble(
        
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
      
      
      if (nrow(participant_missing) > 0) {
        
        mean_percent_missing_participant <-
          round(
            mean(
              participant_missing$percent_missing,
              na.rm = TRUE
            ),
            2
          )
        
        
        participant_range <-
          range(
            participant_missing$percent_missing,
            na.rm = TRUE
          )
        
        
        range_percent_missing_participant <-
          paste0(
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
        
        
        complete_cases <-
          sum(
            participant_missing$percent_missing == 0,
            na.rm = TRUE
          )
        
        
        participants_85_percent_complete <-
          sum(
            participant_missing$percent_complete >= 85,
            na.rm = TRUE
          )
      }
    }
  }
  
  
  tibble(
    
    study_folder =
      study_name,
    
    file_name =
      file_name,
    
    data_group =
      group_name,
    
    n_variables =
      as.integer(
        length(
          observed_variables
        )
      ),
    
    total_data_points =
      as.numeric(
        total_data_points
      ),
    
    total_missing =
      as.numeric(
        total_missing
      ),
    
    percent_missing_overall =
      percent_missing_overall,
    
    percent_data_present =
      percent_data_present,
    
    mean_percent_missing_participant =
      mean_percent_missing_participant,
    
    range_percent_missing_participant =
      range_percent_missing_participant,
    
    complete_cases =
      complete_cases,
    
    participants_85_percent_complete =
      participants_85_percent_complete
  )
}

quality_results <- list()

quality_counter <- 1L


data_group_names <- c(
  "Intake variables",
  "Macronutrient data",
  "Questionnaire data",
  "Other data"
)


for (
  i in seq_len(
    nrow(
      file_lookup
    )
  )
) {
  
  current_file <-
    file_lookup$file_path[i]
  
  current_study <-
    file_lookup$study_folder[i]
  
  current_file_name <-
    file_lookup$file_name[i]
  
  
  for (
    current_group in data_group_names
  ) {
    
    result <- calculate_group_quality(
      
      file_path =
        current_file,
      
      study_name =
        current_study,
      
      file_name =
        current_file_name,
      
      group_name =
        current_group
    )
    
    
    if (!is.null(result)) {
      
      quality_results[[quality_counter]] <-
        result
      
      quality_counter <-
        quality_counter + 1L
    }
  }
}


if (length(quality_results) == 0) {
  
  stop(
    "No grouped data-quality results were created."
  )
}


grouped_data_quality <- bind_rows(
  quality_results
)


grouped_data_quality <- grouped_data_quality %>%
  
  mutate(
    
    data_group = factor(
      data_group,
      levels = data_group_names
    )
  ) %>%
  
  arrange(
    study_folder,
    file_name,
    data_group
  ) %>%
  
  mutate(
    data_group =
      as.character(
        data_group
      )
  )


readr::write_csv(
  
  grouped_data_quality,
  
  file.path(
    output_dir,
    "grouped_data_quality.csv"
  ),
  
  na = ""
)



mealtime_check <- grouped_data_quality %>%
  
  filter(
    study_folder ==
      "1992_eatdis_deprivation",
    
    str_detect(
      file_name,
      regex(
        "mealtime",
        ignore_case = TRUE
      )
    )
  )


if (nrow(mealtime_check) == 0) {
  
  cat(
    "GOOD: 1992 EatDis mealtime dataset is excluded.\n"
  )
  
} else {
  
  cat(
    "WARNING: 1992 EatDis mealtime dataset is still present.\n"
  )
}


check_group <- function(
    study_name,
    group_name,
    display_name
) {
  
  n_found <- grouped_data_quality %>%
    
    filter(
      study_folder == study_name,
      data_group == group_name
    ) %>%
    
    summarise(
      n =
        sum(
          n_variables,
          na.rm = TRUE
        )
    ) %>%
    
    pull(
      n
    )
  
  
  if (
    length(n_found) == 1 &&
    !is.na(n_found) &&
    n_found > 0
  ) {
    
    cat(
      "GOOD: ",
      display_name,
      " has ",
      n_found,
      " ",
      group_name,
      " variables.\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "WARNING: ",
      display_name,
      " has no ",
      group_name,
      " variables.\n",
      sep = ""
    )
  }
}



check_group(
  "1991_eatdis",
  "Intake variables",
  "1991 EatDis"
)

check_group(
  "1991_eatdis",
  "Macronutrient data",
  "1991 EatDis"
)

check_group(
  "1991_ivig_preload",
  "Intake variables",
  "1991 IVIG preload"
)

check_group(
  "1991_ivig_preload",
  "Macronutrient data",
  "1991 IVIG preload"
)



grouped_data_quality %>%
  
  filter(
    data_group %in% c(
      "Intake variables",
      "Macronutrient data"
    )
  ) %>%
  
  group_by(
    study_folder,
    data_group
  ) %>%
  
  summarise(
    
    n_variables =
      sum(
        n_variables,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    study_folder,
    data_group
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )



grouped_data_quality %>%
  
  filter(
    study_folder %in% c(
      "1991_eatdis",
      "1991_ivig_preload",
      "1992_eatdis_deprivation"
    ),
    data_group %in% c(
      "Intake variables",
      "Macronutrient data"
    )
  ) %>%
  
  select(
    study_folder,
    file_name,
    data_group,
    n_variables,
    total_data_points,
    total_missing,
    percent_missing_overall,
    percent_data_present
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\nScript 06 complete.\n",
  "Created: outputs/grouped_data_quality.csv\n",
  sep = ""
)