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


required_variable_columns <- c(
  "study_folder",
  "file_name",
  "variable_name",
  "variable_group"
)

missing_variable_columns <- setdiff(
  required_variable_columns,
  names(variable_groups)
)

if (length(missing_variable_columns) > 0) {
  stop(
    paste0(
      "variable_groups.csv is missing required columns: ",
      paste(
        missing_variable_columns,
        collapse = ", "
      )
    )
  )
}



variable_groups <- variable_groups %>%
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


# Remove mealtime dataset here too
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


read_data_file <- function(file_path) {
  
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


dashboard_groups <- c(
  "All data",
  "Measured intake",
  "Macronutrient intake",
  "Total intake",
  "Questionnaire data"
)


get_variable_groups_for_dashboard <- function(
    dashboard_group
) {
  
  if (dashboard_group == "Measured intake") {
    
    return(
      "intake"
    )
  }
  
  
  if (dashboard_group == "Macronutrient intake") {
    
    return(
      "macronutrient"
    )
  }
  
  
  if (dashboard_group == "Total intake") {
    
    return(
      c(
        "intake",
        "macronutrient"
      )
    )
  }
  
  
  if (dashboard_group == "Questionnaire data") {
    
    return(
      "questionnaire"
    )
  }
  
  
  character(0)
}


# ============================================================
# GET VARIABLES FOR FILE / DASHBOARD GROUP
# ============================================================

get_group_variables <- function(
    dat,
    study_name,
    file_name,
    dashboard_group
) {
  
  # All data = every variable in the file
  if (dashboard_group == "All data") {
    
    return(
      names(dat)
    )
  }
  
  
  wanted_groups <-
    get_variable_groups_for_dashboard(
      dashboard_group
    )
  
  
  selected_variables <- variable_groups %>%
    
    filter(
      study_folder == study_name,
      .data$file_name == file_name,
      variable_group %in% wanted_groups
    ) %>%
    
    pull(
      variable_name
    ) %>%
    
    unique() %>%
    
    intersect(
      names(dat)
    )
  
  
  selected_variables
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
      
      any(
        !is.na(x)
      )
    },
    
    logical(1)
  )
  
  
  variable_names[
    has_observed_value
  ]
}


calculate_study_group <- function(
    study_name,
    dashboard_group
) {
  
  study_files <- file_lookup %>%
    
    filter(
      study_folder == study_name
    )
  
  
  
  all_observed_names <-
    character(0)
  
  total_data_points <-
    0
  
  total_missing <-
    0
  
  found_group_data <-
    FALSE
  
  
  participant_results <-
    list()
  
  participant_counter <-
    1L
  
  
  
  for (
    i in seq_len(
      nrow(
        study_files
      )
    )
  ) {
    
    current_file <-
      study_files$file_path[i]
    
    current_file_name <-
      study_files$file_name[i]
    
    
    dat <- tryCatch(
      
      read_data_file(
        current_file
      ),
      
      error = function(e) {
        
        warning(
          "Could not read ",
          current_file,
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
      
      dat =
        dat,
      
      study_name =
        study_name,
      
      file_name =
        current_file_name,
      
      dashboard_group =
        dashboard_group
    )
    
    
    observed_variables <- keep_observed_variables(
      
      dat =
        dat,
      
      variable_names =
        selected_variables
    )
    
    
    if (length(observed_variables) == 0) {
      next
    }
    
    
    found_group_data <-
      TRUE
    
    
    # Unique variable names across the study
    all_observed_names <- union(
      all_observed_names,
      observed_variables
    )
    
    
    current_data <- dat[
      ,
      observed_variables,
      drop = FALSE
    ]
    
    
    total_data_points <-
      total_data_points +
      nrow(current_data) *
      ncol(current_data)
    
    
    total_missing <-
      total_missing +
      sum(
        is.na(
          current_data
        )
      )
    
    
    
    if ("id" %in% names(dat)) {
      
      participant_variables <- setdiff(
        observed_variables,
        "id"
      )
      
      
      if (length(participant_variables) > 0) {
        
        current_participants <- tibble(
          
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
          )
        
        
        participant_results[[participant_counter]] <-
          current_participants
        
        
        participant_counter <-
          participant_counter + 1L
      }
    }
  }
  
  
  if (!found_group_data) {
    
    return(
      tibble(
        
        study_folder =
          study_name,
        
        data_group =
          dashboard_group,
        
        n_variables_observed =
          0L,
        
        total_data_points =
          NA_real_,
        
        total_missing =
          NA_real_,
        
        percent_missing_overall =
          NA_real_,
        
        percent_data_present =
          NA_real_,
        
        mean_percent_missing_participant =
          NA_real_,
        
        range_percent_missing_participant =
          NA_character_,
        
        complete_cases =
          NA_integer_,
        
        participants_85_percent_complete =
          NA_integer_
      )
    )
  }
  
  
  
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
  
  
  if (length(participant_results) > 0) {
    
    participant_summary <- bind_rows(
      participant_results
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
    
    
    if (nrow(participant_summary) > 0) {
      
      mean_percent_missing_participant <-
        round(
          mean(
            participant_summary$percent_missing,
            na.rm = TRUE
          ),
          2
        )
      
      
      participant_range <- range(
        participant_summary$percent_missing,
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
          participant_summary$percent_missing == 0,
          na.rm = TRUE
        )
      
      
      participants_85_percent_complete <-
        sum(
          participant_summary$percent_complete >= 85,
          na.rm = TRUE
        )
    }
  }
  
  
  tibble(
    
    study_folder =
      study_name,
    
    data_group =
      dashboard_group,
    
    n_variables_observed =
      as.integer(
        length(
          all_observed_names
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


all_studies <- sort(
  unique(
    file_lookup$study_folder
  )
)


study_results <- list()

result_counter <- 1L


for (
  current_study in all_studies
) {
  
  for (
    current_group in dashboard_groups
  ) {
    
    study_results[[result_counter]] <-
      calculate_study_group(
        
        study_name =
          current_study,
        
        dashboard_group =
          current_group
      )
    
    
    result_counter <-
      result_counter + 1L
  }
}


study_data_quality <- bind_rows(
  study_results
)


macro_status <- variable_groups %>%
  
  group_by(
    study_folder
  ) %>%
  
  summarise(
    
    has_measured_intake =
      any(
        variable_group == "intake"
      ),
    
    has_macronutrient =
      any(
        variable_group == "macronutrient"
      ),
    
    has_carbohydrate =
      any(
        variable_group == "macronutrient" &
          str_detect(
            variable_name,
            regex(
              "(^|_)(cho|carb|carbohydrate)($|_)",
              ignore_case = TRUE
            )
          )
      ),
    
    has_fat =
      any(
        variable_group == "macronutrient" &
          str_detect(
            variable_name,
            regex(
              "(^|_)fat($|_)",
              ignore_case = TRUE
            )
          )
      ),
    
    has_protein =
      any(
        variable_group == "macronutrient" &
          str_detect(
            variable_name,
            regex(
              "(^|_)(pro|protein)($|_)",
              ignore_case = TRUE
            )
          )
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    intake_macros_computed =
      case_when(
        
        has_carbohydrate &
          has_fat &
          has_protein ~
          "Yes",
        
        has_measured_intake |
          has_macronutrient ~
          "No",
        
        TRUE ~
          NA_character_
      )
  ) %>%
  
  select(
    study_folder,
    intake_macros_computed
  )


study_data_quality <- study_data_quality %>%
  
  left_join(
    macro_status,
    by = "study_folder"
  )


study_data_quality <- study_data_quality %>%
  
  mutate(
    
    data_group = factor(
      data_group,
      levels = dashboard_groups
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
    
    percent_data_present,
    
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




mealtime_check <- file_lookup %>%
  
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



get_group_count <- function(
    study_name,
    group_name
) {
  
  result <- study_data_quality %>%
    
    filter(
      study_folder == study_name,
      data_group == group_name
    )
  
  
  if (nrow(result) != 1) {
    
    return(
      NA_integer_
    )
  }
  
  
  result$n_variables_observed[1]
}


check_group <- function(
    study_name,
    group_name,
    display_name
) {
  
  result <- study_data_quality %>%
    
    filter(
      study_folder == study_name,
      data_group == group_name
    )
  
  
  if (
    nrow(result) == 1 &&
    !is.na(
      result$n_variables_observed[1]
    ) &&
    result$n_variables_observed[1] > 0 &&
    !is.na(
      result$total_data_points[1]
    )
  ) {
    
    cat(
      "GOOD: ",
      display_name,
      " - ",
      group_name,
      " = ",
      result$n_variables_observed[1],
      " variables, ",
      result$total_data_points[1],
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



check_total_intake <- function(
    study_name,
    display_name
) {
  
  measured_n <- get_group_count(
    study_name,
    "Measured intake"
  )
  
  
  macro_n <- get_group_count(
    study_name,
    "Macronutrient intake"
  )
  
  
  total_n <- get_group_count(
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
      " total intake = measured intake + macronutrient intake (",
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
      " total intake does not equal measured + macronutrient intake.\n",
      sep = ""
    )
  }
}


check_group(
  "1991_eatdis",
  "Measured intake",
  "1991 EatDis"
)


check_group(
  "1991_eatdis",
  "Macronutrient intake",
  "1991 EatDis"
)


check_group(
  "1991_eatdis",
  "Total intake",
  "1991 EatDis"
)


check_total_intake(
  "1991_eatdis",
  "1991 EatDis"
)



check_group(
  "1991_ivig_preload",
  "Measured intake",
  "1991 IVIG preload"
)


check_group(
  "1991_ivig_preload",
  "Macronutrient intake",
  "1991 IVIG preload"
)


check_group(
  "1991_ivig_preload",
  "Total intake",
  "1991 IVIG preload"
)


check_total_intake(
  "1991_ivig_preload",
  "1991 IVIG preload"
)


study_data_quality %>%
  
  filter(
    study_folder %in% c(
      "1991_eatdis",
      "1991_ivig_preload",
      "1992_eatdis_deprivation"
    )
  ) %>%
  
  select(
    study_folder,
    data_group,
    n_variables_observed,
    total_data_points,
    total_missing,
    percent_missing_overall,
    percent_data_present,
    intake_macros_computed
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