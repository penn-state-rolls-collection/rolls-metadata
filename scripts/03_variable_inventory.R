library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)


curated_dir <- Sys.getenv(
  "ROLLS_CURATED_DATA"
)


if (curated_dir == "") {
  stop(
    "ROLLS_CURATED_DATA has not been set."
  )
}


if (!dir.exists(curated_dir)) {
  stop(
    "The curated data folder could not be found."
  )
}


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
    regex(
      "/data/",
      ignore_case = TRUE
    )
  )
]


data_csvs <- data_csvs[
  !(
    str_detect(
      data_csvs,
      regex(
        "/1992_eatdis_deprivation/",
        ignore_case = TRUE
      )
    ) &
      str_detect(
        basename(data_csvs),
        regex(
          "mealtime",
          ignore_case = TRUE
        )
      )
  )
]


if (length(data_csvs) == 0) {
  stop(
    "No CSV files were found inside the study data folders."
  )
}


inventory_variables <- function(file) {
  
  dat <- read_csv(
    file,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character()
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
  
  
  normalized_file <- normalizePath(
    file,
    winslash = "/",
    mustWork = FALSE
  )
  
  
  study_folder <- basename(
    dirname(
      dirname(
        normalized_file
      )
    )
  )
  
  
  tibble(
    study_folder = study_folder,
    file_name = basename(file),
    variable_name = names(dat)
  )
}



variable_inventory <- map_dfr(
  data_csvs,
  inventory_variables
)


variable_inventory <- variable_inventory %>%
  arrange(
    study_folder,
    file_name,
    variable_name
  )


write_csv(
  variable_inventory,
  "outputs/variable_inventory.csv"
)


View(
  variable_inventory
)


cat(
  "\nScript 03 complete.\n\n"
)


cat(
  "Total variables inventoried: ",
  nrow(variable_inventory),
  "\n\n",
  sep = ""
)


cat(
  "Number of datasets represented: ",
  n_distinct(
    paste(
      variable_inventory$study_folder,
      variable_inventory$file_name
    )
  ),
  "\n\n",
  sep = ""
)


mealtime_check <- variable_inventory %>%
  filter(
    study_folder == "1992_eatdis_deprivation",
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
    "GOOD: No variables from the 1992 EatDis mealtime ",
    "dataset are present in variable_inventory.csv.\n\n"
  )
  
} else {
  
  cat(
    "WARNING: Variables from the 1992 EatDis mealtime ",
    "dataset are still present.\n\n"
  )
  
  print(
    mealtime_check,
    n = Inf,
    width = Inf
  )
}


cat(
  "Remaining 1992 EatDis deprivation files ",
  "in the variable inventory:\n\n"
)


variable_inventory %>%
  filter(
    study_folder == "1992_eatdis_deprivation"
  ) %>%
  distinct(
    study_folder,
    file_name
  ) %>%
  arrange(
    file_name
  ) %>%
  print(
    n = Inf,
    width = Inf
  )