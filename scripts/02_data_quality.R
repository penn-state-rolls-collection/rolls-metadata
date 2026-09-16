library(dplyr)
library(readr)
library(stringr)
library(purrr)



source(
  "R/functions_data_quality.R"
)



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



data_quality <- map_dfr(
  data_csvs,
  calculate_data_quality
)



data_quality <- data_quality %>%
  arrange(
    study_folder,
    file_name
  )



View(
  data_quality
)


write_csv(
  data_quality,
  "outputs/data_quality.csv"
)


cat(
  "\nScript 02 complete.\n\n"
)


cat(
  "Number of datasets included in data-quality output: ",
  nrow(data_quality),
  "\n\n",
  sep = ""
)


mealtime_check <- data_quality %>%
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
    "GOOD: No 1992 EatDis mealtime dataset is present ",
    "in data_quality.csv.\n\n"
  )
  
} else {
  
  cat(
    "WARNING: A 1992 EatDis mealtime dataset is still present.\n\n"
  )
  
  print(
    mealtime_check,
    n = Inf,
    width = Inf
  )
}


cat(
  "Remaining 1992 EatDis deprivation datasets:\n\n"
)


data_quality %>%
  filter(
    study_folder == "1992_eatdis_deprivation"
  ) %>%
  select(
    study_folder,
    file_name,
    n_rows,
    n_variables,
    total_missing,
    percent_missing_overall,
    intake_macros_computed
  ) %>%
  print(
    n = Inf,
    width = Inf
  )