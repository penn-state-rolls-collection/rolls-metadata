library(dplyr)
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


file_inventory <- tibble(
  
  file_path = data_csvs,
  
  file_name = basename(
    data_csvs
  ),
  
  study_folder = basename(
    dirname(
      dirname(
        data_csvs
      )
    )
  )
)



file_inventory <- file_inventory %>%
  arrange(
    study_folder,
    file_name
  )



View(
  file_inventory
)


write_csv(
  file_inventory,
  "outputs/file_inventory.csv"
)



cat(
  "\nScript 01 complete.\n\n"
)

cat(
  "Number of data files included: ",
  nrow(file_inventory),
  "\n\n",
  sep = ""
)


cat(
  "Checking for excluded EatDis mealtime files:\n\n"
)

mealtime_check <- file_inventory %>%
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
    "in the file inventory.\n"
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