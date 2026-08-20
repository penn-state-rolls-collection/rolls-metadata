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
  str_detect(all_csvs, regex("/data/", ignore_case = TRUE))
]

file_inventory <- tibble(
  file_path = data_csvs,
  file_name = basename(data_csvs),
  study_folder = basename(
    dirname(
      dirname(data_csvs)
    )
  )
)

View(file_inventory)

write_csv(
  file_inventory,
  "outputs/file_inventory.csv"
)