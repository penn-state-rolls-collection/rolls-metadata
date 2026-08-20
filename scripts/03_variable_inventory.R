library(dplyr)
library(purrr)
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

# Find all CSV files
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

# Keep only files located inside data folders
data_csvs <- all_csvs[
  str_detect(
    all_csvs,
    regex("/data/", ignore_case = TRUE)
  )
]

# Function to get the variable names from one CSV
get_variable_inventory <- function(file) {
  
  dat <- read_csv(
    file,
    show_col_types = FALSE,
    n_max = 1
  )
  
  study_folder <- basename(
    dirname(
      dirname(file)
    )
  )
  
  tibble(
    study_folder = study_folder,
    file_name = basename(file),
    variable_name = names(dat)
  )
}

# Run the function across every curated CSV
variable_inventory <- map_dfr(
  data_csvs,
  get_variable_inventory
)

# Sort results
variable_inventory <- variable_inventory %>%
  arrange(
    study_folder,
    file_name,
    variable_name
  )

View(variable_inventory)

# Save inventory
write_csv(
  variable_inventory,
  "outputs/variable_inventory.csv"
)