library(dplyr)
library(readr)
library(stringr)

# Read the inventory we already created
file_inventory <- read_csv(
  "outputs/file_inventory.csv",
  show_col_types = FALSE
)

# Create a standardized file type
file_types <- file_inventory %>%
  mutate(
    file_type = case_when(
      
      # Participant-level microstructure files
      str_detect(
        file_name,
        regex("assay-microstructure", ignore_case = TRUE)
      ) ~ "microstructure",
      
      # Intake-related files
      str_detect(
        file_name,
        regex(
          "assay-intake|assay-lunch|assay-dinner",
          ignore_case = TRUE
        )
      ) ~ "intake",
      
      # Questionnaire files
      str_detect(
        file_name,
        regex(
          "assay-questionnaire|assay-foodq",
          ignore_case = TRUE
        )
      ) ~ "questionnaire",
      
      # VAS / rating files
      str_detect(
        file_name,
        regex(
          "assay-vas|assay-rating|assay-hunger",
          ignore_case = TRUE
        )
      ) ~ "ratings",
      
      # Demographic data
      str_detect(
        file_name,
        regex(
          "assay-demo",
          ignore_case = TRUE
        )
      ) ~ "demographics",
      
      # Food preference data
      str_detect(
        file_name,
        regex(
          "assay-foodpref|calc-preference|calc-foodinfo",
          ignore_case = TRUE
        )
      ) ~ "food_preference",
      
      # Sensory-specific satiety
      str_detect(
        file_name,
        regex(
          "assay-sss|calc-diff",
          ignore_case = TRUE
        )
      ) ~ "sss",
      
      # Smell / UPSIT
      str_detect(
        file_name,
        regex(
          "upsit|smell",
          ignore_case = TRUE
        )
      ) ~ "smell",
      
      # CCK
      str_detect(
        file_name,
        regex(
          "assay-cck",
          ignore_case = TRUE
        )
      ) ~ "cck",
      
      # Timing calculations
      str_detect(
        file_name,
        regex(
          "timedif",
          ignore_case = TRUE
        )
      ) ~ "timing",
      
      # Anything that has not yet been classified
      TRUE ~ "other"
    )
  )

View(file_types)

write_csv(
  file_types,
  "outputs/file_types.csv"
)

file_types %>%
  count(file_type, sort = TRUE) %>%
  print(n = Inf)