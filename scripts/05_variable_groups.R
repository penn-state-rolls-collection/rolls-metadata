library(dplyr)
library(readr)
library(stringr)

variable_inventory <- read_csv(
  "outputs/variable_inventory.csv",
  show_col_types = FALSE
)

variable_groups <- variable_inventory %>%
  mutate(
    
    
    file_type = case_when(
      
      str_detect(
        file_name,
        regex("assay-microstructure", ignore_case = TRUE)
      ) ~ "microstructure",
      
      str_detect(
        file_name,
        regex(
          "assay-intake|assay-lunch|assay-dinner",
          ignore_case = TRUE
        )
      ) ~ "intake",
      
      str_detect(
        file_name,
        regex(
          "assay-questionnaire|assay-foodq",
          ignore_case = TRUE
        )
      ) ~ "questionnaire",
      
      str_detect(
        file_name,
        regex(
          "assay-vas|assay-rating|assay-hunger",
          ignore_case = TRUE
        )
      ) ~ "ratings",
      
      str_detect(
        file_name,
        regex("assay-demo", ignore_case = TRUE)
      ) ~ "demographics",
      
      str_detect(
        file_name,
        regex(
          "assay-foodpref|calc-preference|calc-foodinfo",
          ignore_case = TRUE
        )
      ) ~ "food_preference",
      
      str_detect(
        file_name,
        regex(
          "assay-sss|calc-diff",
          ignore_case = TRUE
        )
      ) ~ "sss",
      
      str_detect(
        file_name,
        regex(
          "upsit|smell",
          ignore_case = TRUE
        )
      ) ~ "smell",
      
      str_detect(
        file_name,
        regex("assay-cck", ignore_case = TRUE)
      ) ~ "cck",
      
      str_detect(
        file_name,
        regex("timedif", ignore_case = TRUE)
      ) ~ "timing",
      
      TRUE ~ "other"
    ),
    
    
    variable_group = case_when(
      
      
      str_detect(
        variable_name,
        regex(
          "^total_(g|cal|kcal|fat|cho|pro|protein|carb|carbohydrate)$",
          ignore_case = TRUE
        )
      ) ~ "total_intake",
    
      
      str_detect(
        variable_name,
        regex(
          "^(zung|eat|edi|ei|eisc|lsi|beck|bsq|qewp|debq|pfs|cfq)(_|$)",
          ignore_case = TRUE
        )
      ) ~ "questionnaire",
      
      
      file_type == "intake" &
        str_detect(
          variable_name,
          regex(
            "(_g$|^g_|_cal$|^cal_|kcal|^fat_|_fat$|^cho_|_cho$|^pro_|_pro$|protein|carb|gram|intake)",
            ignore_case = TRUE
          )
        ) ~ "intake",
      
      
      file_type %in% c(
        "questionnaire",
        "ratings",
        "food_preference",
        "sss"
      ) ~ "questionnaire",
      
      
      file_type != "microstructure" &
        str_detect(
          variable_name,
          regex(
            "hunger|thirst|fullness|desire|much_eat|taste|pleasant|sweetness|fattiness|creaminess|fruitiness|anxious|anxiety|depressed|bloated|guilt|nausea|calm|drowsy|alert|relaxed|tense|sleepy|appearance|odor|texture|like|freq_eat|_eat$",
            ignore_case = TRUE
          )
        ) ~ "questionnaire",
      
      TRUE ~ "other"
    )
  )


write_csv(
  variable_groups,
  "outputs/variable_groups.csv"
)

View(variable_groups)

variable_groups %>%
  count(variable_group, sort = TRUE) %>%
  print(n = Inf)