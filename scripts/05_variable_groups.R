library(dplyr)
library(readr)
library(stringr)

variable_inventory <- read_csv(
  "outputs/variable_inventory.csv",
  show_col_types = FALSE
)

variable_groups <- variable_inventory %>%
  filter(
    !(
      study_folder == "1992_eatdis_deprivation" &
        str_detect(
          file_name,
          regex("mealtime", ignore_case = TRUE)
        )
    )
  ) %>%
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
          "^(zung|eat|edi|ei|eisc|lsi|beck|bsq|qewp|debq|pfs|cfq)(_|$)",
          ignore_case = TRUE
        )
      ) ~ "questionnaire",
      
      file_type == "intake" &
        str_detect(
          variable_name,
          regex(
            "(_cho_g$|_fat_g$|_pro_g$|_protein_g$|_carb_g$|_carbohydrate_g$|_cho_cal$|_fat_cal$|_pro_cal$|_protein_cal$|_carb_cal$|_carbohydrate_cal$|perc_cho|perc_fat|perc_pro|perc_protein|perc_carb|perc_carbohydrate|percent_cho|percent_fat|percent_pro|percent_protein|percent_carb|percent_carbohydrate|^g_cho$|^g_fat$|^g_pro$|^g_protein$|^g_carb$|^g_carbohydrate$)",
            ignore_case = TRUE
          )
        ) ~ "macronutrient",
      
      file_type == "intake" &
        str_detect(
          variable_name,
          regex(
            "(_g$|^g_|_cal$|^cal_|kcal|gram|intake)",
            ignore_case = TRUE
          )
        ) ~ "intake",
      
      str_detect(
        variable_name,
        regex(
          "^(age|agegroup|sex|gender|race|ethnicity|ethnic|bmi|height|height_cm|height_in|weight|perc_ideal_bw)$",
          ignore_case = TRUE
        )
      ) ~ "demographic",
      
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

mealtime_check <- variable_groups %>%
  filter(
    study_folder == "1992_eatdis_deprivation",
    str_detect(
      file_name,
      regex("mealtime", ignore_case = TRUE)
    )
  )

if (nrow(mealtime_check) == 0) {
  cat("GOOD: 1992 EatDis mealtime dataset is excluded.\n")
} else {
  cat("WARNING: 1992 EatDis mealtime dataset is still present.\n")
}

eatdis_intake_n <- variable_groups %>%
  filter(
    study_folder == "1991_eatdis",
    variable_group == "intake"
  ) %>%
  nrow()

if (eatdis_intake_n > 0) {
  cat(
    "GOOD: 1991 EatDis has",
    eatdis_intake_n,
    "intake variables.\n"
  )
} else {
  cat(
    "WARNING: 1991 EatDis has no detected intake variables.\n"
  )
}

eatdis_macro_n <- variable_groups %>%
  filter(
    study_folder == "1991_eatdis",
    variable_group == "macronutrient"
  ) %>%
  nrow()

if (eatdis_macro_n > 0) {
  cat(
    "GOOD: 1991 EatDis has",
    eatdis_macro_n,
    "macronutrient variables.\n"
  )
} else {
  cat(
    "WARNING: 1991 EatDis has no detected macronutrient variables.\n"
  )
}

ivig_intake_n <- variable_groups %>%
  filter(
    study_folder == "1991_ivig_preload",
    variable_group == "intake"
  ) %>%
  nrow()

if (ivig_intake_n > 0) {
  cat(
    "GOOD: 1991 IVIG preload has",
    ivig_intake_n,
    "intake variables.\n"
  )
} else {
  cat(
    "WARNING: 1991 IVIG preload has no detected intake variables.\n"
  )
}

ivig_macro_n <- variable_groups %>%
  filter(
    study_folder == "1991_ivig_preload",
    variable_group == "macronutrient"
  ) %>%
  nrow()

if (ivig_macro_n > 0) {
  cat(
    "GOOD: 1991 IVIG preload has",
    ivig_macro_n,
    "macronutrient variables.\n"
  )
} else {
  cat(
    "WARNING: 1991 IVIG preload has no detected macronutrient variables.\n"
  )
}

demographic_n <- variable_groups %>%
  filter(
    variable_group == "demographic"
  ) %>%
  nrow()

if (demographic_n > 0) {
  cat(
    "GOOD: Demographic variables were detected:",
    demographic_n,
    "variables.\n"
  )
} else {
  cat(
    "WARNING: No demographic variables were detected.\n"
  )
}

sex_check <- variable_groups %>%
  filter(
    str_to_lower(variable_name) %in% c(
      "sex",
      "gender"
    )
  )

if (
  nrow(sex_check) > 0 &&
  all(sex_check$variable_group == "demographic")
) {
  cat(
    "GOOD: All sex/gender variables are classified as demographic.\n"
  )
} else {
  cat(
    "WARNING: Some sex/gender variables are not classified as demographic.\n"
  )
}

design_check <- variable_groups %>%
  filter(
    variable_name %in% c(
      "group",
      "group_anr_split",
      "cafeteria_study",
      "foodpref_study",
      "smell_study",
      "restrict_cond",
      "session",
      "timepoint",
      "id"
    ),
    variable_group == "demographic"
  )

if (nrow(design_check) == 0) {
  cat(
    "GOOD: Study/design variables are not classified as demographic.\n"
  )
} else {
  cat(
    "WARNING: Some study/design variables are classified as demographic.\n"
  )
}

demographic_check <- variable_groups %>%
  filter(
    variable_group == "demographic"
  ) %>%
  select(
    study_folder,
    file_name,
    variable_name,
    file_type,
    variable_group
  ) %>%
  arrange(
    study_folder,
    file_name,
    variable_name
  )

print(
  demographic_check,
  n = Inf,
  width = Inf
)