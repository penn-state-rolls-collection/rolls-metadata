cat("\n===== QUICK SCRIPT 05 CHECK =====\n")

# 1992 EatDis mealtime
mealtime_check <- variable_groups %>%
  filter(
    study_folder == "1992_eatdis_deprivation",
    str_detect(file_name, regex("mealtime", ignore_case = TRUE))
  )

if (nrow(mealtime_check) == 0) {
  cat("GOOD: 1992 EatDis mealtime dataset is excluded.\n")
} else {
  cat("WARNING: 1992 EatDis mealtime dataset is still present.\n")
}

# 1991 EatDis intake
eatdis_intake_n <- variable_groups %>%
  filter(
    study_folder == "1991_eatdis",
    variable_group == "intake"
  ) %>%
  nrow()

if (eatdis_intake_n > 0) {
  cat("GOOD: 1991 EatDis has", eatdis_intake_n, "intake variables.\n")
} else {
  cat("WARNING: 1991 EatDis has no detected intake variables.\n")
}

# 1991 EatDis macronutrients
eatdis_macro_n <- variable_groups %>%
  filter(
    study_folder == "1991_eatdis",
    variable_group == "macronutrient"
  ) %>%
  nrow()

if (eatdis_macro_n > 0) {
  cat("GOOD: 1991 EatDis has", eatdis_macro_n, "macronutrient variables.\n")
} else {
  cat("WARNING: 1991 EatDis has no detected macronutrient variables.\n")
}

# 1991 IVIG intake
ivig_intake_n <- variable_groups %>%
  filter(
    study_folder == "1991_ivig_preload",
    variable_group == "intake"
  ) %>%
  nrow()

if (ivig_intake_n > 0) {
  cat("GOOD: 1991 IVIG preload has", ivig_intake_n, "intake variables.\n")
} else {
  cat("WARNING: 1991 IVIG preload has no detected intake variables.\n")
}

# 1991 IVIG macronutrients
ivig_macro_n <- variable_groups %>%
  filter(
    study_folder == "1991_ivig_preload",
    variable_group == "macronutrient"
  ) %>%
  nrow()

if (ivig_macro_n > 0) {
  cat("GOOD: 1991 IVIG preload has", ivig_macro_n, "macronutrient variables.\n")
} else {
  cat("WARNING: 1991 IVIG preload has no detected macronutrient variables.\n")
}

cat("=================================\n")