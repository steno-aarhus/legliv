library(tidyverse)
library(lubridate) # For creating baseline age
targets::tar_config_set(store = here::here("_targets"))

data <- targets::tar_read(data_as_baseline)

# Diseases before baseline ------------------------------------------------

# # # TODO: Repeat the same pattern used in `functions.R` to the functions below here.
# icd10_diabetes <- function(data) {
#   diabetes <- data %>%
#     select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
#     # TODO: This does not do what you think it does.
#     mutate(
#       diabetes_ins_non = if_else(
#         rowSums(across(starts_with("p41270var_a"), ~ grepl("E11", .x)) &
#           across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
#         "Yes",
#         "No"
#       ),
#       diabetes_ins = if_else(
#         rowSums(across(starts_with("p41270var_a"), ~ grepl("E10", .x)) &
#           across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
#         "Yes",
#         "No"
#       )
#     )
#   data <- data %>%
#     # TODO: These left_joins aren't necessary.
#     left_join(diabetes %>% select(id, diabetes_ins_non, diabetes_ins), by = "id")
#   return(data)
# }
# data <- icd10_diabetes(data)
#
# icd9_diabetes <- function(data) {
#   diabetes <- data %>%
#     select(starts_with("p41271"), starts_with("p41281"), baseline_start_date, id) %>%
#     mutate(
#       diabetes_icd9 = if_else(
#         rowSums(across(starts_with("p41271var_a"), ~ grepl("250", .x)) &
#           across(starts_with("p41281_a"), ~ .x < baseline_start_date)) > 0,
#         "Yes",
#         "No"
#       )
#     )
#   data <- data %>%
#     left_join(diabetes %>% select(id, diabetes_icd9), by = "id")
#   return(data)
# }
# data <- icd9_diabetes(data)

# icd10_nafl <- function(data) {
#   nafl <- data %>%
#     select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
#     mutate(
#       nafl_icd10 = if_else(
#         rowSums(across(starts_with("p41270var_a"), ~ grepl("K76.0", .x)) &
#           across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
#         "Yes",
#         "No"
#       )
#     )
#   data <- data %>%
#     left_join(nafl %>% select(id, nafl_icd10), by = "id")
#   return(data)
# }
# data <- icd10_nafl(data)
#
# icd10_nash <- function(data) {
#   nash <- data %>%
#     select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
#     mutate(
#       nash_icd10 = if_else(
#         rowSums(across(starts_with("p41270var_a"), ~ grepl("K75.8", .x)) &
#           across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
#         "Yes",
#         "No"
#       )
#     )
#   data <- data %>%
#     left_join(nash %>% select(id, nash_icd10), by = "id")
#   return(data)
# }
# data <- icd10_nash(data)

# data <- data |>
#   mutate(
#     diabetes = if_else(diabetes_ins_non == "No" & diabetes_ins == "No" & diabetes_icd9 == "No", "No", "Yes"),
#     nafld = if_else(nafl_icd10 == "No" & nash_icd10 == "No", "No", "Yes")
#   )

icd10_cholelith <- function(data) {
  cholelith <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      cholelith_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K80", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cholelith %>% select(id, cholelith_icd10), by = "id")
  return(data)
}
data <- icd10_cholelith(data)

icd9_cholelith <- function(data) {
  cholelith <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), baseline_start_date, id) %>%
    mutate(
      cholelith_icd9 = if_else(
        rowSums(across(starts_with("p41271var_a"), ~ grepl("574", .x)) &
          across(starts_with("p41281_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cholelith %>% select(id, cholelith_icd9), by = "id")
  return(data)
}
data <- icd9_cholelith(data)

# Cystectomy before baseline ----------------------------------------------
opcs4_cystectomy <- function(data) {
  cystect <- data %>%
    select(starts_with("p41272"), starts_with("p41282"), baseline_start_date, id) %>%
    mutate(
      cystectomy_opcs4 = if_else(
        rowSums(across(starts_with("p41272var_a"), ~ grepl("J18", .x)) &
          across(starts_with("p41282_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cystect %>% select(id, cystectomy_opcs4), by = "id")
  return(data)
}
data <- opcs4_cystectomy(data)

opcs3_cystectomy <- function(data) {
  cystect <- data %>%
    select(starts_with("p41273"), starts_with("p41283"), baseline_start_date, id) %>%
    mutate(
      cystectomy_opcs3 = if_else(
        rowSums(across(starts_with("p41273var_a"), ~ grepl("522", .x)) &
          across(starts_with("p41283_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cystect %>% select(id, cystectomy_opcs3), by = "id")
  return(data)
}
data <- opcs3_cystectomy(data)

# Removing liver cancers before baseline date -----------------------------
remove_liver_before <- function(data) {
  data_liver_before <- data %>%
    filter(
      cancer_hcc_date <= baseline_start_date |
        cancer_icc_date <= baseline_start_date |
        icd10_hcc_date <= baseline_start_date |
        icd10_icc_date <= baseline_start_date
    )
  data <- data %>%
    anti_join(data_liver_before %>% select(id, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date),
      by = "id"
    )
  return(data)
}
data <- remove_liver_before(data)

data_liver <- data %>%
  filter(status == "Liver cancer")

data <- data %>%
  mutate(
    cholelith = if_else(cholelith_icd10 == "No" & cholelith_icd9 == "No", "No", "Yes"),
    cystectomy = if_else(cystectomy_opcs4 == "No" & cystectomy_opcs3 == "No", "No", "Yes"),
    gall_disease = if_else(cholelith == "No" & cystectomy == "No", "No", "Yes"),
    high_alcohol = if_else(sex == "Male" & alcohol_daily < 56 | sex == "Female" & alcohol_daily < 42, "No", "Yes")
  )
data2 <- data
# data <- data %>%
#   select(id, sex, tdi, bmi, spouse, wc, exercise, education, smoking, trigly, glucose, hdl,
#          med_men, med_women, gall_disease, typical_diet,
#          legume_daily_15, red_meat_daily_15, proc_meat_daily_15,
#          other_foods_daily, total_weight_food_daily, alcohol_daily,
#          status, study_time, status_age, age_at_baseline)

# Perform imputation with mice
# imputed_data <- mice(data, m = 5, maxit = 5, defaultMethod = c("pmm", "logreg", "polyreg", "polr"))
#
# data <- complete(imputed_data)
#
# data %>% describe()

data <- data %>%
  mutate(
    high_trigly = if_else(trigly >= 1.7, "Yes", "No"),
    low_hdl = if_else(hdl <= 1.036 & sex == "Male" | hdl <= 1.295 & sex == "Female", "Yes", "No"),
    chol_med_men = ifelse(grepl("Cholesterol lowering medication", med_men), "Yes", "No"),
    chol_med_women = ifelse(grepl("Cholesterol lowering medication", med_women), "Yes", "No"),
    chol_med = if_else(chol_med_men == "Yes" | chol_med_women == "Yes", "Yes", "No"),
    low_hdl_chol_med = if_else(low_hdl == "No" & chol_med == "No", "No", "Yes"),
    bp_med_men = ifelse(grepl("Blood pressure medication", med_men), "Yes", "No"),
    bp_med_women = ifelse(grepl("Blood pressure medication", med_women), "Yes", "No"),
    bp_med = if_else(bp_med_men == "Yes" | bp_med_women == "Yes", "Yes", "No"),
    high_wc = if_else(sex == "Male" & wc >= 94 | sex == "Female" & wc >= 80, "Yes", "No"),
    high_bmi = if_else(bmi >= 30, "Yes", "No"),
    high_bmi_wc = if_else(high_wc == "No" & high_bmi == "No", "No", "Yes"),
    bs_high = if_else(glucose >= 39, "Yes", "No"),
    ins_med_men = ifelse(grepl("Insulin", med_men), "Yes", "No"),
    ins_med_women = ifelse(grepl("Insulin", med_women), "Yes", "No"),
    ins_med = if_else(ins_med_men == "Yes" | ins_med_women == "Yes", "Yes", "No"),
    high_bs = if_else(bs_high == "No" & ins_med == "No", "No", "Yes"),
    )

data <- data %>%
  mutate(
    met_synd = case_when(
      rowSums(is.na(select(., c("high_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs")))) > 0 ~ NA_character_,
      rowSums(select(., c("high_bmi_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs")) == "Yes", na.rm = TRUE) >= 3 ~ "Yes",
      TRUE ~ "No"
      )
  )

data %>% group_by(met_synd) %>% summarise(n())
