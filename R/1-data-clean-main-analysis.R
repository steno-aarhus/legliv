library(tidyverse)
library(lubridate) # For creating baseline age
targets::tar_config_set(store = here::here("_targets"))

data <- targets::tar_read(data_as_baseline)

# Diseases before baseline ------------------------------------------------

# # TODO: Repeat the same pattern used in `functions.R` to the functions below here.
icd10_diabetes <- function(data) {
  diabetes <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    # TODO: This does not do what you think it does.
    mutate(
      diabetes_ins_non = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("E11", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      ),
      diabetes_ins = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("E10", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    # TODO: These left_joins aren't necessary.
    left_join(diabetes %>% select(id, diabetes_ins_non, diabetes_ins), by = "id")
  return(data)
}
data <- icd10_diabetes(data)

icd9_diabetes <- function(data) {
  diabetes <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), baseline_start_date, id) %>%
    mutate(
      diabetes_icd9 = if_else(
        rowSums(across(starts_with("p41271var_a"), ~ grepl("250", .x)) &
          across(starts_with("p41281_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(diabetes %>% select(id, diabetes_icd9), by = "id")
  return(data)
}
data <- icd9_diabetes(data)

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

icd10_nafl <- function(data) {
  nafl <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      nafl_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K76.0", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(nafl %>% select(id, nafl_icd10), by = "id")
  return(data)
}
data <- icd10_nafl(data)

icd10_nash <- function(data) {
  nash <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      nash_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K75.8", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(nash %>% select(id, nash_icd10), by = "id")
  return(data)
}
data <- icd10_nash(data)

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

data <- data |>
  mutate(
    diabetes = if_else(diabetes_ins_non == "No" & diabetes_ins == "No" & diabetes_icd9 == "No", "No", "Yes"),
    cholelith = if_else(cholelith_icd10 == "No" & cholelith_icd9 == "No", "No", "Yes"),
    cystectomy = if_else(cystectomy_opcs4 == "No" & cystectomy_opcs3 == "No", "No", "Yes"),
    nafld = if_else(nafl_icd10 == "No" & nash_icd10 == "No", "No", "Yes")
  )

data <- data %>%
  mutate(
    gall_disease = if_else(cholelith == "No" & cystectomy == "No", "No", "Yes"),
    high_alcohol = if_else(sex == "Male" & alcohol_daily < 56 | sex == "Female" & alcohol_daily < 42, "No", "Yes")
  )

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
