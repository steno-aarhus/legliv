cancer_register <- function(data) {
  data_cancer_before <- data %>%
    select(starts_with("p40006"), starts_with("p40005"), baseline_start_date, id) %>%
    mutate(
      cancer_before = if_else(
        rowSums(across(starts_with("p40006_i"), ~ grepl("C\\d{2}", .x)) &
          across(starts_with("p40005_i"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(data_cancer_before %>% select(id, cancer_before), by = "id")
  return(data)
}
data <- cancer_register(data)

cancer_icd10_register <- function(data) {
  data_cancer_before_icd10 <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      cancer_before_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("C\\d{2}", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(data_cancer_before_icd10 %>% select(id, cancer_before_icd10), by = "id")
  return(data)
}
data <- cancer_icd10_register(data)

icd10_inflam_liver <- function(data) {
  inflam_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      inflam_liver_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K75\\.[0-79]", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(inflam_liver %>% select(id, inflam_liver_icd10), by = "id")
  return(data)
}
data <- icd10_inflam_liver(data)

icd10_alc_liver <- function(data) {
  alc_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      alc_liver = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K70", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(alc_liver %>% select(id, alc_liver), by = "id")
  return(data)
}
data <- icd10_alc_liver(data)

icd10_cirr_liver <- function(data) {
  cirr_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      cirr_liver = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K74", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cirr_liver %>% select(id, cirr_liver), by = "id")
  return(data)
}
data <- icd10_cirr_liver(data)

icd10_viral_hepatitis <- function(data) {
  viral_hepatitis <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      viral_hepatitis_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("B18", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(viral_hepatitis %>% select(id, viral_hepatitis_icd10), by = "id")
  return(data)
}
data <- icd10_viral_hepatitis(data)

icd9_viral_hepatitis <- function(data) {
  viral_hepatitis <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), baseline_start_date, id) %>%
    mutate(
      viral_hepatitis_icd9 = if_else(
        rowSums(across(starts_with("p41271var_a"), ~ grepl("5733", .x)) &
          across(starts_with("p41281_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(viral_hepatitis %>% select(id, viral_hepatitis_icd9), by = "id")
  return(data)
}
data <- icd9_viral_hepatitis(data)

icd10_other_liver <- function(data) {
  other_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      other_liver_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K76\\.[1-9]", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(other_liver %>% select(id, other_liver_icd10), by = "id")
  return(data)
}
data <- icd10_other_liver(data)

icd10_tox_liver <- function(data) {
  tox_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      tox_liver_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K71", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(tox_liver %>% select(id, tox_liver_icd10), by = "id")
  return(data)
}
data <- icd10_tox_liver(data)

icd10_fail_liver <- function(data) {
  fail_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      fail_liver_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K72", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(fail_liver %>% select(id, fail_liver_icd10), by = "id")
  return(data)
}
data <- icd10_fail_liver(data)

icd10_chronic_liver <- function(data) {
  chronic_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      chronic_liver_icd10 = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K73", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(chronic_liver %>% select(id, chronic_liver_icd10), by = "id")
  return(data)
}
data <- icd10_chronic_liver(data)

# Variables for secondary analyses ----------------------------------------

data <- data %>%
  mutate(
    misreporter = if_else(sex == "Male" & total_energy_food_daily < 3200 | sex == "Male" & total_energy_food_daily > 16800 |
      sex == "Female" & total_energy_food_daily < 2000 | sex == "Female" & total_energy_food_daily > 14000,
    "Yes", "No"
    ),
    high_alcohol = if_else(sex == "Male" & alcohol_daily < 30 | sex == "Female" & alcohol_daily < 20, "No", "Yes"),
    high_wc = if_else(sex == "Male" & wc >= 102 | sex == "Female" & wc >= 88, "Yes", "No"),
    liver_disease = if_else(inflam_liver_icd10 == "No" & alc_liver == "No" & cirr_liver == "No" &
      viral_hepatitis == "No" & tox_liver_icd10 == "No" &
      fail_liver_icd10 == "No" & chronic_liver_icd10 == "No", "No", "Yes"),
    liver_disease = ifelse(nafld == "Yes", "No", liver_disease),
    cancer_before_baseline = if_else(cancer_before == "No" & cancer_before_icd10 == "No", "No", "Yes"),
    smoking_ever_never = if_else(smoking == "Never", "No", "Yes"),
    viral_hepatitis = if_else(viral_hepatitis_icd10 == "No" & viral_hepatitis_icd9 == "No", "No", "Yes"),
  )

# Stratified data for sex, waist circumference, and diabetes ----------


stratify_prepare <- function(data) {
  data_wc_high <- data %>%
    filter(high_wc == "Yes")
  data_wc_low <- data %>%
    filter(high_wc == "No")
  data_diabetes_yes <- data %>%
    filter(diabetes == "Yes")
  data_diabetes_no <- data %>%
    filter(diabetes == "No")
  data_men <- data %>%
    filter(sex == "Male")
  data_women <- data %>%
    filter(sex == "Female")
  return(list(
    data_wc_high = data_wc_high,
    data_wc_low = data_wc_low,
    data_diabetes_yes = data_diabetes_yes,
    data_diabetes_no = data_diabetes_no,
    data_men = data_men,
    data_women = data_women
  ))
}
data_list <- stratify_prepare(data)
data_wc_high <- data_list$data_wc_high
data_wc_low <- data_list$data_wc_low
data_diabetes_yes <- data_list$data_diabetes_yes
data_diabetes_no <- data_list$data_diabetes_no
data_men <- data_list$data_men
data_women <- data_list$data_women


# Liver cancer stratified by cancer type ----------------------------------


cancer_is_hcc <- function(data) {
  data_hcc <- data %>%
    mutate(
      earliest_date = pmin(dead_date, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
        coalesce(as.Date("2022-12-31")),
      status = case_when(
        earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == dead_date & earliest_date > baseline_start_date & dead_cause == "C22.0 Liver cell carcinoma" ~ "Liver cancer",
        earliest_date == dead_date & earliest_date > baseline_start_date & dead_cause == "C22.1 Intrahepatic bile duct carcinoma" ~ "Censored",
        earliest_date == dead_date ~ "Censored",
        TRUE ~ "Censored"
      ),
      status_date = earliest_date,
      status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
      status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      time = status_age - age_at_baseline
    ) %>%
    select(-earliest_date)
  return(data_hcc)
}
data_hcc <- cancer_is_hcc(data)

cancer_is_icc <- function(data) {
  data_icc <- data %>%
    mutate(
      earliest_date = pmin(dead_date, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
        coalesce(as.Date("2022-12-31")),
      status = case_when(
        earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == dead_date & earliest_date > baseline_start_date & dead_cause == "C22.0 Liver cell carcinoma" ~ "Censored",
        earliest_date == dead_date & earliest_date > baseline_start_date & dead_cause == "C22.1 Intrahepatic bile duct carcinoma" ~ "Liver cancer",
        earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == dead_date ~ "Censored",
        TRUE ~ "Censored"
      ),
      status_date = earliest_date,
      status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
      status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      time = status_age - age_at_baseline
    ) %>%
    select(-earliest_date)
  return(data_icc)
}
data_icc <- cancer_is_icc(data)


# Remove other liver diseases before baseline -----------------------------

data_no_liver_before <- data %>%
  filter(liver_disease == "No")


# Removing all cancers before baseline ------------------------------------


data_no_cancer <- data %>%
  filter(cancer_before_baseline == "No")



# Remove heavy alcohol drinkers -------------------------------------------


data_no_alcohol <- data %>%
  filter(high_alcohol == "No")
data_no_alcohol_hcc <- data_hcc %>%
  filter(high_alcohol == "No")
data_no_alcohol_icc <- data_icc %>%
  filter(high_alcohol == "No")

# Remove untypical diet ---------------------------------------------------


data_typical_diet <- data %>%
  filter(typical_diet == "Yes")



# Variying number of diet questionnaires completed ------------------------

filter_ques_comp_n <- function(data) {
  data_ques_3 <- data %>%
    filter(p20077 >= 3)
  data_ques_4 <- data %>%
    filter(p20077 >= 4)
  data_ques_5 <- data %>%
    filter(p20077 >= 5)
  return(list(
    data_ques_3 = data_ques_3,
    data_ques_4 = data_ques_4,
    data_ques_5 = data_ques_5
  ))
}
ques_comp_list <- filter_ques_comp_n(data)
data_ques_3 <- ques_comp_list$data_ques_3
data_ques_4 <- ques_comp_list$data_ques_4
data_ques_5 <- ques_comp_list$data_ques_5


# Removing intake misreporters --------------------------------------------


data_no_misreporter <- data %>%
  filter(misreporter == "No")


data_filtered <- data %>%
  filter(
    cancer_before_baseline == "No" &
      liver_disease == "No" &
      misreporter == "No" &
      high_alcohol == "No"
  )

data_smoking <- data %>%
  filter(smoking_ever_never == "No")


data <- data %>%
  mutate(baseline_start_date = ymd(baseline_start_date),  # Convert to date format using lubridate
         baseline_2 = baseline_start_date + years(2))     # Add 2 years

data <- data %>%
  mutate(
    earliest_date = pmin(dead_date, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
      coalesce(as.Date("2022-12-31")),
    status = case_when(
      earliest_date == cancer_hcc_date & earliest_date > baseline_2 ~ "Liver cancer",
      earliest_date == cancer_icc_date & earliest_date > baseline_2 ~ "Liver cancer",
      earliest_date == icd10_hcc_date & earliest_date > baseline_2 ~ "Liver cancer",
      earliest_date == icd10_icc_date & earliest_date > baseline_2 ~ "Liver cancer",
      earliest_date == dead_date & earliest_date > baseline_2 & dead_cause == "C22.0 Liver cell carcinoma" ~ "Liver cancer",
      earliest_date == dead_date & earliest_date > baseline_2 & dead_cause == "C22.1 Intrahepatic bile duct carcinoma" ~ "Liver cancer",
      earliest_date == l2fu_d & earliest_date > baseline_2 ~ "Censored",
      earliest_date == dead_date ~ "Censored",
      TRUE ~ "Censored"
    ),
    status_date = earliest_date,
    status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
    status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
    study_time = status_age - age_at_baseline
  )
