# Prepare data ------------------------------------------------------------

ready_data <- function(data) {
  # Removing participants who did not complete 2 or more diet questionnaires
  data <- data %>%
    filter(p20077 >= 2) %>%
    mutate(id = 1:n(), .before = everything())
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD10 codes)
  data <- data %>%
    separate_wider_delim(p41270,
      delim = "|",
      names = paste0("p41270var_a", 0:258), too_few = "debug"
    )
  data <- data %>%
    select(starts_with("p41270"), starts_with("p41280")) %>%
    select_if(~ !all(is.na(.))) %>%
    bind_cols(data %>% select(-starts_with("p41270"), -starts_with("p41280")))
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD9 codes)
  data <- data %>%
    separate_wider_delim(p41271,
      delim = "|",
      names = paste0("p41271var_a", 0:46), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS4 codes)
  data <- data %>%
    separate_wider_delim(p41272,
      delim = "|",
      names = paste0("p41272var_a", 0:125), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS3 codes)
  data <- data %>%
    separate_wider_delim(p41273,
      delim = "|",
      names = paste0("p41273var_a", 0:15), too_few = "debug"
    )
  return(data)
}

# Find liver cancer cases -------------------------------------------------

icd10_longer_subset <- function(data) {
  data %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(
      cols = matches("_a[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
}

cancer_longer_subset <- function(data) {
  data %>%
    select(matches("p40006|p40005|id")) %>%
    pivot_longer(
      cols = matches("_i[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
}

icd10_hcc <- function(data) {
  data %>%
    # TODO: I don't this does what you think it's doing. Need to assess in RAP
    mutate(
      icd10_hcc_date = ifelse(str_detect(p41270var, "C22.0"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_hcc_date = as.Date(icd10_hcc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(icd10_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, icd10_hcc_date)
}

icd10_icc <- function(data) {
  data %>%
    mutate(
      icd10_icc_date = ifelse(str_detect(p41270var, "C22.1"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_icc_date = as.Date(icd10_icc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(icd10_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, icd10_icc_date)
}

cancer_hcc <- function(data) {
  data %>%
    mutate(
      cancer_hcc_date = ifelse(str_detect(p40006, "C22.0"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_hcc_date = as.Date(cancer_hcc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(cancer_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, cancer_hcc_date)
}

cancer_icc <- function(data) {
  data %>%
    mutate(
      cancer_icc_date = ifelse(str_detect(p40006, "C22.1"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_icc_date = as.Date(cancer_icc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(cancer_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, cancer_icc_date)
}

# Define baseline date ----------------------------------------------------

remove_timestamp <- function(data) {
  # Removing specific time stamp from date of completed questionnaires:
  data %>%
    mutate(across(
      c(
        p105010_i0,
        p105010_i1,
        p105010_i2,
        p105010_i3,
        p105010_i4
      ), ~ substr(.x, 1, 10)
    ))
}

baseline_date <- function(data) {
  baseline_start_date <- data %>%
    select(starts_with("p105010_i"), id) %>%
    pivot_longer(
      cols = starts_with("p105010_i"),
      names_to = "questionnaire",
      values_to = "completion_date"
    ) %>%
    filter(!is.na(completion_date)) %>%
    group_by(id) %>%
    arrange(completion_date) %>%
    mutate(last_questionnaire_date = lag(completion_date)) %>%
    filter(!is.na(last_questionnaire_date)) %>%
    filter(is.na(lead(completion_date))) %>%
    rename(baseline_start_date = completion_date) %>%
    ungroup()
  data <- data %>%
    left_join(baseline_start_date %>% select(id, baseline_start_date), by = "id")
  return(data)
}
