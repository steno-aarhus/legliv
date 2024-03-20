icd10_liver_unsp <- function(data) {
    icd10_liver_unsp <- icd10_subset %>%
        mutate(
            icd10_liver_unsp_date = ifelse(str_detect(p41270var, "C22.9"),
                                           as.character(c_across(starts_with("p41280"))),
                                           NA
            ),
            icd10_liver_unsp_date = as.Date(icd10_liver_unsp_date, format = "%Y-%m-%d")
        )
    first_non_na_liver_unsp <- icd10_liver_unsp %>%
        filter(!is.na(icd10_liver_unsp_date)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
    data <- data %>%
        left_join(first_non_na_liver_unsp %>% select(id, icd10_liver_unsp_date), by = "id")
    return(data)
}
data <- icd10_liver_unsp(data)

cancer_liver_unsp <- function(data) {
    cancer_liver_unsp <- cancer_subset %>%
        mutate(
            cancer_liver_unsp_date = ifelse(str_detect(p40006, "C22.9"),
                                            as.character(c_across(starts_with("p40005"))),
                                            NA
            ),
            cancer_liver_unsp_date = as.Date(cancer_liver_unsp_date, format = "%Y-%m-%d")
        )
    first_non_na_liver_unsp_c <- cancer_liver_unsp %>%
        filter(!is.na(cancer_liver_unsp_date)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
    data <- data %>%
        left_join(first_non_na_liver_unsp_c %>% select(id, cancer_liver_unsp_date), by = "id")
    return(data)
}
data <- cancer_liver_unsp(data)

icd10_liver_other <- function(data) {
    icd10_liver_other <- icd10_subset %>%
        mutate(
            icd10_liver_other_date = ifelse(str_detect(p41270var, "C22.7"),
                                            as.character(c_across(starts_with("p41280"))),
                                            NA
            ),
            icd10_liver_other_date = as.Date(icd10_liver_other_date, format = "%Y-%m-%d")
        )
    first_non_na_liver_other <- icd10_liver_other %>%
        filter(!is.na(icd10_liver_other_date)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
    data <- data %>%
        left_join(first_non_na_liver_other %>% select(id, icd10_liver_other_date), by = "id")
    return(data)
}
data <- icd10_liver_other(data)

cancer_liver_other <- function(data) {
    cancer_liver_other <- cancer_subset %>%
        mutate(
            cancer_liver_other_date = ifelse(str_detect(p40006, "C22.7"),
                                             as.character(c_across(starts_with("p40005"))),
                                             NA
            ),
            cancer_liver_other_date = as.Date(cancer_liver_other_date, format = "%Y-%m-%d")
        )
    first_non_na_liver_other_c <- cancer_liver_other %>%
        filter(!is.na(cancer_liver_other_date)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
    data <- data %>%
        left_join(first_non_na_liver_other_c %>% select(id, cancer_liver_other_date), by = "id")
    return(data)
}
data <- cancer_liver_other(data)

remove(icd10_subset)
remove(cancer_subset)

data <- data %>%
    mutate(
        liver_cancer = if_else(is.na(icd10_hcc_date) & is.na(icd10_icc_date) & is.na(cancer_hcc_date) & is.na(cancer_icc_date), "No", "Yes"),
        liver_cancer_unsp = if_else(is.na(icd10_liver_unsp_date) & is.na(cancer_liver_unsp_date), "No", "Yes"),
        liver_cancer_other = if_else(is.na(icd10_liver_other_date) & is.na(cancer_liver_other_date), "No", "Yes")
    )

remove_two_liver_cancer <- function(data) {
    two_liver_cancer <- data %>%
        filter(liver_cancer == "Yes" & liver_cancer_unsp == "Yes" |
                   liver_cancer == "Yes" & liver_cancer_other == "Yes") %>%
        mutate(
            icd10_liver_unsp_date_new = ifelse(!is.na(icd10_liver_unsp_date), NA, icd10_liver_unsp_date),
            icd10_liver_other_date_new = ifelse(!is.na(icd10_liver_other_date), NA, icd10_liver_other_date),
            cancer_liver_unsp_date_new = ifelse(!is.na(cancer_liver_unsp_date), NA, cancer_liver_unsp_date),
            cancer_liver_other_date_new = ifelse(!is.na(cancer_liver_other_date), NA, cancer_liver_other_date)
        )
    data <- data %>%
        left_join(two_liver_cancer %>% select(id, icd10_liver_unsp_date_new, icd10_liver_other_date_new, cancer_liver_unsp_date_new, cancer_liver_other_date_new), by = "id")
    return(data)
}
data <- remove_two_liver_cancer(data)


status <- function(data) {
    # Creating status, status date and status age:
    data <- data %>%
        mutate(
            earliest_date = pmin(dead_date, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
                coalesce(as.Date("2022-12-31")),
            status = case_when(
                earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == cancer_liver_unsp_date_new & earliest_date > baseline_start_date ~ "Censored",
                earliest_date == cancer_liver_other_date_new & earliest_date > baseline_start_date ~ "Censored",
                earliest_date == icd10_liver_unsp_date_new & earliest_date > baseline_start_date ~ "Censored",
                earliest_date == icd10_liver_other_date_new & earliest_date > baseline_start_date ~ "Censored",
                earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
                earliest_date == dead_date ~ "Censored",
                TRUE ~ "Censored"
            ),
            status_date = earliest_date,
            status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
            status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
            study_time = status_age - age_at_baseline
        )
    return(data)
}
data <- status(data)

remove_liver_before <- function(data) {
    data_liver_before <- data %>%
        filter(
            cancer_hcc_date <= baseline_start_date |
                cancer_icc_date <= baseline_start_date |
                icd10_hcc_date <= baseline_start_date |
                icd10_icc_date <= baseline_start_date |
                cancer_liver_unsp_date <= baseline_start_date |
                cancer_liver_other_date <= baseline_start_date |
                icd10_liver_unsp_date <= baseline_start_date |
                icd10_liver_other_date <= baseline_start_date
        )
    data <- data %>%
        anti_join(data_liver_before %>% select(id, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date,
                                               cancer_liver_unsp_date, cancer_liver_other_date, icd10_liver_unsp_date, icd10_liver_other_date),
                  by = "id")
    return(data)
}
data <- remove_liver_before(data)
