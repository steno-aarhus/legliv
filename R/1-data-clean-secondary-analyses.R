stratify_prepare1 <- function(data) {
    data_wc_high <- data %>%
        filter(sex == "Male" & wc >= 102 | sex == "Female" & wc >= 88)
    data_wc_low <- data %>%
        filter(sex == "Male" & wc < 102 | sex == "Female" & wc < 88)
    data_diabetes_yes <- data %>%
        filter(diabetes == "Yes")
    data_diabetes_no <- data %>%
        filter(diabetes == "No")
    data_men <- data %>%
        filter(sex == "Male")
    data_women <- data %>%
        filter(sex == "Female")
    data_alcohol <- data %>%
        filter(
            sex == "Male" & alcohol_daily < 30 |sex == "Female" & alcohol_daily < 20
        )
    data_typical_diet <- data %>%
        filter(typical_diet == "Yes")
    return(list(
        data_wc_high = data_wc_high,
        data_wc_low = data_wc_low,
        data_diabetes_yes = data_diabetes_yes,
        data_diabetes_no = data_diabetes_no,
        data_men = data_men,
        data_women = data_women,
        data_alcohol = data_alcohol,
        data_typical_diet = data_typical_diet
    ))
}
data_list <- stratify_prepare1(data)
data_wc_high <- data_list$data_wc_high
data_wc_low <- data_list$data_wc_low
data_diabetes_yes <- data_list$data_diabetes_yes
data_diabetes_no <- data_list$data_diabetes_no
data_men <- data_list$data_men
data_women <- data_list$data_women
data_alcohol <- data_list$data_alcohol
data_typical_diet <-  data_list$data_typical_diet

stratify_prepare2 <- function(data) {
    data_hcc <- data %>%
        mutate(
            earliest_date = pmin(dead_date, cancer_hcc_date, icd10_hcc_date, l2fu_d, na.rm = TRUE) %>%
                coalesce(as.Date("2022-12-31")),
            status = case_when(
                earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
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
    return(data_hcc)
}
data_hcc <- stratify_prepare2(data)

stratify_prepare3 <- function(data) {
    data_icc <- data %>%
        mutate(
            earliest_date = pmin(dead_date, cancer_icc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
                coalesce(as.Date("2022-12-31")),
            status = case_when(
                earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
                earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
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
data_icc <- stratify_prepare3(data)

remove_liver_disease_before <- function(data) {
    data_liver_disease_before <- data %>%
        filter(
            inflam_liver == "Yes" | alc_liver == "Yes" | cirr_liver == "Yes" | viral_hepatitis == "Yes" |
                nafld == "Yes"
        )
    data <- data %>%
        anti_join(data_liver_disease_before %>% select(id, inflam_liver, alc_liver, cirr_liver, viral_hepatitis, nafld), by = "id")
    return(data)
}
data_sens <- remove_liver_disease_before(data)

data <- data %>%
    mutate(
        liver_disease = if_else(nafld == "Yes" | inflam_liver == "Yes" || alc_liver == "Yes" | cirr_liver == "Yes" | viral_hepatitis == "Yes", "Yes", "No")
    )

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
    data_cancer_icd10_before <- data %>%
        select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
        mutate(
            cancer_icd10_before = if_else(
                rowSums(across(starts_with("p41270var_a"), ~ grepl("C\\d{2}", .x)) &
                            across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
                "Yes",
                "No"
            )
        )
    data <- data %>%
        left_join(data_cancer_icd10_before %>% select(id, cancer_icd10_before), by = "id")
    return(data)
}
data <- cancer_icd10_register(data)

data <- data %>%
    mutate(cancer_before_baseline = if_else(cancer_before == "No" & cancer_icd10_before == "No", "No", "Yes"))
data %>% group_by(cancer_before_baseline) %>% summarise(n=n())

data_no_cancer <- data %>%
    filter(cancer_before_baseline == "No")
