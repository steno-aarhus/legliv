# Fie kode
icd10_subset <- data %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"),
                 names_to = c(".value", "a"),
                 names_sep = "_")

icd10_hcc <- icd10_subset %>%
    mutate(icd10_hcc_date = ifelse(str_detect(p41270var, "C22.0"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
           icd10_hcc_date = as.Date(icd10_hcc_date, format = "%Y-%m-%d"))

first_non_na_hcc <- icd10_hcc %>%
    filter(!is.na(icd10_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_hcc %>% select(id, icd10_hcc_date), by = "id")

icd10_icc <- icd10_subset %>%
    mutate(icd10_icc_date = ifelse(str_detect(p41270var, "C22.1"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
           icd10_icc_date = as.Date(icd10_icc_date, format = "%Y-%m-%d"))

first_non_na_icc <- icd10_icc %>%
    filter(!is.na(icd10_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_icc %>% select(id, icd10_icc_date), by = "id")


cancer_subset <- data %>%
    select(matches("p40006|p40005|id")) %>%
    pivot_longer(cols = matches("_i[0-9]*$"),
                 names_to = c(".value", "a"),
                 names_sep = "_")

cancer_hcc <- cancer_subset %>%
    mutate(cancer_hcc_date = ifelse(str_detect(p40006, "C22.0"),
                                    as.character(c_across(starts_with("p40005"))),
                                    NA),
           cancer_hcc_date = as.Date(cancer_hcc_date, format = "%Y-%m-%d"))

first_non_na_hcc_c <- cancer_hcc %>%
    filter(!is.na(cancer_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_hcc_c %>% select(id, cancer_hcc_date), by = "id")

cancer_icc <- cancer_subset %>%
    mutate(cancer_icc_date = ifelse(str_detect(p40006, "C22.1"),
                                    as.character(c_across(starts_with("p40005"))),
                                    NA),
           cancer_icc_date = as.Date(cancer_icc_date, format = "%Y-%m-%d"))

first_non_na_icc_c <- cancer_icc %>%
    filter(!is.na(cancer_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_icc_c %>% select(id, cancer_icc_date), by = "id")
