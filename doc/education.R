# Education
data_edu <- data %>%
    select(education)

data_edu <- data_edu %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "1 College or University degree",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "2 A levels/AS levels or equivalent",
        grepl("O levels", education, ignore.case = TRUE) ~ "3 O levels/GSCEs or equivalent",
        grepl("CSEs", education, ignore.case = TRUE) ~ "4 CSEs or equivalent",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "5 NVQ or HND or HNC or equivalent",
        grepl("Other professional", education, ignore.case = TRUE) ~ "6 Other professional qualifications eg: nursing, teaching",
        TRUE ~ as.character(education)
    ))

data_edu %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

data_edu %>%
    ggplot(aes(x = education2)) +
    geom_bar()


# For liver data
data_edu_liver <- data_liver %>%
    select(education)

data_edu_liver <- data_edu_liver %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "1 College or University degree",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "2 A levels/AS levels or equivalent",
        grepl("O levels", education, ignore.case = TRUE) ~ "3 O levels/GSCEs or equivalent",
        grepl("CSEs", education, ignore.case = TRUE) ~ "4 CSEs or equivalent",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "5 NVQ or HND or HNC or equivalent",
        grepl("Other professional", education, ignore.case = TRUE) ~ "6 Other professional qualifications eg: nursing, teaching",
        TRUE ~ as.character(education)
    ))

data_edu_liver %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

data_edu_liver %>%
    ggplot(aes(x = education2)) +
    geom_bar()
