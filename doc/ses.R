# SES variables:

# Education
data_edu <- data %>%
    select(education)
view(data_edu)

# each participants have multiple categories. Recoding to 1 of 3 categories:
data_edu <- data_edu %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "high",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("O levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("CSEs", education, ignore.case = TRUE) ~ "low",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "low",
        grepl("Other professional", education, ignore.case = TRUE) ~ "low",
        grepl("None of the above", education, ignore.case = TRUE) ~ "low",
        TRUE ~ as.character(education)
    ))

data_edu %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

data_edu %>%
    summary

data_edu %>%
    ggplot(aes(x = education2)) +
    geom_bar()

# For liver data

data_edu_liver <- data_liver %>%
    select(education)

data_edu_liver <- data_edu_liver %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "high",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("O levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("CSEs", education, ignore.case = TRUE) ~ "low",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "low",
        grepl("Other professional", education, ignore.case = TRUE) ~ "low",
        grepl("None of the above", education, ignore.case = TRUE) ~ "low",
        TRUE ~ as.character(education)
    ))

data_edu_liver %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

data_edu_liver %>%
    ggplot(aes(x = education2)) +
    geom_bar()


# townsend deprivation index
data_tdi <- data %>%
    select(tdi)

data_tdi %>%
    summary()

data_tdi %>%
    ggplot(aes(x = tdi)) +
    geom_histogram()

data_tdi %>%
    ggplot(aes(sample = tdi)) +
    stat_qq() +
    stat_qq_line()

data_tdi_liver <- data_liver %>%
    select(tdi)

data_tdi_liver %>%
    summary()

data_tdi_liver %>%
    ggplot(aes(x=tdi))+
    geom_histogram(bins = 10)

data_tdi_liver %>%
    ggplot(aes(sample=tdi)) +
    stat_qq()+
    stat_qq_line()

wilcox.test(data_tdi$tdi, data_tdi_liver$tdi ) |>
    parameters()


# Living alone, computed from number in household

data_spouse <- data %>%
    select(spouse)

data_spouse %>%
    count(spouse) %>%
    arrange() %>%
    mutate(percentage = n/sum(n)*100) %>%
    print(n=37)


data_spouse_liver <- data_liver %>%
    select(spouse)

data_spouse_liver %>%
    count(spouse) %>%
    arrange() %>%
    mutate(percentage = n/sum(n)*100) %>%
    print(n=37)
