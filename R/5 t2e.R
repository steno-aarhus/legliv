t2e <- data %>%
    select(age_recruit, age_at_baseline, age_l2fu, age_dead, starts_with("cancer_age"), date_birth, baseline_start_date, starts_with("cancer_date"), dead_date, l2fu_d)


t2e <- t2e %>%
    mutate(
        baseline_start_date = as.Date(baseline_start_date),
        dead_date = as.Date(dead_date),
        cancer_date0 = as.Date(cancer_date0),
        cancer_date1 = as.Date(cancer_date1),
        cancer_date2 = as.Date(cancer_date2),
        cancer_date3 = as.Date(cancer_date3),
        l2fu_d = as.Date(l2fu_d),
        status = case_when(
            dead_date == baseline_start_date + days(1) ~ "dead",
            cancer_date0 == baseline_start_date + days(1) |
                cancer_date1 == baseline_start_date + days(1) |
                cancer_date2 == baseline_start_date + days(1) |
                cancer_date3 == baseline_start_date + days(1) ~ "liver cancer",
            l2fu_d == baseline_start_date + days(1) ~ "lost to follow-up",
            TRUE ~ "normal"
        )
    )




t2e <- t2e %>%
    mutate(
        status = case_when(
            dead_date > baseline_start_date &
                (is.na(l2fu_d) | dead_date < l2fu_d) &
                (is.na(cancer_date0) | dead_date < cancer_date0) &
                (is.na(cancer_date1) | dead_date < cancer_date1) &
                (is.na(cancer_date2) | dead_date < cancer_date2) &
                (is.na(cancer_date3) | dead_date < cancer_date3) ~ "dead",
            !is.na(cancer_date0) & cancer_date0 > baseline_start_date &
                (is.na(l2fu_d) | cancer_date0 < l2fu_d) &
                (is.na(dead_date) | cancer_date0 < dead_date) &
                (is.na(cancer_date1) | cancer_date0 < cancer_date1) &
                (is.na(cancer_date2) | cancer_date0 < cancer_date2) &
                (is.na(cancer_date3) | cancer_date0 < cancer_date3) ~ "liver cancer",
            !is.na(l2fu_d) & l2fu_d > baseline_start_date &
                (is.na(dead_date) | l2fu_d < dead_date) &
                (is.na(cancer_date0) | l2fu_d < cancer_date0) &
                (is.na(cancer_date1) | l2fu_d < cancer_date1) &
                (is.na(cancer_date2) | l2fu_d < cancer_date2) &
                (is.na(cancer_date3) | l2fu_d < cancer_date3) ~ "lost to follow-up",
            TRUE ~ "normal"
        )
    )

t2e %>%
    select(status) %>%
    group_by(status) %>%
    count()
