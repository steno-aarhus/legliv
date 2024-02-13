t2e <- data %>%
    select(age_recruit, age_at_baseline, age_l2fu, age_dead, starts_with("cancer_age"), date_birth, baseline_start_date, starts_with("cancer_date"), dead_date, l2fu_d)

