
t2e <- data %>%
    select(id, age_recruit, age_at_baseline, age_l2fu, age_dead, starts_with("age_cancer"), date_birth, baseline_start_date, starts_with("cancer_date"), dead_date, l2fu_d, status, status_date, status_age)

# Creating status, status date and status age
t2e <- t2e %>%
    mutate(
        earliest_date = pmin(dead_date, cancer_date0, cancer_date1, cancer_date2, cancer_date3, l2fu_d, na.rm = TRUE),
        status = case_when(
            earliest_date == cancer_date0 & earliest_date > baseline_start_date ~ "liver cancer",
            earliest_date == cancer_date1 & earliest_date > baseline_start_date ~ "liver cancer",
            earliest_date == cancer_date2 & earliest_date > baseline_start_date ~ "liver cancer",
            earliest_date == cancer_date3 & earliest_date > baseline_start_date ~ "liver cancer",
            earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "lost to follow-up",
            earliest_date == dead_date ~ "dead",
            TRUE ~ "alive"
        ),
        status_date = earliest_date,
        status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25  # Calculating age in years
    ) %>%
    select(-earliest_date)

t2e %>%
    select(status) %>%
    group_by(status) %>%
    count()


# Attempt at plotting:
sample <- data %>%
    sample_n(10000, replace = FALSE)  # Change replace to TRUE if you want sampling with replacement

t2e_plot <- sample %>%
    select(id, age_recruit, age_at_baseline, age_l2fu, age_dead, starts_with("age_cancer"), date_birth, baseline_start_date, starts_with("cancer_date"), dead_date, l2fu_d, status, status_date, status_age)

t2e_plot <- t2e_plot %>%
    arrange(age_at_baseline)

t2e_plot <- t2e_plot %>%
    mutate(id = row_number())

t2e_plot %>%
    ggplot() +
    geom_segment(aes(x = age_at_baseline, y = id, xend = status_age, yend = id, color = status)) +
    geom_point(aes(x = status_age, y = id)) +
    labs(title = "Observations from Baseline Age to Status Age for Liver Cancer, loss to follow-up, death, or survival",
         x = "Age",
         y = "Row ID",
         color = "status") +
    theme_minimal()


