
# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
    mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, month_names)))

data <- data %>%
    unite(birth, birth_year, month_of_birth_num, sep = "-")

remove(month_names)

data <- data %>%
    select(-month_of_birth)

# adding 15 as DD for all participants:

data$birth <- as.Date(paste0(data$birth, "-15"))

# Removing specific time stamp from date of completed questionnaires:
# (May be irrelevant)

data <- data %>%
    mutate(ques_comp_t0 = substr(ques_comp_t0, 1, 10),
           ques_comp_t1 = substr(ques_comp_t1, 1, 10),
           ques_comp_t2 = substr(ques_comp_t2, 1, 10),
           ques_comp_t3 = substr(ques_comp_t3, 1, 10),
           ques_comp_t4 = substr(ques_comp_t4, 1, 10)
    )

# Creating baseline age:

# Creating age at baseline:
# (Takes a long time to load)
data <- data %>%
   mutate(age_at_baseline = year(baseline_start_date) - year(birth) -
             ifelse(month(baseline_start_date) < month(birth) |
                       (month(baseline_start_date) == month(birth) &
                           day(baseline_start_date) < day(birth)), 1, 0))



# Go back to runfirst to create liver cancer event cohort
