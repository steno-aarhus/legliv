#calculating age at baseline (test code, the right code is in 1runfirst.R).

data <- data %>%
    mutate(id = 1:nrow(data))

# Create a new column with the baseline start date
data_baseline <- data %>%
    # Gather questionnaire dates into long format
    pivot_longer(cols = starts_with("ques_comp_t"),
                 names_to = "questionnaire",
                 values_to = "completion_date") %>%
    # Remove rows with NA completion dates
    filter(!is.na(completion_date)) %>%
    # Group by participant_id
    group_by(id) %>%
    # Arrange by completion date within each participant
    arrange(completion_date) %>%
    # Create a lagged column to find the last completed questionnaire
    mutate(last_questionnaire_date = lag(completion_date)) %>%
    # Filter participants who completed >= 2 questionnaires
    filter(!is.na(last_questionnaire_date)) %>%
    # Keep only the last completed questionnaire for each participant
    filter(is.na(lead(completion_date))) %>%
    # Rename the columns to match the desired output
    rename(baseline_start_date = completion_date) %>%
    # Remove unnecessary columns
    select(id, birth, baseline_start_date)

data_baseline$birth <- as.Date(paste0(data_baseline$birth, "-01"))

data_baseline <- data_baseline %>%
    mutate(age_at_baseline = year(baseline_start_date) - year(birth) -
               ifelse(month(baseline_start_date) < month(birth) |
                          (month(baseline_start_date) == month(birth) &
                               day(baseline_start_date) < day(birth)), 1, 0))


data_age <- data %>%
    select(age_at_baseline)

data_age %>%
    summary()

data_age_liv <- data_liver %>%
    select(age_at_baseline)

data_age_liv %>%
    summary()
