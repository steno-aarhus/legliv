# Table 1

# age

table1 <- data %>%
    select(age_at_baseline)

table1 %>%
    summary()

table1_liv <- data_liver %>%
    select(age_at_baseline)

table1_liv %>%
    summary()

# Sex
table1 <- data %>%
    select(sex)

table1 %>%
    group_by(sex) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)

table1_liv <- data_liver %>%
    select(sex)

table1_liv %>%
    group_by(sex) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)

# Socioeconomic status

