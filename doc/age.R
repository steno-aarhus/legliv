data_age <- data %>%
    select(age_recruit, birth_year)

data_age %>%
    summary()

data_age %>%
    ggplot(aes(x = age_recruit)) +
    geom_histogram(bins = 31)

data_age %>%
    ggplot(aes(sample = age_recruit)) +
    stat_qq()+
    stat_qq_line()

data_age <- data_age %>%
    mutate(log_age = log(age_recruit))

data_age %>%
    ggplot(aes(x = log_age)) +
    geom_histogram(bins = 31)

data_age %>%
    ggplot(aes(sample = log_age)) +
    stat_qq()+
    stat_qq_line()

data_age_liv <- data_liver %>%
    select(age_recruit)

data_age_liv %>%
    summary()


#calculating age at baseline
data_age <- data_age %>%
    mutate(recruit_year = birth_year + age_recruit)

data_age %>%
    group_by(recruit_year) %>%
    summarise()

data %>%
    mutate(sex = as.factor(sex))

data%>%
    summary()
