data %>%
    group_by(smoking) %>%
    summarise()

data %>%
    count(smoking) %>%
    mutate(percentage = n/sum(n)*100)

data_liver %>%
    count(smoking) %>%
    mutate(percentage = n/sum(n)*100)

