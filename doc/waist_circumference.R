# waist cirumference:

data %>%
    ggplot(aes(x = wc)) +
    geom_histogram()

data %>%
    ggplot(aes(sample = wc)) +
    stat_qq() +
    stat_qq_line()

# smiling line, not normal distributed. log-transforming:

data <- data %>%
    mutate(logwc = log(wc))

data %>%
    ggplot(aes(x = logwc)) +
    geom_histogram()

data %>%
    ggplot(aes(sample = logwc)) +
    stat_qq() +
    stat_qq_line()

data %$%
    exp(smean.cl.normal(logwc))

# For liver data:

data_liver %>%
    ggplot(aes(x = wc)) +
    geom_histogram()

data_liver %>%
    ggplot(aes(sample = wc)) +
    stat_qq() +
    stat_qq_line()

# smiling line, not normal distributed. log-transforming:

data_liver <- data_liver %>%
    mutate(logwc = log(wc))

data_liver %>%
    ggplot(aes(x = logwc)) +
    geom_histogram()

data_liver %>%
    ggplot(aes(sample = logwc)) +
    stat_qq() +
    stat_qq_line()

data_liver %$%
    smean.cl.normal(wc)
