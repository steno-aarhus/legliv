# Alcohol variable (also test for how to code diet variables)

data_alc <- data %>%
    select(contains('alcohol'))

# Calculating mean of all datapoints:
data_alc <- data_alc %>%
    rowwise() %>%
    mutate(alc_mean = mean(c_across(alcohol0:alcohol4), na.rm = T))

data_alc$alc_mean %>%
    summary()

data_alc$alc_mean %>%
    describe()

data_alc %>%
    ggplot(aes(x = alc_mean)) +
    geom_histogram()
