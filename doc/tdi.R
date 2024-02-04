data_tdi <- data %>%
    select(tdi)

data_tdi %>%
    ggplot(aes(x = tdi)) +
    geom_histogram()

data_tdi %>%
    ggplot(aes(sample = tdi)) +
    stat_qq() +
    stat_qq_line()

#trying logtransformation

data_tdi <- data_tdi %>%
    mutate(tdi10 = tdi + 10)

data_tdi <- data_tdi %>%
    mutate(logtdi = log(tdi10))

data_tdi %>%
    ggplot(aes(x=logtdi))+
    geom_histogram()

data_tdi %>%
    ggplot(aes(sample=logtdi))+
    stat_qq()+
    stat_qq_line()

# Looks bloody awful

data_tdi %$%
    ci_median(tdi)


data_tdi_liver <- data_liver %>%
    select(tdi)

data_tdi_liver %>%
    ggplot(aes(x=tdi))+
    geom_histogram(bins = 10)

data_tdi_liver %>%
    ggplot(aes(sample=tdi)) +
    stat_qq()+
    stat_qq_line()

data_tdi_liver <- data_tdi_liver %>%
    mutate(tdi10 = tdi + 10,
           logtdi = log(tdi10))

data_tdi_liver %>%
    ggplot(aes(x=logtdi))+
    geom_histogram(bins = 10)

data_tdi_liver %>%
    ggplot(aes(sample=logtdi))+
    stat_qq()+
    stat_qq_line()

data_tdi_liver %$%
    exp(smean.cl.normal(logtdi))

10-7.524956

data_tdi_liver %$%
    ci_median(tdi)

wilcox.test(data_tdi$tdi, data_tdi_liver$tdi ) |>
    parameters()

