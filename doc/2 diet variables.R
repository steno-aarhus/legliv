# Diet variables


data <- data %>%
    rowwise() %>%
    mutate(legumes_daily = mean(c_across(legumes0:legumes4), na.rm = T),
           red_meat_daily = mean(c_across(red_meat0:red_meat4), na.rm = T),
           proc_meat_daily = mean(c_across(proc_meat0:proc_meat4), na.rm = T),
           red_proc_daily = mean(c_across(red_proc_meat0:red_proc_meat4), na.rm = T),
           food_weight_daily = mean(c_across(food_weight0:food_weight4), na.rm = T),
           food_energy_mean = mean(c_across(food_energy0:food_energy4), na.rm = T),
           alcohol_mean = mean(c_across(alcohol0:alcohol4), na.rm = T)
    )

data %>%
    select(leg_mean,
           red_meat_mean,
           proc_meat_mean,
           red_proc_mean,
           food_weight_mean,
           food_energy_mean,
           alcohol_mean) %>%
    summary()

#Checking energy intake:
data %>%
    ggplot(aes(x = food_energy_mean)) +
    geom_histogram()

data %>%
    ggplot(aes(sample = food_energy_mean)) +
    stat_qq() +
    stat_qq_line()

# does not look normally distributed. summary shows that median and mean are not equal.
# logtransforming. 2 observation = 0. Removing these:

data <- data %>%
    filter(food_energy_mean != 0) %>%
    mutate(log_energy_mean = log(food_energy_mean))

data %>%
    ggplot(aes(x = log_energy_mean)) +
    geom_histogram()

data %>%
    ggplot(aes(sample = log_energy_mean)) +
    stat_qq() +
    stat_qq_line()

# Some outliers, but look much better.

data %>%
    select(log_energy_mean) %>%
    summary()

exp(c(8.879, 9.040, 9.034, 9.196))
# Median and mean are closer now.

#for liver data

data_liver <- data_liver %>%
    rowwise() %>%
    mutate(leg_mean = mean(c_across(legumes0:legumes4), na.rm = T),
           red_meat_mean = mean(c_across(red_meat0:red_meat4), na.rm = T),
           proc_meat_mean = mean(c_across(proc_meat0:proc_meat4), na.rm = T),
           red_proc_mean = mean(c_across(red_proc_meat0:red_proc_meat4), na.rm = T),
           food_weight_mean = mean(c_across(food_weight0:food_weight4), na.rm = T),
           food_energy_mean = mean(c_across(food_energy0:food_energy4), na.rm = T),
           alcohol_mean = mean(c_across(alcohol0:alcohol4), na.rm = T)
    )

data_liver %>%
    select(leg_mean,
           red_meat_mean,
           proc_meat_mean,
           red_proc_mean,
           food_weight_mean,
           food_energy_mean,
           alcohol_mean) %>%
    summary()

# Energy intake:
data_liver %>%
    ggplot(aes(x = food_energy_mean)) +
    geom_histogram(bins = 6)

data_liver %>%
    ggplot(aes(sample = food_energy_mean)) +
    stat_qq() +
    stat_qq_line()

data_liver <- data_liver %>%
    mutate(log_energy_mean = log(food_energy_mean))

data_liver %>%
    ggplot(aes(x = log_energy_mean)) +
    geom_histogram(bins = 6)

data_liver %>%
    ggplot(aes(sample = log_energy_mean)) +
    stat_qq() +
    stat_qq_line()

data_liver %>%
    select(log_energy_mean) %>%
    summary()

exp(c(8.943, 9.101, 9.089, 9.247))
